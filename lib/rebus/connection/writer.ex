defmodule Rebus.Connection.Writer do
  @moduledoc false

  # The outbound half of a D-Bus connection: the FIFO queue of frames waiting
  # for the socket, the single frame currently being written, and the serial
  # counter that numbers them. Writes are one frame at a time, so a reply can
  # never overtake or starve caller traffic, and a partially written frame is
  # always finished before the next one starts.
  #
  # Unlike `Rebus.Connection.Inbound` this is not a pure buffer: it owns the
  # write-timeout timer, answers its callers with `GenServer.reply/2` and calls
  # the transport itself. What it does not own is the reply-correlation table:
  # a completed `:call` write is handed back to the connection, which registers
  # the pending entry and then asks for the next write. Everything the writer
  # needs from the connection arrives per call in a `t:context/0`, so nothing
  # here reaches into the connection struct.

  use TypedStruct

  alias Rebus.Connection.Pending
  alias Rebus.Connection.SocketError
  alias Rebus.Message

  require Logger

  @max_serial 4_294_967_295

  # A peer that floods method calls without reading its socket would otherwise
  # grow the write queue without bound: replies are produced per inbound frame
  # but drain only as fast as the transport accepts them. Beyond this many
  # queued connection-originated replies, further calls go unanswered, exactly
  # as if their reply had expired before it could be written.
  @max_queued_replies 64

  # `sendmsg` errors that describe the descriptors being passed rather than the
  # stream itself.
  @fd_send_errors [:ebadf, :einval, :eperm, :emfile, :enfile]

  defmodule Active do
    @moduledoc false

    use TypedStruct

    # The one frame currently being written: the operation it came from, the
    # serial it was encoded with, the bytes the socket has yet to accept, and
    # the state of the descriptor-passing handshake.

    typedstruct enforce: true do
      field :kind, :call | :send | :reply
      field :from, GenServer.from() | nil
      field :msg, Message.t()
      field :deadline, integer()
      field :request_ref, reference()
      field :monitor_ref, reference() | nil
      field :serial, pos_integer()
      field :rest, binary()
      # `nil` when the socket is free to be written, `{:continue, cont}` when a
      # registered continuation is ready to resume, and `{:select, cont, handle}`
      # while waiting on a select notification.
      field :wait, nil | {:continue, tuple()} | {:select, tuple(), reference()}
      field :timer_ref, reference()
      field :partial?, boolean()
      field :unix_fds, [Rebus.UnixFD.t()]
      field :uses_sendmsg?, boolean()
      # `:socket.sendmsg/4` retains the original encoded control map in a select
      # continuation. We keep this explicit so that only a no-progress select
      # uses that continuation; once bytes have been accepted, the remaining
      # stream bytes use plain send/4.
      field :fd_control, :none | :initial | :select_continuation | :accepted
    end
  end

  typedstruct enforce: true do
    field :queue, :queue.queue(), default: :queue.new()
    field :active, Active.t() | nil, default: nil
    # Request references, spelled `%MapSet{}` rather than `MapSet.t(reference())`
    # because an empty set does not inhabit that opaque type and every spec
    # naming `t/0` would then be reported as violating it.
    field :queued_refs, %MapSet{}, default: MapSet.new()
    field :cancelled_refs, %MapSet{}, default: MapSet.new()
    field :monitor_index, %{reference() => reference()}, default: %{}
    # Connection-originated replies waiting behind the active write, and
    # whether the cap was hit since the queue last drained below it (so the
    # refusal is logged once per saturation episode, not once per call).
    field :replies, non_neg_integer(), default: 0
    field :saturated?, boolean(), default: false
    # The next serial to allocate.
    field :serial, pos_integer(), default: 1
  end

  @typedoc """
  A frame handed to `queue/2` for writing. `from` is `nil` for
  connection-originated replies, which have no caller to answer, monitor or
  cancel.
  """
  @type operation :: %{
          required(:kind) => :call | :send | :reply,
          required(:from) => GenServer.from() | nil,
          required(:msg) => Message.t(),
          required(:deadline) => integer(),
          required(:request_ref) => reference()
        }

  @typedoc """
  An operation once it is on the queue. `queue/2` attaches the caller monitor
  (`nil` when there is no caller), so a dequeued frame always carries one.
  """
  @type queued_operation :: %{
          required(:kind) => :call | :send | :reply,
          required(:from) => GenServer.from() | nil,
          required(:msg) => Message.t(),
          required(:deadline) => integer(),
          required(:request_ref) => reference(),
          required(:monitor_ref) => reference() | nil
        }

  @typedoc """
  Everything the writer borrows from the connection for the duration of one
  call: the socket and the modules behind its side effects, the write timeout,
  the serials currently spoken for, and the connection's outbound message
  validation.
  """
  @type context :: %{
          required(:sock) => :socket.socket(),
          required(:transport) => module(),
          required(:hooks) => module(),
          required(:write_timeout) => pos_integer(),
          required(:pending) => %{non_neg_integer() => Pending.Entry.t()},
          required(:validate) => (Message.t() -> :ok | {:error, term()})
        }

  @typedoc """
  What the connection must do next.

  `:ok` means the writer is idle or parked on a select notification, `:continue`
  that it has more to write and wants the `:write` continuation, `:stop` that
  the transport failed fatally, and `:call_written` that a `:call` frame reached
  the peer: the connection registers the pending entry it carries and calls
  `advance/2` again.
  """
  @type result ::
          {:ok, t()}
          | {:continue, t()}
          | {:call_written, Pending.Entry.t(), t()}
          | {:stop, term(), t()}

  # `:queue.new/0` is called rather than left to the field default so the queue
  # keeps its opaque type instead of the literal a compile-time default bakes in.
  @spec new() :: t()
  def new, do: %__MODULE__{queue: :queue.new()}

  @doc false
  @spec serial(t()) :: pos_integer()
  def serial(%__MODULE__{serial: serial}), do: serial

  # The handshake writes Hello itself, ahead of the queue, and so consumes a
  # serial without going through a write operation.
  @doc false
  @spec consume_serial(t()) :: t()
  def consume_serial(%__MODULE__{serial: serial} = writer),
    do: %{writer | serial: next_serial(serial)}

  @doc false
  @spec active(t()) :: Active.t() | nil
  def active(%__MODULE__{active: active}), do: active

  # Connection-originated frames (`kind: :reply`) have no caller: no `from` to
  # reply to, no monitor to release, and no cancellation. They share the FIFO
  # write queue so a reply can never overtake or starve caller traffic.
  @spec queue(t(), operation()) :: t()
  def queue(%__MODULE__{} = writer, operation) do
    monitor_ref = monitor_operation(operation)
    operation = Map.put(operation, :monitor_ref, monitor_ref)

    %{
      writer
      | queue: :queue.in(operation, writer.queue),
        queued_refs: MapSet.put(writer.queued_refs, operation.request_ref),
        monitor_index: index_monitor(writer.monitor_index, monitor_ref, operation.request_ref),
        replies: reserve_reply_slot(writer.replies, operation)
    }
  end

  # Writes are one-frame-at-a-time. OTP retains the unaccepted RestData in every
  # partial result; retaining it here is what preserves D-Bus stream framing.
  @spec enqueue(t(), operation(), context()) :: result()
  def enqueue(%__MODULE__{} = writer, operation, ctx), do: advance(queue(writer, operation), ctx)

  @spec replies_saturated?(t()) :: boolean()
  def replies_saturated?(%__MODULE__{replies: replies}), do: replies >= @max_queued_replies

  # A peer flooding calls into a stalled transport must not also flood the
  # log: warn when the cap is first hit, then stay quiet until the queue has
  # drained below it again.
  @spec refuse_reply(t()) :: t()
  def refuse_reply(%__MODULE__{saturated?: true} = writer), do: writer

  def refuse_reply(%__MODULE__{} = writer) do
    Logger.warning("D-Bus internal reply dropped: :reply_queue_full", reason: :reply_queue_full)
    %{writer | saturated?: true}
  end

  @spec advance(t(), context()) :: result()
  def advance(%__MODULE__{active: nil} = writer, ctx) do
    case :queue.out(writer.queue) do
      {:empty, _queue} ->
        {:ok, writer}

      {{:value, operation}, queue} ->
        writer = %{
          release_reply_slot(writer, operation)
          | queue: queue,
            queued_refs: MapSet.delete(writer.queued_refs, operation.request_ref)
        }

        if cancelled_or_expired?(operation, writer) do
          writer = release_monitor(writer, operation)

          advance(
            %{
              writer
              | cancelled_refs: MapSet.delete(writer.cancelled_refs, operation.request_ref)
            },
            ctx
          )
        else
          start_write(writer, operation, ctx)
        end
    end
  end

  def advance(%__MODULE__{active: %Active{wait: {:select, _cont, _handle}}} = writer, _ctx),
    do: {:ok, writer}

  def advance(%__MODULE__{active: write} = writer, ctx) do
    if (expired?(write) or cancelled?(write, writer)) and not write.partial? do
      advance(drop_active(writer, ctx, cancel?: true), ctx)
    else
      result = safe_socket_send(ctx, write)
      handle_write_result(result, %{writer | active: %{write | wait: nil}}, ctx)
    end
  end

  # A writable socket notification for the active write. The registered
  # continuation is what makes the next attempt resume rather than restart.
  @spec resume_select(t(), tuple(), context()) :: result()
  def resume_select(%__MODULE__{active: write} = writer, continuation, ctx),
    do: advance(%{writer | active: %{write | wait: {:continue, continuation}}}, ctx)

  # The write-timeout timer of the active write fired. Only the caller of the
  # matching operation is affected while nothing has entered the stream.
  @spec write_timeout(t(), context()) :: result()
  def write_timeout(%__MODULE__{active: write} = writer, ctx) do
    if write.partial? do
      {:stop, :timeout, writer}
    else
      # No bytes have entered the stream, so this frame can be safely abandoned.
      reply_if_live(write, {:error, :timeout}, writer)
      advance(drop_active(writer, ctx, cancel?: true), ctx)
    end
  end

  @spec cancel(t(), reference(), context()) :: result()
  def cancel(
        %__MODULE__{active: %Active{request_ref: request_ref, partial?: false}} = writer,
        request_ref,
        ctx
      ),
      do: advance(drop_active(writer, ctx, cancel?: true), ctx)

  def cancel(%__MODULE__{active: %Active{request_ref: request_ref}} = writer, request_ref, _ctx),
    do: {:ok, mark_cancelled(writer, request_ref)}

  def cancel(%__MODULE__{} = writer, request_ref, _ctx) do
    if MapSet.member?(writer.queued_refs, request_ref),
      do: {:ok, mark_cancelled(writer, request_ref)},
      else: {:ok, writer}
  end

  # The caller of an outbound operation went away. Its monitor has already been
  # taken out of the index by `pop_monitor/2`, so the request is known to be
  # queued or active and needs no membership test.
  @spec cancel_monitored(t(), reference(), context()) :: result()
  def cancel_monitored(
        %__MODULE__{active: %Active{request_ref: request_ref, partial?: false}} = writer,
        request_ref,
        ctx
      ),
      do: advance(drop_active(writer, ctx, cancel?: true), ctx)

  def cancel_monitored(%__MODULE__{} = writer, request_ref, _ctx),
    do: {:ok, mark_cancelled(writer, request_ref)}

  @spec pop_monitor(t(), reference()) :: {reference(), t()} | :error
  def pop_monitor(%__MODULE__{} = writer, monitor_ref) do
    case Map.pop(writer.monitor_index, monitor_ref) do
      {nil, _index} -> :error
      {request_ref, index} -> {request_ref, %{writer | monitor_index: index}}
    end
  end

  # Teardown. Every caller still waiting on a queued or active frame learns the
  # connection is gone; a queued connection-originated reply is simply
  # discarded. The serial counter is deliberately preserved.
  @spec abandon_all(t()) :: t()
  def abandon_all(%__MODULE__{} = writer) do
    case writer.active do
      nil ->
        :ok

      write ->
        _ = Process.cancel_timer(write.timer_ref)
        abandon_operation(write)
    end

    writer.queue |> :queue.to_list() |> Enum.each(&abandon_operation/1)

    %{
      writer
      | queue: :queue.new(),
        active: nil,
        queued_refs: MapSet.new(),
        cancelled_refs: MapSet.new(),
        monitor_index: %{},
        replies: 0,
        saturated?: false
    }
  end

  @spec start_write(t(), queued_operation(), context()) :: result()
  defp start_write(writer, operation, ctx) do
    with :ok <- ctx.validate.(operation.msg),
         {:ok, serial} <- allocate_serial(writer.serial, ctx.pending),
         {:ok, bin} <- encode_message(%{operation.msg | serial: serial}) do
      bin = IO.iodata_to_binary(bin)

      timer_ref =
        Process.send_after(self(), {:write_timeout, operation.request_ref}, ctx.write_timeout)

      write = %Active{
        kind: operation.kind,
        from: operation.from,
        msg: operation.msg,
        deadline: operation.deadline,
        request_ref: operation.request_ref,
        monitor_ref: operation.monitor_ref,
        serial: serial,
        rest: bin,
        wait: nil,
        timer_ref: timer_ref,
        partial?: false,
        unix_fds: operation.msg.unix_fds,
        uses_sendmsg?: operation.msg.unix_fds != [],
        fd_control: if(operation.msg.unix_fds == [], do: :none, else: :initial)
      }

      advance(%{writer | active: write}, ctx)
    else
      {:error, reason} -> advance(fail_operation(writer, operation, reason), ctx)
    end
  end

  # A connection-originated reply has no caller to inform. Failing to encode,
  # serialize or transport one is a defect in this library rather than a caller
  # error, so it is logged and the frame is dropped; the connection continues.
  defp fail_operation(writer, %{kind: :reply} = operation, reason) do
    Logger.warning("D-Bus internal reply dropped: #{inspect(reason)}", reason: reason)
    release_monitor(writer, operation)
  end

  defp fail_operation(writer, operation, reason) do
    writer = release_monitor(writer, operation)
    GenServer.reply(operation.from, {:error, reason})
    writer
  end

  defp handle_write_result(result, %__MODULE__{active: write} = writer, ctx) do
    case classify_write_result(result, write) do
      :ok ->
        complete_active_write(writer, ctx)

      {:continue, rest} ->
        {:continue, put_active_rest(writer, rest)}

      {:select, continuation, rest} ->
        handle_write_select(writer, ctx, write, continuation, rest)

      {:error, {:send_fatal, reason}} ->
        {:stop, reason, writer}

      {:error, reason} ->
        if write.partial? do
          {:stop, reason, writer}
        else
          reply_if_live(write, {:error, reason}, writer)
          advance(drop_active(writer, ctx, cancel?: true), ctx)
        end
    end
  end

  defp handle_write_select(writer, ctx, write, continuation, rest) do
    partial_with_rights? = fd_control_accepted?(write, rest)
    writer = if rest, do: put_active_rest(writer, rest), else: writer
    {:select_info, _operation, handle} = continuation

    if partial_with_rights? do
      # OTP's Cont keeps the original encoded Msg (including ctrl); using
      # it after a byte was sent could emit SCM_RIGHTS again. Cancel the
      # pending select and let plain send/4 register its own continuation.
      cancel_socket_write(ctx, {:select, continuation, handle})
      {:continue, writer}
    else
      # `:accepted` is sticky. A positive-progress sendmsg has already
      # transferred SCM_RIGHTS and its tail is now a plain send/4
      # operation. A later plain-send select must never turn it back into
      # a sendmsg continuation (whose OTP continuation still owns ctrl).
      writer = retain_sendmsg_continuation(writer, write)

      {:ok, %{writer | active: %{writer.active | wait: {:select, continuation, handle}}}}
    end
  end

  defp retain_sendmsg_continuation(%__MODULE__{} = writer, write) do
    if write.uses_sendmsg? and writer.active.fd_control in [:initial, :select_continuation],
      do: %{writer | active: %{writer.active | fd_control: :select_continuation}},
      else: writer
  end

  defp classify_write_result(result, %Active{uses_sendmsg?: true, fd_control: control, rest: rest})
       when control in [:initial, :select_continuation],
       do: classify_sendmsg_result(result, byte_size(rest))

  defp classify_write_result(result, %Active{rest: rest}),
    do: classify_send_result(result, byte_size(rest))

  defp put_active_rest(%__MODULE__{active: write} = writer, rest) do
    partial? = write.partial? or byte_size(rest) < byte_size(write.rest)
    fd_control = if fd_control_accepted?(write, rest), do: :accepted, else: write.fd_control

    %{writer | active: %{write | rest: rest, partial?: partial?, fd_control: fd_control}}
  end

  defp fd_control_accepted?(
         %Active{uses_sendmsg?: true, fd_control: control, rest: previous},
         rest
       )
       when control in [:initial, :select_continuation] and is_binary(rest) do
    byte_size(rest) < byte_size(previous)
  end

  defp fd_control_accepted?(_write, _rest), do: false

  defp complete_active_write(%__MODULE__{active: write} = writer, ctx) do
    live? = not cancelled_or_expired?(write, writer)
    writer = drop_active(writer, ctx, retain_monitor?: live? and write.kind == :call)
    writer = %{writer | serial: next_serial(write.serial)}

    if live? do
      case write.kind do
        :reply ->
          advance(writer, ctx)

        :send ->
          GenServer.reply(write.from, :ok)
          advance(writer, ctx)

        :call ->
          register_call(writer, write, ctx)
      end
    else
      advance(writer, ctx)
    end
  end

  # The frame is on the wire, so its reply is now the connection's business:
  # the request timer is started here and the correlation entry is handed back.
  defp register_call(writer, write, ctx) do
    case remaining_timeout(write.deadline) do
      {:ok, remaining} ->
        timer_ref =
          Process.send_after(
            self(),
            {:request_timeout, write.serial, write.request_ref},
            remaining + ctx.hooks.request_timeout_slack()
          )

        writer = %{writer | monitor_index: Map.delete(writer.monitor_index, write.monitor_ref)}

        entry = %Pending.Entry{
          serial: write.serial,
          from: write.from,
          timer_ref: timer_ref,
          request_ref: write.request_ref,
          monitor_ref: write.monitor_ref,
          deadline: write.deadline
        }

        {:call_written, entry, writer}

      {:error, :timeout} ->
        advance(release_monitor(writer, write), ctx)
    end
  end

  defp drop_active(%__MODULE__{active: write} = writer, ctx, opts) do
    _ = Process.cancel_timer(write.timer_ref)

    if Keyword.get(opts, :cancel?, false), do: cancel_socket_write(ctx, write.wait)

    writer = %{
      writer
      | active: nil,
        cancelled_refs: MapSet.delete(writer.cancelled_refs, write.request_ref)
    }

    if Keyword.get(opts, :retain_monitor?, false),
      do: writer,
      else: release_monitor(writer, write)
  end

  defp mark_cancelled(%__MODULE__{} = writer, request_ref),
    do: %{writer | cancelled_refs: MapSet.put(writer.cancelled_refs, request_ref)}

  defp monitor_operation(%{from: nil}), do: nil
  defp monitor_operation(%{from: from}), do: Process.monitor(elem(from, 0))

  defp index_monitor(index, nil, _request_ref), do: index

  defp index_monitor(index, monitor_ref, request_ref),
    do: Map.put(index, monitor_ref, request_ref)

  defp release_monitor(writer, %{monitor_ref: nil}), do: writer

  defp release_monitor(writer, operation) do
    Process.demonitor(operation.monitor_ref, [:flush])
    %{writer | monitor_index: Map.delete(writer.monitor_index, operation.monitor_ref)}
  end

  defp reserve_reply_slot(replies, %{kind: :reply}), do: replies + 1
  defp reserve_reply_slot(replies, _operation), do: replies

  defp release_reply_slot(%__MODULE__{} = writer, %{kind: :reply}),
    do: %{writer | replies: writer.replies - 1, saturated?: false}

  defp release_reply_slot(%__MODULE__{} = writer, _operation), do: writer

  defp cancelled_or_expired?(operation, writer),
    do: cancelled?(operation, writer) or expired?(operation)

  defp cancelled?(operation, writer),
    do: MapSet.member?(writer.cancelled_refs, operation.request_ref)

  defp expired?(operation), do: match?({:error, :timeout}, remaining_timeout(operation.deadline))

  defp remaining_timeout(deadline) when is_integer(deadline) do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, remaining}
      _ -> {:error, :timeout}
    end
  end

  defp reply_if_live(%{from: nil}, _reply, _writer), do: :ok

  defp reply_if_live(operation, reply, writer) do
    if not cancelled_or_expired?(operation, writer), do: GenServer.reply(operation.from, reply)
  end

  # A queued connection-originated reply is simply discarded on teardown.
  defp abandon_operation(%{from: nil}), do: :ok

  defp abandon_operation(operation) do
    Process.demonitor(operation.monitor_ref, [:flush])
    GenServer.reply(operation.from, {:error, :disconnected})
  end

  defguardp is_select_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :select_info and
                   elem(info, 1) in [:send, :sendmsg] and is_reference(elem(info, 2))

  defguardp is_sendmsg_select_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :select_info and
                   elem(info, 1) == :sendmsg and is_reference(elem(info, 2))

  @doc false
  @spec classify_send_result(term(), non_neg_integer()) ::
          :ok
          | {:continue, iodata()}
          | {:select, tuple(), binary() | nil}
          | {:error, term()}
  def classify_send_result(:ok, _payload_length), do: :ok

  def classify_send_result({:ok, rest}, _payload_length) when is_binary(rest),
    do: {:continue, rest}

  def classify_send_result({:select, {select_info, rest}}, _payload_length)
      when is_select_info(select_info) and is_binary(rest),
      do: {:select, select_info, rest}

  def classify_send_result({:select, select_info}, _payload_length)
      when is_select_info(select_info),
      do: {:select, select_info, nil}

  def classify_send_result({:error, {:timeout, rest}}, payload_length) do
    if SocketError.iolist?(rest) and IO.iodata_length(rest) == payload_length,
      do: {:error, :timeout},
      else: {:error, {:send_fatal, :timeout}}
  end

  def classify_send_result({:error, {reason, _rest}}, _payload_length) when is_atom(reason),
    do: {:error, {:send_fatal, reason}}

  def classify_send_result({:error, reason}, _payload_length) when is_atom(reason),
    do: {:error, {:send_fatal, reason}}

  def classify_send_result({:error, _reason}, _payload_length),
    do: {:error, {:send_fatal, :send_failed}}

  def classify_send_result(_result, _payload_length), do: {:error, {:send_fatal, :send_failed}}

  defp classify_sendmsg_result({:ok, rest}, _payload_length) do
    case send_rest_binary(rest) do
      {:ok, rest} -> {:continue, rest}
      _ -> {:error, {:send_fatal, :send_failed}}
    end
  end

  defp classify_sendmsg_result({:select, {select_info, rest}}, _payload_length)
       when is_sendmsg_select_info(select_info) do
    case send_rest_binary(rest) do
      {:ok, rest} -> {:select, select_info, rest}
      _ -> {:error, {:send_fatal, :send_failed}}
    end
  end

  defp classify_sendmsg_result({:select, select_info}, _payload_length)
       when is_sendmsg_select_info(select_info),
       do: {:select, select_info, nil}

  defp classify_sendmsg_result({:select, _unexpected}, _payload_length),
    do: {:error, {:send_fatal, :send_failed}}

  # A descriptor-local failure before this attempt accepted bytes is not a
  # stream failure. The queued caller receives a bounded error and the
  # connection can continue with independent calls.
  defp classify_sendmsg_result({:error, reason}, _payload_length)
       when reason in @fd_send_errors,
       do: {:error, :unix_fd_send_failed}

  defp classify_sendmsg_result({:error, {reason, rest}}, payload_length)
       when reason in @fd_send_errors do
    if SocketError.iolist?(rest) and IO.iodata_length(rest) == payload_length,
      do: {:error, :unix_fd_send_failed},
      else: {:error, {:send_fatal, reason}}
  end

  defp classify_sendmsg_result(other, payload_length),
    do: classify_send_result(other, payload_length)

  defp send_rest_binary(rest) do
    {:ok, IO.iodata_to_binary(rest)}
  rescue
    ArgumentError -> :error
  end

  @doc false
  @spec socket_send_args(binary(), nil | {:continue, tuple()}) ::
          {binary(), [] | tuple(), :nowait}
  def socket_send_args(rest, {:continue, continuation}), do: {rest, continuation, :nowait}
  def socket_send_args(rest, _wait), do: {rest, [], :nowait}

  # `socket.erl` in OTP 26--28 stores the encoded original Msg in a sendmsg
  # select continuation (prim_socket:sendmsg/4's Cont is `{Msg, EMsg, EFlags}`).
  # Therefore an IOV-only continuation is correct only when no byte has been
  # accepted. After partial progress we cancel that continuation and send the
  # tail without ctrl, which guarantees SCM_RIGHTS is emitted once.
  defp socket_send(ctx, %Active{uses_sendmsg?: true, fd_control: :initial, wait: nil} = write) do
    ctx.transport.sendmsg(
      ctx.sock,
      %{
        iov: [write.rest],
        ctrl: [%{level: :socket, type: :rights, data: rights_data(write.unix_fds)}]
      },
      [],
      :nowait
    )
  end

  defp socket_send(
         ctx,
         %Active{
           uses_sendmsg?: true,
           fd_control: :select_continuation,
           wait: {:continue, continuation}
         } = write
       ) do
    ctx.transport.sendmsg(ctx.sock, [write.rest], continuation, :nowait)
  end

  defp socket_send(ctx, write) do
    {rest, flags_or_cont, timeout} = socket_send_args(write.rest, write.wait)
    ctx.transport.send(ctx.sock, rest, flags_or_cont, timeout)
  end

  # Socket wrappers are injectable for deterministic state-machine coverage.
  # Never let a malformed result or an injected exception crash the GenServer:
  # that would make OTP log the active frame and its control state.
  defp safe_socket_send(ctx, write) do
    socket_send(ctx, write)
  rescue
    _exception -> {:error, :send_failed}
  catch
    _, _ -> {:error, :send_failed}
  end

  defp rights_data(fds) do
    for fd <- fds, into: <<>>, do: <<fd::native-signed-32>>
  end

  defp cancel_socket_write(ctx, {:select, continuation, _handle}) do
    _ = ctx.transport.cancel(ctx.sock, continuation)
    :ok
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  defp cancel_socket_write(_ctx, _wait), do: :ok

  defp encode_message(%Message{} = msg) do
    case Message.encode(msg) do
      {:ok, bin} ->
        {:ok, bin}

      {:error, reason}
      when reason in [
             :invalid_body,
             :invalid_header_fields,
             :invalid_message,
             :message_too_large
           ] ->
        Logger.warning("D-Bus message encoding failed: #{inspect(reason)}", reason: reason)
        {:error, :encode_failed}

      {:error, _reason} ->
        Logger.warning("D-Bus message encoding failed: :invalid_message",
          reason: :invalid_message
        )

        {:error, :encode_failed}
    end
  rescue
    exception ->
      Logger.warning("D-Bus message encoding failed: #{inspect(exception.__struct__)}",
        reason: exception.__struct__
      )

      {:error, :encode_failed}
  catch
    kind, _reason ->
      Logger.warning("D-Bus message encoding failed: #{inspect(kind)}", reason: kind)
      {:error, :encode_failed}
  end

  defp allocate_serial(serial, pending), do: allocate_serial(serial, pending, @max_serial)

  @doc false
  @spec allocate_serial(non_neg_integer(), map(), pos_integer()) ::
          {:ok, pos_integer()} | {:error, :serial_exhausted}
  def allocate_serial(serial, pending, max_serial)
      when is_integer(serial) and is_map(pending) and is_integer(max_serial) and max_serial > 0 do
    allocate_serial(serial, pending, max_serial, max_serial)
  end

  defp allocate_serial(_serial, _pending, _max_serial, 0), do: {:error, :serial_exhausted}

  defp allocate_serial(serial, pending, max_serial, attempts) do
    if Map.has_key?(pending, serial) do
      allocate_serial(next_serial(serial, max_serial), pending, max_serial, attempts - 1)
    else
      {:ok, serial}
    end
  end

  defp next_serial(@max_serial), do: 1
  defp next_serial(serial), do: serial + 1
  defp next_serial(max_serial, max_serial), do: 1
  defp next_serial(serial, _max_serial), do: serial + 1
end
