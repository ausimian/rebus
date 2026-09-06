defmodule Rebus.Connection.Dispatch do
  @moduledoc false

  # The inbound half of an established D-Bus connection: reading the socket,
  # feeding `Rebus.Connection.Inbound`, and deciding what each complete frame
  # means. Replies are correlated against the pending table and answered (or
  # turned into an FD claim), signals are matched against this connection's
  # handlers, and method calls are handed to `Rebus.Connection.Peer`, which
  # answers them because Rebus exposes no service-side API.
  #
  # It works on the connection struct directly — reply correlation writes the
  # pending table — but it is not a `GenServer`. Every entry point answers with
  # a `t:result/0`, and the connection maps that to a callback return in a
  # single place, so the framing recursion never has to know which callback it
  # was entered from.

  alias Rebus.Connection
  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Peer
  alias Rebus.Connection.Pending
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Setup
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.UnixFD

  require Logger

  @max_read_chunk 65_536
  @max_unix_fd_control_size 256

  @typedoc """
  What the connection must do next.

  `:ok` means the callback returns without a continuation, `:continue` that it
  returns the continuation carried here, and the three terminal values that it
  stops: `:shutdown` verbatim, `:protocol_error` and `:transport_error` after
  the connection has sanitised, logged and failed everything outstanding.
  """
  @type result ::
          {:ok, Connection.t()}
          | {:continue, term(), Connection.t()}
          | {:protocol_error, term(), Connection.t()}
          | {:transport_error, term(), Connection.t()}
          | {:shutdown, term(), Connection.t()}

  @doc false
  @spec max_read_chunk() :: pos_integer()
  def max_read_chunk, do: @max_read_chunk

  @doc false
  @spec max_unix_fd_control_size() :: pos_integer()
  def max_unix_fd_control_size, do: @max_unix_fd_control_size

  @doc false
  @spec recv(Connection.t()) :: result()
  def recv(%Connection{} = state) do
    cond do
      state.unix_fd_negotiated? ->
        transport(state).recvmsg(
          state.sock,
          Inbound.receive_size(state.inbound, @max_read_chunk),
          @max_unix_fd_control_size,
          [],
          :nowait
        )
        |> receive_result(state)

      # OTP documents CtrlSz=0 as its default control-buffer size, not as a
      # request to discard ancillary data. Keep the normal coalescing byte
      # path, but receive a bounded cmsg and close any illicit rights before a
      # partial frame can retain them.
      state.unix_fd_transport? ->
        transport(state).recvmsg(state.sock, 0, @max_unix_fd_control_size, [], :nowait)
        |> receive_result(state)

      true ->
        receive_result(transport(state).recv(state.sock, 0, [], :nowait), state)
    end
  end

  @doc false
  @spec receive_result(term(), Connection.t()) :: result()
  def receive_result({:ok, data}, %Connection{} = state) when is_binary(data) do
    append_inbound(data, state, :recv)
  end

  def receive_result({:ok, message}, %Connection{} = state) when is_map(message) do
    append_recvmsg(message, state, :recv)
  end

  def receive_result({:select, {:select_info, op, handle}}, %Connection{} = state)
      when op in [:recv, :recvmsg] do
    {:ok, %{state | rref: handle}}
  end

  def receive_result({:select, {{:select_info, :recv, handle}, data}}, %Connection{} = state)
      when is_binary(data) do
    append_inbound(data, %{state | rref: handle}, :recv)
  end

  def receive_result(
        {:select, {{:select_info, :recvmsg, handle}, message}},
        %Connection{} = state
      )
      when is_map(message) do
    append_recvmsg(message, %{state | rref: handle}, :recv)
  end

  def receive_result({:error, reason}, %Connection{} = state) do
    {:transport_error, reason, state}
  end

  def receive_result(_result, %Connection{} = state) do
    {:transport_error, :receive_failed, state}
  end

  # Each zero-length receive returns data already available through the fixed
  # OTP buffer. Fixed-header validation still happens as soon as 16 bytes are
  # retained, without making allocation depend on a peer-declared frame length.
  @doc false
  @spec append_inbound(binary(), Connection.t(), term()) :: result()
  def append_inbound(data, %Connection{} = state, continuation) do
    case Inbound.append(state.inbound, data) do
      {:ok, inbound} ->
        process_inbound(%{state | inbound: inbound}, continuation)

      {:error, reason} ->
        {:protocol_error, reason, state}
    end
  end

  @doc false
  @spec append_recvmsg(map(), Connection.t(), term()) :: result()
  def append_recvmsg(message, %Connection{} = state, continuation) do
    message
    |> Rights.decode(state.inbound_fds, rights_context(state))
    |> apply_rights_decision(state, continuation)
  end

  @doc false
  @spec process_inbound(Connection.t(), term()) :: result()
  def process_inbound(%Connection{} = state, continuation) do
    case Inbound.next(state.inbound) do
      {:frame, data, inbound} ->
        parse_complete_message(data, %{state | inbound: inbound}, continuation)

      {:incomplete, inbound} ->
        buffer_incomplete_message(%{state | inbound: inbound}, continuation)

      {:error, reason} ->
        {:protocol_error, reason, state}
    end
  end

  @doc false
  @spec request_timeout(non_neg_integer(), reference(), Connection.t()) :: result()
  def request_timeout(serial, request_ref, %Connection{} = state) do
    case Pending.fetch_by_serial(state.pending, serial) do
      {:ok, %Pending.Entry{request_ref: ^request_ref}} ->
        {entry, pending} = Pending.pop_by_serial(state.pending, serial)
        Pending.fail(entry, {:error, :timeout})
        {:ok, %{state | pending: pending}}

      _stale ->
        {:ok, state}
    end
  end

  @doc false
  @spec discard_inbound_unix_fds(Connection.t()) :: Connection.t()
  def discard_inbound_unix_fds(%Connection{inbound_fds: inbound_fds} = state) do
    _ = UnixFD.close_all(Rights.fds(inbound_fds))
    %{state | inbound_fds: Rights.new()}
  end

  @doc false
  @spec cancel_partial_frame_timer({reference(), reference()} | nil) :: nil
  def cancel_partial_frame_timer(nil), do: nil

  def cancel_partial_frame_timer({timer_ref, _token}) do
    _ = Process.cancel_timer(timer_ref)
    nil
  end

  # Everything the rights decoder borrows from the connection for one
  # `recvmsg` result.
  defp rights_context(%Connection{} = state) do
    %{
      negotiated?: state.unix_fd_negotiated?,
      frame_pending?: Inbound.pending?(state.inbound),
      max_bytes: @max_read_chunk
    }
  end

  # The single close-or-deliver ownership path for every received descriptor:
  # `:frame` retains them for the frame under assembly, and the other two
  # decisions close exactly the descriptors they name.
  defp apply_rights_decision({:frame, data, fds}, %Connection{} = state, continuation) do
    append_inbound(
      data,
      %{state | inbound_fds: Rights.retain(state.inbound_fds, fds)},
      continuation
    )
  end

  defp apply_rights_decision({:quarantine, data, fds}, %Connection{} = state, continuation) do
    _ = UnixFD.close_all(fds)
    append_inbound(data, %{state | inbound_fds: Rights.taint(state.inbound_fds)}, continuation)
  end

  defp apply_rights_decision({:stop, reason, fds}, %Connection{} = state, _continuation) do
    _ = UnixFD.close_all(fds)
    {:protocol_error, reason, state}
  end

  # A timer exists only while a nonempty frame is incomplete. Each retained
  # fragment replaces it, so a peer that is making progress remains connected
  # while a peer that stops or dribbles too slowly cannot pin retained data.
  defp buffer_incomplete_message(%Connection{} = state, continuation) do
    state =
      if Inbound.pending?(state.inbound) do
        %{state | partial_frame_timer: restart_partial_frame_timer(state)}
      else
        clear_partial_frame(state)
      end

    if is_nil(state.rref) do
      {:continue, continuation, state}
    else
      {:ok, state}
    end
  end

  defp clear_partial_frame(%Connection{} = state) do
    %{
      state
      | inbound: Inbound.clear(state.inbound),
        partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)
    }
  end

  defp finish_frame(%Connection{} = state) do
    %{state | partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)}
  end

  defp restart_partial_frame_timer(%Connection{} = state) do
    cancel_partial_frame_timer(state.partial_frame_timer)
    token = make_ref()
    timer_ref = Process.send_after(self(), {:partial_frame_timeout, token}, state.read_timeout)
    {timer_ref, token}
  end

  # One pass over a receive buffer carries two values that never change: the
  # continuation the entry point must return, and the flat binary every
  # remainder is a sub-binary of.
  defp parse_complete_message(data, %Connection{} = state, continuation) do
    parse_flat_messages(data, state, %{continuation: continuation, source: data})
  end

  # `data` is already flat when a complete frame is available. Parse every
  # coalesced frame directly from its sub-binary remainder, retaining only the
  # final incomplete tail. This avoids re-flattening a receive buffer per frame.
  defp parse_flat_messages(<<>>, %Connection{} = state, cursor) do
    process_inbound(state, cursor.continuation)
  end

  defp parse_flat_messages(data, %Connection{} = state, cursor) do
    case Message.parse_inbound(data) do
      nil ->
        append_inbound(Inbound.retain_remainder(data, cursor.source), state, cursor.continuation)

      # Anything but nil either consumed a frame or ends the connection, so
      # the partial-frame timer is cancelled once here rather than on every
      # way out.
      parsed ->
        parse_complete_frame(parsed, finish_frame(state), cursor)
    end
  end

  defp parse_complete_frame({:ok, %Message{} = msg, rest}, %Connection{} = state, cursor) do
    parse_attached_message(msg, rest, state, cursor)
  end

  defp parse_complete_frame(
         {:error, :resource_limit, _envelope, _rest},
         %Connection{hello_serial: hello_serial} = state,
         _cursor
       )
       when not is_nil(hello_serial) do
    {:protocol_error, {:hello_failed, :resource_limit}, state}
  end

  # `parse_inbound/1` knows the frame boundary, so the remainder is always
  # supplied and this frame can be dropped without rescanning the buffer.
  defp parse_complete_frame(
         {:error, :resource_limit, envelope, rest},
         %Connection{} = state,
         cursor
       ) do
    Logger.warning("D-Bus frame dropped: :resource_limit", reason: :resource_limit)
    state = discard_inbound_unix_fds(state)
    {:ok, state} = drop_resource_limited_reply(envelope, state)
    parse_flat_messages(rest, state, cursor)
  end

  defp parse_complete_frame({:error, reason}, %Connection{} = state, _cursor) do
    {:protocol_error, reason, state}
  end

  defp parse_attached_message(%Message{} = msg, rest, %Connection{} = state, cursor) do
    case attach_inbound_fds(msg, state) do
      {:ok, msg, state} ->
        dispatch_inbound_message(msg, rest, state, cursor)

      {:error, reason, state} ->
        drop_recoverable_fd_frame(reason, rest, state, cursor)
    end
  end

  defp attach_inbound_fds(%Message{} = msg, %Connection{} = state) do
    case Rights.attach(state.inbound_fds, msg, state.unix_fd_negotiated?) do
      {:ok, msg, inbound_fds} ->
        {:ok, msg, %{state | inbound_fds: inbound_fds}}

      {:error, reason, fds, inbound_fds} ->
        _ = UnixFD.close_all(fds)
        {:error, reason, %{state | inbound_fds: inbound_fds}}
    end
  end

  # Count/index/negotiation checks run after a complete D-Bus frame and its
  # ancillary data have been collected. The stream boundary is therefore known:
  # close the descriptors, drop only this frame, and continue with a coalesced
  # successor rather than letting a peer kill unrelated calls or handlers.
  defp drop_recoverable_fd_frame(reason, rest, state, cursor) do
    reason = Rights.drop_reason(reason)
    Logger.warning("D-Bus FD frame dropped: #{inspect(reason)}", reason: reason)
    parse_flat_messages(rest, state, cursor)
  end

  defp dispatch_inbound_message(
         %Message{} = msg,
         rest,
         %Connection{hello_serial: hello_serial} = state,
         cursor
       )
       when not is_nil(hello_serial) do
    # dbus-daemon's bus/dispatch.c replies to Hello before emitting the
    # directed NameAcquired signal. Until that reply supplies our unique name,
    # any other frame is a protocol error rather than application traffic.
    if msg.unix_fds != [] do
      close_message_fds(msg)
      {:protocol_error, :invalid_unix_fds, state}
    else
      dispatch_hello_reply(Setup.hello_reply_result(msg, hello_serial), rest, state, cursor)
    end
  end

  defp dispatch_inbound_message(%Message{type: type} = msg, rest, %Connection{} = state, cursor)
       when type in [:method_return, :error] do
    case reply(msg, state) do
      {:ok, state} -> parse_flat_messages(rest, state, cursor)
      {:error, reason} -> {:protocol_error, reason, state}
    end
  end

  defp dispatch_inbound_message(
         %Message{type: :signal} = msg,
         rest,
         %Connection{} = state,
         cursor
       ) do
    # Signals may have multiple subscribers. Without a per-subscriber dup(2)
    # primitive, one raw descriptor cannot be transferred safely to all of
    # them, so FD-bearing signals are rejected and closed.
    if msg.unix_fds == [] do
      parse_flat_messages(rest, notify(msg, state), cursor)
    else
      close_message_fds(msg)
      Logger.warning("D-Bus FD frame dropped: :signal_ownership", reason: :signal_ownership)
      parse_flat_messages(rest, state, cursor)
    end
  end

  defp dispatch_inbound_message(
         %Message{type: :method_call} = msg,
         rest,
         %Connection{} = state,
         cursor
       ) do
    # No method served by this connection takes a descriptor, so any received
    # descriptor is closed before the call is answered.
    close_message_fds(msg)
    parse_flat_messages(rest, Peer.answer(msg, state), cursor)
  end

  defp dispatch_inbound_message(%Message{} = msg, rest, state, cursor) do
    close_message_fds(msg)
    parse_flat_messages(rest, state, cursor)
  end

  # The reader takes over from the setup path here, so this is the one point
  # in a pass where the continuation changes.
  defp dispatch_hello_reply({:ok, name}, rest, %Connection{} = state, cursor) do
    case Setup.establish_connection(%{
           state
           | name: name,
             hello_serial: nil,
             established?: true
         }) do
      {:ok, established} ->
        parse_flat_messages(rest, established, %{cursor | continuation: :recv})

      {:error, reason} ->
        {:shutdown, reason, state}
    end
  end

  defp dispatch_hello_reply({:error, reason}, _rest, %Connection{} = state, _cursor) do
    {:protocol_error, reason, state}
  end

  defp close_message_fds(%Message{unix_fds: fds}), do: UnixFD.close_all(fds)

  # A non-bus connection has no unique name, so nothing can be its own
  # NameAcquired signal. Match the absent name explicitly rather than letting
  # `nil` participate in the header comparison below.
  defp notify(%Message{} = msg, %Connection{name: nil} = state) do
    dispatch_signal(msg, state)
    state
  end

  # Ignore our own NameAcquired signals.
  defp notify(
         %Message{header_fields: %{member: "NameAcquired", destination: name}, body: [name]},
         %Connection{name: name} = state
       ),
       do: state

  defp notify(%Message{} = msg, %Connection{} = state) do
    dispatch_signal(msg, state)
    state
  end

  # Handlers live in this connection's state, so the match rule of a
  # subscription is evaluated here, in the connection process, and only for
  # the signals this connection received.
  defp dispatch_signal(%Message{} = msg, %Connection{handlers: handlers}) do
    Enum.each(handlers, fn {handler_ref, %{pid: pid, rule: rule}} ->
      if is_nil(rule) or MatchRule.matches?(rule, msg) do
        send(pid, {handler_ref, msg})
      end
    end)
  end

  defp reply(%Message{} = msg, %Connection{} = state) do
    case Map.fetch(msg.header_fields, :reply_serial) do
      {:ok, reply_serial} ->
        {:ok, correlate_reply(msg, reply_serial, state)}

      :error ->
        {:error, {:malformed_reply, :missing_reply_serial}}
    end
  end

  defp correlate_reply(%Message{} = msg, reply_serial, %Connection{} = state) do
    case Pending.pop_by_serial(state.pending, reply_serial) do
      {nil, _pending} ->
        close_message_fds(msg)
        log_orphaned_reply(reply_serial)
        state

      {entry, pending} ->
        deliver_reply(msg, entry, %{state | pending: pending})
    end
  end

  defp deliver_reply(
         %Message{unix_fds: []} = msg,
         %Pending.Entry{} = entry,
         %Connection{} = state
       ) do
    Pending.release_monitor(entry)

    if live_from?(entry.from) do
      GenServer.reply(entry.from, msg)
    else
      close_message_fds(msg)
    end

    state
  end

  # The claim keeps the monitor this entry took on its caller, so the reply is
  # handed over without releasing it.
  defp deliver_reply(%Message{} = msg, %Pending.Entry{} = entry, %Connection{} = state) do
    claims =
      FDClaims.open(
        state.fd_claims,
        %{
          msg: msg,
          from: entry.from,
          request_ref: entry.request_ref,
          monitor_ref: entry.monitor_ref,
          deadline: entry.deadline
        },
        Connection.fd_claims_context(state)
      )

    %{state | fd_claims: claims}
  end

  # A pending entry is only ever registered for a call that arrived through
  # `handle_call/3`, so its `from` is always a `t:GenServer.from/0`.
  defp live_from?({pid, _tag}) when is_pid(pid), do: Process.alive?(pid)

  defp drop_resource_limited_reply(
         %{type: :method_return, reply_serial: reply_serial},
         %Connection{} = state
       )
       when is_integer(reply_serial) and reply_serial > 0 do
    drop_resource_limited_pending(reply_serial, :method_return, state)
  end

  defp drop_resource_limited_reply(
         %{type: :error, reply_serial: reply_serial, error_name: error_name},
         %Connection{} = state
       )
       when is_integer(reply_serial) and reply_serial > 0 and is_binary(error_name) do
    drop_resource_limited_pending(reply_serial, {:error, error_name}, state)
  end

  defp drop_resource_limited_reply(_envelope, %Connection{} = state), do: {:ok, state}

  defp drop_resource_limited_pending(reply_serial, reply_kind, %Connection{} = state) do
    case Pending.pop_by_serial(state.pending, reply_serial) do
      {nil, _pending} ->
        log_orphaned_reply(reply_serial)
        {:ok, state}

      {entry, pending} ->
        Pending.fail(entry, {:error, {:reply_dropped, reply_kind}})
        {:ok, %{state | pending: pending}}
    end
  end

  # The call timed out, its caller went down, or the peer answered twice.
  defp log_orphaned_reply(reply_serial) do
    Logger.info("Ignoring late or orphaned D-Bus reply for serial #{reply_serial}")
  end

  defp transport(%Connection{impl: %{transport: transport}}), do: transport
end
