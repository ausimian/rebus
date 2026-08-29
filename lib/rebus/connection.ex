defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.SignalHandler
  alias Rebus.Message
  require Logger

  @default_write_timeout 5_000
  @default_read_timeout 5_000
  @max_auth_line_size 1_024
  @max_read_chunk 65_536
  @max_read_attempts 1
  @max_inbound_segments 64
  @max_serial 4_294_967_295

  @spec call(pid(), Message.t(), non_neg_integer()) :: Message.t() | {:error, term()}
  def call(pid, %Message{} = msg, timeout)
      when is_pid(pid) and is_integer(timeout) and timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + timeout

      try do
        GenServer.call(pid, {:call, msg, deadline, request_ref}, timeout)
      catch
        :exit, {:timeout, _call} ->
          GenServer.cast(pid, {:cancel, request_ref})
          {:error, :timeout}

        :exit, _reason ->
          {:error, :disconnected}
      end
    else
      {:error, :remote_connection_unsupported}
    end
  end

  @spec send(pid(), Message.t(), non_neg_integer()) :: :ok | {:error, term()}
  def send(pid, %Message{} = msg, dispatch_timeout \\ @default_write_timeout)
      when is_pid(pid) and is_integer(dispatch_timeout) and dispatch_timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + dispatch_timeout

      try do
        GenServer.call(pid, {:send, msg, deadline, request_ref}, dispatch_timeout)
      catch
        :exit, {:timeout, _call} ->
          GenServer.cast(pid, {:cancel, request_ref})
          {:error, :timeout}

        :exit, _reason ->
          {:error, :disconnected}
      end
    else
      {:error, :remote_connection_unsupported}
    end
  end

  @spec add_signal_handler(pid()) :: reference()
  def add_signal_handler(conn) when is_pid(conn) do
    GenServer.call(conn, {:add_signal_handler, self()})
  end

  @spec delete_signal_handler(pid(), reference()) :: :ok
  def delete_signal_handler(conn, ref) when is_pid(conn) and is_reference(ref) do
    GenServer.call(conn, {:delete_signal_handler, ref})
  end

  @spec start_link(keyword()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(args) do
    GenServer.start_link(__MODULE__, args)
  end

  typedstruct enforce: true do
    field :sock, :socket.socket()
    field :guid, binary() | nil, default: nil
    field :rref, term() | nil, default: nil
    field :inbound_segments, [{pos_integer(), binary()}], default: []
    field :inbound_size, non_neg_integer(), default: 0
    field :inbound_expected_size, pos_integer() | nil, default: nil
    field :inbound_flatten_count, non_neg_integer(), default: 0
    field :name, binary() | nil, default: nil
    field :serial, non_neg_integer(), default: 1
    field :write_timeout, pos_integer(), default: @default_write_timeout
    field :read_timeout, pos_integer(), default: @default_read_timeout
    field :partial_frame_timer, {reference(), reference()} | nil, default: nil

    field :pending,
          %{
            non_neg_integer() => {:gen_statem.from(), reference(), reference(), reference()}
          },
          default: %{}

    field :request_index, %{reference() => non_neg_integer()}, default: %{}
    field :monitor_index, %{reference() => non_neg_integer()}, default: %{}
    field :active_write, map() | nil, default: nil
    field :write_queue, :queue.queue(), default: :queue.new()
    field :queued_requests, MapSet.t(reference()), default: MapSet.new()
    field :cancelled_requests, MapSet.t(reference()), default: MapSet.new()
    field :outbound_monitor_index, %{reference() => reference()}, default: %{}
    field :send_fun, function(), default: &:socket.send/4
    field :cancel_fun, function(), default: &:socket.cancel/2
  end

  @impl true
  def init(args) do
    %{family: family} = addr = Keyword.fetch!(args, :addr)
    write_timeout = Keyword.get(args, :write_timeout, @default_write_timeout)
    read_timeout = Keyword.get(args, :read_timeout, @default_read_timeout)

    cond do
      not (is_integer(write_timeout) and write_timeout > 0) ->
        {:stop, :invalid_write_timeout}

      not (is_integer(read_timeout) and read_timeout > 0) ->
        {:stop, :invalid_read_timeout}

      true ->
        case :socket.open(family, :stream, :default) do
          {:ok, sock} ->
            _ = configure_receive_buffer(sock)
            initialize(sock, addr, write_timeout, read_timeout)

          {:error, reason} ->
            {:stop, normalize_socket_error(reason)}
        end
    end
  end

  @impl true
  def terminate(_reason, %__MODULE__{sock: sock, partial_frame_timer: timer_ref}) do
    cancel_partial_frame_timer(timer_ref)
    _ = :socket.close(sock)
    :ok
  end

  @impl true
  def handle_info({:"$socket", s, :select, h}, %__MODULE__{sock: s, rref: h} = state) do
    {:noreply, %{state | rref: nil}, {:continue, :recv}}
  end

  # Send select handles are deliberately kept separate from the receive handle.
  # A writable socket must not prevent us from continuing to drain inbound replies.
  def handle_info(
        {:"$socket", s, :select, h},
        %__MODULE__{sock: s, active_write: %{wait: {:select, continuation, h}}} = state
      ) do
    advance_writes(%{
      state
      | active_write: %{state.active_write | wait: {:continue, continuation}}
    })
  end

  def handle_info(
        {:"$socket", s, :completion, {h, result}},
        %__MODULE__{sock: s, active_write: %{wait: {:completion, _continuation, h}}} = state
      ) do
    handle_completion_result(result, %{state | active_write: %{state.active_write | wait: nil}})
  end

  def handle_info(
        {:"$socket", s, :completion, {h, result}},
        %__MODULE__{sock: s, rref: {:completion, h}} = state
      ) do
    handle_read_completion(result, %{state | rref: nil})
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, rref: h} = state
      ) do
    stop_for_transport_error(reason, state)
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, rref: {:completion, h}} = state
      ) do
    stop_for_transport_error(reason, state)
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, %__MODULE__{} = state) do
    case Map.pop(state.monitor_index, ref) do
      {nil, _index} ->
        case Map.pop(state.outbound_monitor_index, ref) do
          {nil, _outbound_index} ->
            :gen_event.delete_handler(SignalHandler, {SignalHandler, ref}, nil)
            {:noreply, state}

          {request_ref, outbound_monitor_index} ->
            state = %{state | outbound_monitor_index: outbound_monitor_index}
            cancel_outbound_request(state, request_ref)
        end

      {serial, monitor_index} ->
        {entry, pending} = Map.pop(state.pending, serial)
        {_from, timer_ref, request_ref, _monitor_ref} = entry
        _ = Process.cancel_timer(timer_ref)

        {:noreply,
         %{
           state
           | pending: pending,
             monitor_index: monitor_index,
             request_index: Map.delete(state.request_index, request_ref)
         }}
    end
  end

  def handle_info({:gen_event_EXIT, {SignalHandler, ref}, _reason}, %__MODULE__{} = state) do
    # Because handlers are addede via :gen_event.add_sup_handler/3, we receive
    # `:gen_event_EXIT` messages when they are removed. We can use this to clean
    # up the monitor
    Process.demonitor(ref, [:flush])
    {:noreply, state}
  end

  def handle_info({:request_timeout, serial, request_ref}, %__MODULE__{} = state) do
    case Map.fetch(state.pending, serial) do
      {:ok, {from, _timer_ref, ^request_ref, monitor_ref}} ->
        {_pending_entry, pending} = Map.pop(state.pending, serial)
        Process.demonitor(monitor_ref, [:flush])
        GenServer.reply(from, {:error, :timeout})
        {:noreply, remove_indexes(%{state | pending: pending}, request_ref, monitor_ref)}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info(
        {:partial_frame_timeout, token},
        %__MODULE__{
          partial_frame_timer: {_timer_ref, token}
        } = state
      ) do
    stop_for_protocol_error(:read_timeout, %{state | partial_frame_timer: nil})
  end

  def handle_info(
        {:write_timeout, request_ref},
        %__MODULE__{active_write: %{request_ref: request_ref} = write} = state
      ) do
    if write.partial? do
      stop_for_transport_error(:timeout, state)
    else
      # No bytes have entered the stream, so this frame can be safely abandoned.
      reply_if_live(write, {:error, :timeout}, state)
      advance_writes(drop_active(state, cancel?: true))
    end
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, active_write: %{wait: {:select, _continuation, h}}} = state
      ),
      do: stop_for_transport_error(reason, state)

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, active_write: %{wait: {:completion, _continuation, h}}} = state
      ),
      do: stop_for_transport_error(reason, state)

  def handle_info(_message, %__MODULE__{} = state), do: {:noreply, state}

  @impl true

  def handle_continue(:hello, %__MODULE__{} = state) do
    # Send the Hello method call
    with {:ok, method} <-
           Message.new(:method_call,
             path: "/",
             interface: "org.freedesktop.DBus",
             destination: "org.freedesktop.DBus",
             member: "Hello"
           ),
         {:ok, bin} <- Message.encode(%{method | serial: state.serial}) do
      case :socket.send(state.sock, bin, [], state.write_timeout) do
        :ok ->
          {:noreply, %{state | serial: next_serial(state.serial)},
           {:continue, :hello_reply_buffer}}

        {:error, reason} ->
          stop_for_transport_error(reason, state)

        _unexpected ->
          stop_for_transport_error(:send_failed, state)
      end
    else
      {:error, reason} -> stop_for_protocol_error(reason, state)
    end
  end

  def handle_continue(:hello_reply_buffer, %__MODULE__{} = state) do
    # Authentication may have read D-Bus bytes along with its final response.
    process_inbound(state, {:hello_reply, read_deadline(state.read_timeout)})
  end

  def handle_continue(:hello_reply, %__MODULE__{} = state) do
    receive_hello_reply(state, read_deadline(state.read_timeout))
  end

  def handle_continue({:hello_reply, deadline}, %__MODULE__{} = state) do
    receive_hello_reply(state, deadline)
  end

  def handle_continue(:recv, %__MODULE__{rref: nil} = state) do
    handle_receive_result(:socket.recv(state.sock, 0, [], :nowait), state)
  end

  # A pending socket operation owns the receive continuation. Keeping this
  # catch-all prevents a stale continuation from crashing and exposing state.
  def handle_continue(:recv, %__MODULE__{} = state), do: {:noreply, state}

  def handle_continue(:write, %__MODULE__{} = state), do: advance_writes(state)

  @doc false
  def handle_receive_result({:ok, data}, %__MODULE__{} = state) when is_binary(data) do
    append_inbound(data, state, :recv)
  end

  def handle_receive_result(
        {:select, {:select_info, :recv, handle}},
        %__MODULE__{} = state
      ) do
    {:noreply, %{state | rref: handle}}
  end

  def handle_receive_result(
        {:select, {{:select_info, :recv, handle}, data}},
        %__MODULE__{} = state
      )
      when is_binary(data) do
    append_inbound(data, %{state | rref: handle}, :recv)
  end

  def handle_receive_result(
        {:completion, {:completion_info, :recv, handle}},
        %__MODULE__{} = state
      ) do
    {:noreply, %{state | rref: {:completion, handle}}}
  end

  def handle_receive_result({:error, reason}, %__MODULE__{} = state) do
    stop_for_transport_error(reason, state)
  end

  def handle_receive_result(_result, %__MODULE__{} = state) do
    stop_for_transport_error(:receive_failed, state)
  end

  defp receive_hello_reply(%__MODULE__{} = state, deadline) do
    case remaining_timeout(deadline, state.read_timeout) do
      :expired ->
        stop_for_protocol_error(:read_timeout, state)

      {:ok, timeout} ->
        receive_hello_reply(state, deadline, timeout)
    end
  end

  defp receive_hello_reply(%__MODULE__{} = state, deadline, timeout) do
    case :socket.recv(state.sock, 0, [], timeout) do
      {:ok, data} ->
        continue_hello_reply(data, state, deadline)

      {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
        continue_hello_reply(data, state, deadline)

      {:error, :timeout} ->
        stop_for_protocol_error(:read_timeout, state)

      {:error, {:timeout, _data}} ->
        stop_for_protocol_error(:read_timeout, state)

      {:error, reason} ->
        stop_for_transport_error(reason, state)
    end
  end

  defp continue_hello_reply(data, %__MODULE__{} = state, deadline) do
    case append_inbound(data, state, {:hello_reply, deadline}) do
      {:noreply, %__MODULE__{} = state, {:continue, {:hello_reply, _deadline}}} ->
        receive_hello_reply(state, deadline)

      result ->
        result
    end
  end

  defp handle_read_completion({:ok, data}, %__MODULE__{} = state) when is_binary(data) do
    append_inbound(data, state, :recv)
  end

  defp handle_read_completion({:error, reason}, %__MODULE__{} = state) do
    stop_for_transport_error(reason, state)
  end

  # Completion-based socket backends may use a result shape that differs from
  # the readiness backend. Treat an unknown result as a clean transport stop
  # rather than raising and allowing GenServer to log buffered peer data.
  defp handle_read_completion(_result, %__MODULE__{} = state) do
    stop_for_transport_error(:receive_failed, state)
  end

  @impl true
  def handle_call({:call, %Message{} = msg, deadline, request_ref}, from, %__MODULE__{} = state) do
    case validate_call_message(msg) do
      :ok ->
        enqueue_write(state, %{
          kind: :call,
          from: from,
          msg: msg,
          deadline: deadline,
          request_ref: request_ref
        })

      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  def handle_call({:send, %Message{} = msg, deadline, request_ref}, from, %__MODULE__{} = state) do
    case validate_send_message(msg) do
      :ok ->
        enqueue_write(state, %{
          kind: :send,
          from: from,
          msg: msg,
          deadline: deadline,
          request_ref: request_ref
        })

      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  def handle_call({:add_signal_handler, pid}, _from, %__MODULE__{} = state) do
    ref = Process.monitor(pid)
    :ok = :gen_event.add_sup_handler(SignalHandler, {SignalHandler, ref}, {self(), pid, ref})
    {:reply, ref, state}
  end

  def handle_call({:delete_signal_handler, ref}, _from, %__MODULE__{} = state) do
    Process.demonitor(ref, [:flush])
    :gen_event.delete_handler(SignalHandler, {SignalHandler, ref}, nil)
    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:cancel, request_ref}, %__MODULE__{} = state) do
    case Map.pop(state.request_index, request_ref) do
      {nil, _index} ->
        case state.active_write do
          %{request_ref: ^request_ref, partial?: false} ->
            advance_writes(drop_active(state, cancel?: true))

          %{request_ref: ^request_ref} ->
            {:noreply,
             %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}

          _ ->
            if MapSet.member?(state.queued_requests, request_ref) do
              {:noreply,
               %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}
            else
              {:noreply, state}
            end
        end

      {serial, request_index} ->
        {entry, pending} = Map.pop(state.pending, serial)
        {_from, timer_ref, _request_ref, monitor_ref} = entry
        _ = Process.cancel_timer(timer_ref)
        Process.demonitor(monitor_ref, [:flush])

        {:noreply,
         %{
           state
           | pending: pending,
             request_index: request_index,
             monitor_index: Map.delete(state.monitor_index, monitor_ref)
         }}
    end
  end

  defp cancel_outbound_request(state, request_ref) do
    case state.active_write do
      %{request_ref: ^request_ref, partial?: false} ->
        advance_writes(drop_active(state, cancel?: true))

      %{request_ref: ^request_ref} ->
        {:noreply,
         %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}

      _ ->
        {:noreply,
         %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}
    end
  end

  defp parse_complete_message(data, %__MODULE__{} = state, continuation) do
    parse_flat_messages(data, state, continuation, data)
  end

  # `data` is already flat when a complete frame is available. Parse every
  # coalesced frame directly from its sub-binary remainder, retaining only the
  # final incomplete tail. This avoids re-flattening a receive buffer per frame.
  defp parse_flat_messages(<<>>, %__MODULE__{} = state, continuation, _source) do
    process_inbound(state, continuation)
  end

  defp parse_flat_messages(data, %__MODULE__{} = state, continuation, source) do
    case Message.parse(data) do
      {:ok, %Message{} = msg, rest} when is_nil(state.name) ->
        state = finish_frame(state)

        case hello_reply_result(msg) do
          {:ok, name} -> parse_flat_messages(rest, %{state | name: name}, :recv, source)
          {:error, reason} -> stop_for_protocol_error(reason, state)
        end

      {:ok, %Message{type: type} = msg, rest} when type in [:method_return, :error] ->
        state = finish_frame(state)

        case reply(msg, state) do
          {:ok, state} -> parse_flat_messages(rest, state, continuation, source)
          {:error, reason} -> stop_for_protocol_error(reason, state)
        end

      {:ok, %Message{type: :signal} = msg, rest} ->
        state = finish_frame(state)
        parse_flat_messages(rest, notify(msg, state), continuation, source)

      {:ok, %Message{type: :method_call}, rest} ->
        parse_flat_messages(rest, finish_frame(state), continuation, source)

      {:ok, %Message{}, rest} ->
        parse_flat_messages(rest, finish_frame(state), continuation, source)

      nil ->
        append_inbound(retain_remainder(data, source), state, continuation)

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  defp initialize(sock, addr, write_timeout, read_timeout) do
    auth = "AUTH EXTERNAL #{get_auth_id()}\r\n"

    with :ok <- connect_socket(sock, addr, read_timeout),
         :ok <- handshake_send(sock, [0, auth], write_timeout),
         {:ok, <<"OK ", guid::binary-size(32)>>, rest} <-
           handshake_recv(sock, read_timeout),
         :ok <- handshake_send(sock, "BEGIN \r\n", write_timeout) do
      {:ok,
       %__MODULE__{
         sock: sock,
         guid: guid,
         inbound_segments: if(rest == <<>>, do: [], else: [{byte_size(rest), rest}]),
         inbound_size: byte_size(rest),
         write_timeout: write_timeout,
         read_timeout: read_timeout
       }, {:continue, :hello}}
    else
      {:ok, _, _} -> stop_and_close(sock, :auth_failed)
      {:error, reason} -> stop_and_close(sock, reason)
    end
  end

  defp stop_and_close(sock, reason) do
    _ = :socket.close(sock)
    {:stop, normalize_socket_error(reason)}
  end

  defp connect_socket(sock, addr, timeout) do
    case :socket.connect(sock, addr, timeout) do
      :ok -> :ok
      {:error, :timeout} -> {:error, :read_timeout}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  def configure_receive_buffer(
        sock,
        setopt_fun \\ &:socket.setopt/3,
        warning_fun \\ fn message -> Logger.warning(message) end
      ) do
    # A zero-length receive returns the bytes currently available on every
    # supported OTP release. Keep the backing allocation independent of a
    # peer-declared D-Bus frame length. Some backends only accept the scalar
    # form, so failure to tune this hint must never make connections unavailable.
    case setopt_fun.(sock, {:otp, :rcvbuf}, {@max_read_attempts, @max_read_chunk}) do
      :ok ->
        :tuple

      {:error, _reason} ->
        case setopt_fun.(sock, {:otp, :rcvbuf}, @max_read_chunk) do
          :ok ->
            :scalar

          {:error, _reason} ->
            default_receive_buffer(warning_fun)

          _other ->
            default_receive_buffer(warning_fun)
        end

      _other ->
        default_receive_buffer(warning_fun)
    end
  end

  defp default_receive_buffer(warning_fun) do
    warning_fun.("D-Bus connection is using OTP's default receive buffer")
    :default
  end

  defp handshake_recv(sock, timeout) do
    receive_auth_line(sock, <<>>, read_deadline(timeout), timeout)
  end

  defp receive_auth_line(sock, buffer, deadline, timeout) do
    case remaining_timeout(deadline, timeout) do
      :expired ->
        {:error, :read_timeout}

      {:ok, receive_timeout} ->
        case :socket.recv(sock, 0, [], receive_timeout) do
          {:ok, data} ->
            consume_auth_data(sock, buffer, data, deadline, timeout)

          {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
            consume_auth_data(sock, buffer, data, deadline, timeout)

          {:error, :timeout} ->
            {:error, :read_timeout}

          {:error, {:timeout, _data}} ->
            {:error, :read_timeout}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp consume_auth_data(sock, buffer, data, deadline, timeout) do
    buffer = buffer <> data

    case :binary.match(buffer, "\r\n") do
      {line_size, 2} when line_size <= @max_auth_line_size ->
        line = binary_part(buffer, 0, line_size)
        rest_size = byte_size(buffer) - line_size - 2
        rest = binary_part(buffer, line_size + 2, rest_size)
        {:ok, line, rest}

      {_, 2} ->
        {:error, :auth_failed}

      :nomatch when byte_size(buffer) > @max_auth_line_size ->
        {:error, :auth_failed}

      :nomatch ->
        receive_auth_line(sock, buffer, deadline, timeout)
    end
  end

  defp handshake_send(sock, data, timeout) do
    case :socket.send(sock, data, [], timeout) do
      :ok -> :ok
      {:error, reason} -> {:error, normalize_socket_error(reason)}
      _other -> {:error, :send_failed}
    end
  end

  @doc false
  @spec normalize_socket_error(term()) :: term()
  def normalize_socket_error({reason, partial} = error) when is_atom(reason) do
    if is_binary(partial) or iolist?(partial), do: reason, else: error
  end

  def normalize_socket_error(reason), do: reason

  defp stop_for_transport_error(reason, %__MODULE__{} = state) do
    reason = normalize_socket_error(reason)
    Logger.warning("D-Bus connection transport stopped: #{inspect(reason)}")
    {:stop, {:shutdown, reason}, fail_pending(state)}
  end

  defp stop_for_protocol_error(reason, %__MODULE__{} = state) do
    reason = sanitize_protocol_reason(reason)
    Logger.warning("D-Bus connection protocol stopped: #{inspect(reason)}")
    {:stop, {:shutdown, reason}, fail_pending(state)}
  end

  # Each zero-length receive returns data already available through the fixed
  # OTP buffer. Fixed-header validation still happens as soon as 16 bytes are
  # retained, without making allocation depend on a peer-declared frame length.
  @doc false
  @spec append_inbound_fragment(binary(), t(), term()) ::
          {:noreply, t()} | {:noreply, t(), {:continue, term()}} | {:stop, term(), t()}
  def append_inbound_fragment(data, %__MODULE__{} = state, continuation)
      when is_binary(data) do
    append_inbound(data, state, continuation)
  end

  defp append_inbound(<<>>, %__MODULE__{} = state, continuation),
    do: process_inbound(state, continuation)

  defp append_inbound(data, %__MODULE__{} = state, continuation) do
    segments = append_segment(data, state.inbound_segments)

    if length(segments) <= @max_inbound_segments do
      state = %{
        state
        | inbound_segments: segments,
          inbound_size: state.inbound_size + byte_size(data)
      }

      process_inbound(state, continuation)
    else
      # Segment metadata is part of the retained inbound budget. A peer that
      # defeats rope merging with pathological fragment sizes is rejected
      # before its BEAM-term overhead becomes unbounded.
      stop_for_protocol_error(:message_too_large, state)
    end
  end

  defp process_inbound(%__MODULE__{inbound_size: 0} = state, continuation),
    do: buffer_incomplete_message(state, continuation)

  defp process_inbound(%__MODULE__{inbound_expected_size: nil} = state, continuation) do
    case Message.expected_size(inbound_prefix(state, min(state.inbound_size, 16))) do
      {:ok, expected_size} ->
        process_inbound(%{state | inbound_expected_size: expected_size}, continuation)

      nil ->
        buffer_incomplete_message(state, continuation)

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  defp process_inbound(%__MODULE__{} = state, continuation) do
    if state.inbound_size >= state.inbound_expected_size do
      data = inbound_binary(state)

      parse_complete_message(
        data,
        clear_inbound_frame(%{state | inbound_flatten_count: state.inbound_flatten_count + 1}),
        continuation
      )
    else
      buffer_incomplete_message(state, continuation)
    end
  end

  @doc false
  @spec inbound_receive_buffer_size() :: pos_integer()
  def inbound_receive_buffer_size, do: @max_read_chunk

  defp inbound_prefix(%__MODULE__{} = state, size) do
    state.inbound_segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> take_prefix(size, [])
    |> IO.iodata_to_binary()
  end

  defp take_prefix(_segments, 0, acc), do: Enum.reverse(acc)
  defp take_prefix([], _size, acc), do: Enum.reverse(acc)

  defp take_prefix([segment | segments], size, acc) when byte_size(segment) <= size do
    take_prefix(segments, size - byte_size(segment), [segment | acc])
  end

  defp take_prefix([segment | _segments], size, acc) do
    Enum.reverse([binary_part(segment, 0, size) | acc])
  end

  defp inbound_binary(%__MODULE__{} = state) do
    state.inbound_segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> IO.iodata_to_binary()
  end

  # Segments are newest first. Merging a segment only with smaller or equal
  # predecessors keeps common small-fragment traffic logarithmic while
  # preserving byte order. The explicit segment limit protects pathological
  # decreasing fragment sizes without flattening an ever-growing buffer.
  defp append_segment(data, segments) do
    merge_segment(byte_size(data), data, segments)
  end

  defp merge_segment(size, data, [{previous_size, previous} | segments])
       when previous_size <= size do
    merge_segment(previous_size + size, previous <> data, segments)
  end

  defp merge_segment(size, data, segments), do: [{size, data} | segments]

  defp retain_remainder(remainder, source) do
    if byte_size(remainder) * 4 < byte_size(source) do
      :binary.copy(remainder)
    else
      remainder
    end
  end

  defp clear_inbound_frame(%__MODULE__{} = state),
    do: %{state | inbound_segments: [], inbound_size: 0, inbound_expected_size: nil}

  # A timer exists only while a nonempty frame is incomplete. Each retained
  # fragment replaces it, so a peer that is making progress remains connected
  # while a peer that stops or dribbles too slowly cannot pin retained data.
  defp buffer_incomplete_message(%__MODULE__{inbound_size: 0, rref: rref} = state, _continuation)
       when not is_nil(rref) do
    {:noreply, clear_partial_frame(state)}
  end

  defp buffer_incomplete_message(%__MODULE__{inbound_size: 0} = state, continuation) do
    {:noreply, clear_partial_frame(state), {:continue, continuation}}
  end

  defp buffer_incomplete_message(%__MODULE__{rref: rref} = state, _continuation)
       when not is_nil(rref) do
    timer_ref = restart_partial_frame_timer(state)
    {:noreply, %{state | partial_frame_timer: timer_ref}}
  end

  defp buffer_incomplete_message(%__MODULE__{} = state, continuation) do
    timer_ref = restart_partial_frame_timer(state)
    {:noreply, %{state | partial_frame_timer: timer_ref}, {:continue, continuation}}
  end

  defp clear_partial_frame(%__MODULE__{} = state) do
    %{
      state
      | inbound_segments: [],
        inbound_size: 0,
        inbound_expected_size: nil,
        partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)
    }
  end

  defp finish_frame(%__MODULE__{} = state) do
    %{state | partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)}
  end

  defp restart_partial_frame_timer(%__MODULE__{} = state) do
    cancel_partial_frame_timer(state.partial_frame_timer)
    token = make_ref()
    timer_ref = Process.send_after(self(), {:partial_frame_timeout, token}, state.read_timeout)
    {timer_ref, token}
  end

  defp cancel_partial_frame_timer(nil), do: nil

  defp cancel_partial_frame_timer({timer_ref, _token}) do
    _ = Process.cancel_timer(timer_ref)
    nil
  end

  defp hello_reply_result(%Message{
         type: :method_return,
         header_fields: %{reply_serial: 1},
         body: [name | _]
       })
       when is_binary(name) do
    {:ok, name}
  end

  defp hello_reply_result(%Message{type: :method_return, header_fields: %{reply_serial: 1}}) do
    {:error, {:hello_failed, :missing_unique_name}}
  end

  defp hello_reply_result(%Message{type: :error, header_fields: %{reply_serial: 1}} = msg) do
    {:error, {:hello_failed, hello_error_reason(msg.header_fields)}}
  end

  defp hello_reply_result(%Message{type: type}) do
    {:error, {:unexpected_handshake_message, type}}
  end

  defp hello_error_reason(header_fields) do
    case Map.fetch(header_fields, :error_name) do
      :error ->
        :missing_error_name

      {:ok, error_name} ->
        if valid_error_name?(error_name), do: error_name, else: :invalid_error_name
    end
  end

  @doc false
  @spec sanitize_protocol_reason(term()) ::
          :insufficient_data
          | :invalid_endianness
          | :invalid_message
          | :invalid_message_type
          | :message_too_large
          | :read_timeout
          | :unsupported_protocol_version
          | :protocol_error
          | {:hello_failed,
             binary() | :invalid_error_name | :missing_error_name | :missing_unique_name}
          | {:malformed_reply, :missing_reply_serial}
          | {:unexpected_handshake_message, Message.message_type()}
  def sanitize_protocol_reason(reason) do
    case reason do
      {:hello_failed, reason}
      when reason in [:missing_unique_name, :missing_error_name, :invalid_error_name] ->
        {:hello_failed, reason}

      {:hello_failed, error_name} when is_binary(error_name) ->
        if valid_error_name?(error_name),
          do: {:hello_failed, error_name},
          else: {:hello_failed, :invalid_error_name}

      {:hello_failed, _reason} ->
        {:hello_failed, :invalid_error_name}

      {:unexpected_handshake_message, type} when is_atom(type) ->
        {:unexpected_handshake_message, type}

      {:malformed_reply, :missing_reply_serial} ->
        {:malformed_reply, :missing_reply_serial}

      reason
      when reason in [
             :insufficient_data,
             :invalid_endianness,
             :invalid_message,
             :invalid_message_type,
             :message_too_large,
             :read_timeout,
             :unsupported_protocol_version
           ] ->
        reason

      _reason ->
        :protocol_error
    end
  end

  defp valid_error_name?(name) when is_binary(name) and byte_size(name) <= 255 do
    case :binary.split(name, ".", [:global]) do
      [_, _ | _] = parts -> Enum.all?(parts, &valid_error_name_element?/1)
      _ -> false
    end
  end

  defp valid_error_name?(_name), do: false

  defp valid_error_name_element?(<<first, rest::binary>>)
       when first in ?A..?Z or first in ?a..?z or first == ?_ do
    valid_error_name_tail?(rest)
  end

  defp valid_error_name_element?(_element), do: false

  defp valid_error_name_tail?(<<>>), do: true

  defp valid_error_name_tail?(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char == ?_ do
    valid_error_name_tail?(rest)
  end

  defp valid_error_name_tail?(_rest), do: false

  defp iolist?(data) do
    try do
      _ = IO.iodata_to_binary(data)
      true
    rescue
      ArgumentError -> false
    end
  end

  defp notify(%Message{} = msg, %__MODULE__{name: name} = state) do
    case msg do
      %Message{header_fields: %{member: "NameAcquired", destination: ^name}, body: [^name]} ->
        # Ignore our own NameAcquired signals
        :ok

      _ ->
        Rebus.SignalHandler.notify(msg)
    end

    state
  end

  defp reply(%Message{} = msg, %__MODULE__{} = state) do
    case Map.fetch(msg.header_fields, :reply_serial) do
      {:ok, reply_serial} ->
        case Map.pop(state.pending, reply_serial) do
          {nil, _pending} ->
            Logger.info("Ignoring late or orphaned D-Bus reply for serial #{reply_serial}")
            {:ok, state}

          {{from, timer_ref, request_ref, monitor_ref}, pending} ->
            _ = Process.cancel_timer(timer_ref)
            Process.demonitor(monitor_ref, [:flush])
            GenServer.reply(from, msg)
            {:ok, remove_indexes(%{state | pending: pending}, request_ref, monitor_ref)}
        end

      :error ->
        {:error, {:malformed_reply, :missing_reply_serial}}
    end
  end

  defp encode_message(%Message{} = msg) do
    try do
      {:ok, bin} = Message.encode(msg)
      {:ok, bin}
    rescue
      exception ->
        Logger.warning("D-Bus message encoding failed: #{inspect(exception.__struct__)}")
        {:error, :encode_failed}
    catch
      kind, _reason ->
        Logger.warning("D-Bus message encoding failed: #{inspect(kind)}")
        {:error, :encode_failed}
    end
  end

  defp validate_call_message(%Message{type: :method_call, flags: flags}) do
    if :no_reply_expected in flags, do: {:error, :no_reply_expected}, else: :ok
  end

  defp validate_call_message(%Message{type: type}), do: {:error, {:invalid_message_type, type}}

  defp validate_send_message(%Message{type: :signal}), do: :ok

  defp validate_send_message(%Message{type: :method_call, flags: flags}) do
    if :no_reply_expected in flags, do: :ok, else: {:error, :reply_expected}
  end

  defp validate_send_message(%Message{type: type}), do: {:error, {:invalid_message_type, type}}

  defp remaining_timeout(deadline) when is_integer(deadline) do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, remaining}
      _ -> {:error, :timeout}
    end
  end

  defp read_deadline(timeout) when is_integer(timeout) and timeout > 0 do
    System.monotonic_time(:millisecond) + timeout
  end

  defp remaining_timeout(deadline, maximum) when is_integer(deadline) and maximum > 0 do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, min(remaining, maximum)}
      _ -> :expired
    end
  end

  @doc false
  defguardp is_select_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :select_info and
                   elem(info, 1) == :send and is_reference(elem(info, 2))

  defguardp is_completion_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :completion_info and
                   elem(info, 1) == :send and is_reference(elem(info, 2))

  @spec classify_send_result(term(), non_neg_integer()) ::
          :ok
          | {:continue, iodata()}
          | {:select, tuple(), binary() | nil}
          | {:completion, term()}
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

  def classify_send_result({:completion, completion_info}, _payload_length)
      when is_completion_info(completion_info),
      do: {:completion, completion_info}

  def classify_send_result({:error, {:timeout, rest}}, payload_length) do
    if iolist?(rest) and IO.iodata_length(rest) == payload_length,
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

  # Writes are one-frame-at-a-time.  OTP retains the unaccepted RestData in every
  # partial result; retaining it here is what preserves D-Bus stream framing.
  defp enqueue_write(state, operation) do
    monitor_ref = Process.monitor(elem(operation.from, 0))
    operation = Map.put(operation, :monitor_ref, monitor_ref)

    advance_writes(%{
      state
      | write_queue: :queue.in(operation, state.write_queue),
        queued_requests: MapSet.put(state.queued_requests, operation.request_ref),
        outbound_monitor_index:
          Map.put(state.outbound_monitor_index, monitor_ref, operation.request_ref)
    })
  end

  defp advance_writes(%__MODULE__{active_write: nil} = state) do
    case :queue.out(state.write_queue) do
      {:empty, _} ->
        {:noreply, state}

      {{:value, operation}, queue} ->
        state = %{
          state
          | write_queue: queue,
            queued_requests: MapSet.delete(state.queued_requests, operation.request_ref)
        }

        if cancelled_or_expired?(operation, state) do
          state = release_outbound_monitor(state, operation)

          advance_writes(%{
            state
            | cancelled_requests: MapSet.delete(state.cancelled_requests, operation.request_ref)
          })
        else
          case allocate_serial(state.serial, state.pending) do
            {:ok, serial} ->
              case encode_message(%{operation.msg | serial: serial}) do
                {:ok, bin} ->
                  bin = IO.iodata_to_binary(bin)

                  timer_ref =
                    Process.send_after(
                      self(),
                      {:write_timeout, operation.request_ref},
                      state.write_timeout
                    )

                  write =
                    Map.merge(operation, %{
                      serial: serial,
                      rest: bin,
                      frame_size: byte_size(bin),
                      wait: nil,
                      timer_ref: timer_ref,
                      partial?: false
                    })

                  advance_writes(%{state | active_write: write})

                {:error, reason} ->
                  state = release_outbound_monitor(state, operation)
                  GenServer.reply(operation.from, {:error, reason})
                  advance_writes(state)
              end

            {:error, reason} ->
              state = release_outbound_monitor(state, operation)
              GenServer.reply(operation.from, {:error, reason})
              advance_writes(state)
          end
        end
    end
  end

  defp advance_writes(%__MODULE__{active_write: %{wait: {:select, _, _}}} = state),
    do: {:noreply, state}

  defp advance_writes(%__MODULE__{active_write: %{wait: {:completion, _, _}}} = state),
    do: {:noreply, state}

  defp advance_writes(%__MODULE__{active_write: write} = state) do
    cond do
      (expired?(write) or cancelled?(write, state)) and not write.partial? ->
        advance_writes(drop_active(state, cancel?: true))

      true ->
        {rest, flags_or_cont, timeout} = socket_send_args(write.rest, write.wait)
        result = state.send_fun.(state.sock, rest, flags_or_cont, timeout)
        handle_write_result(result, %{state | active_write: %{write | wait: nil}})
    end
  end

  defp handle_write_result(result, %__MODULE__{active_write: write} = state) do
    case classify_send_result(result, write.frame_size) do
      :ok ->
        complete_active_write(state)

      {:continue, rest} ->
        state = put_active_rest(state, rest)
        {:noreply, state, {:continue, :write}}

      {:select, continuation, rest} ->
        state = if rest, do: put_active_rest(state, rest), else: state
        {:select_info, :send, handle} = continuation

        {:noreply,
         %{state | active_write: %{state.active_write | wait: {:select, continuation, handle}}}}

      {:completion, {:completion_info, :send, notification_handle} = handle} ->
        {:noreply,
         %{
           state
           | active_write: %{
               state.active_write
               | wait: {:completion, handle, notification_handle}
             }
         }}

      {:error, {:send_fatal, reason}} ->
        stop_for_transport_error(reason, state)

      {:error, reason} ->
        if write.partial? do
          stop_for_transport_error(reason, state)
        else
          reply_if_live(write, {:error, reason}, state)
          advance_writes(drop_active(state, cancel?: true))
        end
    end
  end

  defp handle_completion_result(:ok, state), do: complete_active_write(state)

  defp handle_completion_result({:ok, written}, %__MODULE__{active_write: write} = state)
       when is_integer(written) and written >= 0 and written < byte_size(write.rest) do
    <<_sent::binary-size(written), rest::binary>> = write.rest
    state = put_active_rest(state, rest)
    {:noreply, state, {:continue, :write}}
  end

  defp handle_completion_result({:error, reason}, state),
    do: stop_for_transport_error(reason, state)

  defp handle_completion_result(_unexpected, state),
    do: stop_for_transport_error(:send_failed, state)

  defp put_active_rest(%__MODULE__{active_write: write} = state, rest) do
    partial? = write.partial? or byte_size(rest) < byte_size(write.rest)
    %{state | active_write: %{write | rest: rest, partial?: partial?}}
  end

  defp complete_active_write(%__MODULE__{active_write: write} = state) do
    live? = not cancelled_or_expired?(write, state)
    state = drop_active(state, retain_monitor?: live? and write.kind == :call)
    state = %{state | serial: next_serial(write.serial)}

    if not live? do
      advance_writes(state)
    else
      case write.kind do
        :send ->
          GenServer.reply(write.from, :ok)
          advance_writes(state)

        :call ->
          case remaining_timeout(write.deadline) do
            {:ok, remaining} ->
              timer_ref =
                Process.send_after(
                  self(),
                  {:request_timeout, write.serial, write.request_ref},
                  remaining
                )

              state = %{
                state
                | outbound_monitor_index:
                    Map.delete(state.outbound_monitor_index, write.monitor_ref)
              }

              pending =
                Map.put(
                  state.pending,
                  write.serial,
                  {write.from, timer_ref, write.request_ref, write.monitor_ref}
                )

              advance_writes(%{
                state
                | pending: pending,
                  request_index: Map.put(state.request_index, write.request_ref, write.serial),
                  monitor_index: Map.put(state.monitor_index, write.monitor_ref, write.serial)
              })

            {:error, :timeout} ->
              advance_writes(release_outbound_monitor(state, write))
          end
      end
    end
  end

  defp drop_active(%__MODULE__{active_write: write} = state, opts) do
    _ = Process.cancel_timer(write.timer_ref)

    if Keyword.get(opts, :cancel?, false), do: cancel_socket_write(state, write.wait)

    state = %{
      state
      | active_write: nil,
        cancelled_requests: MapSet.delete(state.cancelled_requests, write.request_ref)
    }

    if Keyword.get(opts, :retain_monitor?, false),
      do: state,
      else: release_outbound_monitor(state, write)
  end

  defp release_outbound_monitor(state, operation) do
    Process.demonitor(operation.monitor_ref, [:flush])

    %{
      state
      | outbound_monitor_index: Map.delete(state.outbound_monitor_index, operation.monitor_ref)
    }
  end

  defp cancelled_or_expired?(operation, state) do
    cancelled?(operation, state) or expired?(operation)
  end

  defp cancelled?(operation, state),
    do: MapSet.member?(state.cancelled_requests, operation.request_ref)

  defp expired?(operation), do: match?({:error, :timeout}, remaining_timeout(operation.deadline))

  @doc false
  @spec socket_send_args(binary(), nil | {:continue, tuple()}) ::
          {binary(), [] | tuple(), :nowait}
  def socket_send_args(rest, {:continue, continuation}), do: {rest, continuation, :nowait}
  def socket_send_args(rest, _wait), do: {rest, [], :nowait}

  defp cancel_socket_write(state, {:select, continuation, _handle}) do
    _ = state.cancel_fun.(state.sock, continuation)
    :ok
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  defp cancel_socket_write(state, {:completion, continuation, _handle}) do
    _ = state.cancel_fun.(state.sock, continuation)
    :ok
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  defp cancel_socket_write(_state, _wait), do: :ok

  defp reply_if_live(operation, reply, state) do
    if not cancelled_or_expired?(operation, state), do: GenServer.reply(operation.from, reply)
  end

  defp fail_pending(%__MODULE__{} = state) do
    case state.active_write do
      nil ->
        :ok

      write ->
        _ = Process.cancel_timer(write.timer_ref)
        Process.demonitor(write.monitor_ref, [:flush])
        GenServer.reply(write.from, {:error, :disconnected})
    end

    :queue.to_list(state.write_queue)
    |> Enum.each(fn operation ->
      Process.demonitor(operation.monitor_ref, [:flush])
      GenServer.reply(operation.from, {:error, :disconnected})
    end)

    Enum.each(state.pending, fn {_serial, {from, timer_ref, _request_ref, monitor_ref}} ->
      _ = Process.cancel_timer(timer_ref)
      Process.demonitor(monitor_ref, [:flush])
      GenServer.reply(from, {:error, :disconnected})
    end)

    %{
      state
      | pending: %{},
        request_index: %{},
        monitor_index: %{},
        outbound_monitor_index: %{},
        active_write: nil,
        write_queue: :queue.new(),
        queued_requests: MapSet.new(),
        cancelled_requests: MapSet.new()
    }
  end

  defp remove_indexes(state, request_ref, monitor_ref) do
    %{
      state
      | request_index: Map.delete(state.request_index, request_ref),
        monitor_index: Map.delete(state.monitor_index, monitor_ref)
    }
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

  defp get_auth_id do
    {resp, 0} = System.cmd("id", ["-u"])

    resp
    |> String.trim()
    |> :binary.encode_hex()
  end
end
