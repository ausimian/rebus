defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.SignalHandler
  alias Rebus.Message
  require Logger

  def send(pid, %Message{} = msg) when is_pid(pid) do
    GenServer.call(pid, {:send, msg})
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
    field :rref, reference() | nil, default: nil
    field :prev, binary(), default: <<>>
    field :name, binary() | nil, default: nil
    field :serial, non_neg_integer(), default: 1
    field :pending, %{non_neg_integer() => :gen_statem.from()}, default: %{}
  end

  @impl true
  def init(args) do
    %{family: family} = addr = Keyword.fetch!(args, :addr)

    case :socket.open(family, :stream, :default) do
      {:ok, sock} -> initialize(sock, addr)
      {:error, reason} -> {:stop, normalize_socket_error(reason)}
    end
  end

  @impl true
  def terminate(_reason, %__MODULE__{sock: sock}) do
    _ = :socket.close(sock)
    :ok
  end

  @impl true
  def handle_info({:"$socket", s, :select, h}, %__MODULE__{sock: s, rref: h} = state) do
    {:noreply, %{state | rref: nil}, {:continue, :recv}}
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, rref: h} = state
      ) do
    stop_for_transport_error(reason, state)
  end

  def handle_info({:DOWN, ref, _, _, _}, %__MODULE__{} = state) do
    :gen_event.delete_handler(SignalHandler, {SignalHandler, ref}, nil)
    {:noreply, state}
  end

  def handle_info({:gen_event_EXIT, {SignalHandler, ref}, _reason}, %__MODULE__{} = state) do
    # Because handlers are addede via :gen_event.add_sup_handler/3, we receive
    # `:gen_event_EXIT` messages when they are removed. We can use this to clean
    # up the monitor
    Process.demonitor(ref, [:flush])
    {:noreply, state}
  end

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
      case :socket.send(state.sock, bin) do
        :ok ->
          {:noreply, %{state | serial: state.serial + 1}, {:continue, :hello_reply_buffer}}

        {:error, reason} ->
          stop_for_transport_error(reason, state)
      end
    else
      {:error, reason} -> stop_for_protocol_error(reason, state)
    end
  end

  def handle_continue(:hello_reply_buffer, %__MODULE__{} = state) do
    # Authentication may have read D-Bus bytes along with its final response.
    parse_hello_reply(state.prev, state)
  end

  def handle_continue(:hello_reply, %__MODULE__{} = state) do
    # Wait for the Hello reply
    case :socket.recv(state.sock, 0, [], 5_000) do
      {:ok, data} ->
        parse_hello_reply(state.prev <> data, state)

      {:error, reason} ->
        stop_for_transport_error(reason, state)
    end
  end

  def handle_continue(:recv, %__MODULE__{rref: nil} = state) do
    case :socket.recv(state.sock, 0, [], :nowait) do
      {:ok, data} ->
        parse(state.prev <> data, %__MODULE__{state | prev: <<>>})

      {:select, {:select_info, :recv, handle}} ->
        {:noreply, %{state | rref: handle}}

      {:error, reason} ->
        stop_for_transport_error(reason, state)
    end
  end

  @impl true
  def handle_call({:send, %Message{} = msg}, from, %__MODULE__{} = state) do
    msg = %{msg | serial: state.serial}
    {:ok, bin} = Message.encode(msg)

    case :socket.send(state.sock, bin) do
      :ok ->
        if msg.type == :method_call && !Enum.member?(msg.flags, :no_reply_expected) do
          pending = Map.put(state.pending, msg.serial, from)
          {:noreply, %{state | pending: pending, serial: state.serial + 1}}
        else
          {:reply, :ok, %{state | serial: state.serial + 1}}
        end

      error ->
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

  defp parse(data, %__MODULE__{} = state) do
    case Message.parse(data) do
      {:ok, %Message{type: type, header_fields: %{reply_serial: 1}} = msg, rest}
      when is_nil(state.name) and type in [:method_return, :error] ->
        case hello_reply_result(msg) do
          {:ok, name} -> parse(rest, %{state | name: name})
          {:error, reason} -> stop_for_protocol_error(reason, state)
        end

      {:ok, %Message{type: type} = msg, rest} when type in [:method_return, :error] ->
        case reply(msg, state) do
          {:ok, state} -> parse(rest, state)
          {:error, reason} -> stop_for_protocol_error(reason, state)
        end

      {:ok, %Message{type: :signal} = msg, rest} ->
        parse(rest, notify(msg, state))

      {:ok, %Message{type: :method_call}, rest} ->
        parse(rest, state)

      {:ok, %Message{}, rest} ->
        parse(rest, state)

      nil ->
        # Incomplete message, store data for next recv
        {:noreply, %{state | prev: data}, {:continue, :recv}}

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  defp initialize(sock, addr) do
    auth = "AUTH EXTERNAL #{get_auth_id()}\r\n"

    with :ok <- :socket.connect(sock, addr),
         :ok <- :socket.send(sock, [0, auth]),
         {:ok, <<"OK ", guid::binary-size(32), "\r\n", rest::binary>>} <- :socket.recv(sock, 0),
         :ok <- :socket.send(sock, "BEGIN \r\n") do
      {:ok, %__MODULE__{sock: sock, guid: guid, prev: rest}, {:continue, :hello}}
    else
      {:ok, _} -> stop_and_close(sock, :auth_failed)
      {:error, reason} -> stop_and_close(sock, reason)
    end
  end

  defp stop_and_close(sock, reason) do
    _ = :socket.close(sock)
    {:stop, normalize_socket_error(reason)}
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
    {:stop, {:shutdown, reason}, state}
  end

  defp stop_for_protocol_error(reason, %__MODULE__{} = state) do
    reason = sanitize_protocol_reason(reason)
    Logger.warning("D-Bus connection protocol stopped: #{inspect(reason)}")
    {:stop, {:shutdown, reason}, state}
  end

  defp handle_hello_reply(
         %Message{} = msg,
         rest,
         %__MODULE__{} = state
       ) do
    case hello_reply_result(msg) do
      {:ok, name} -> parse(rest, %{state | name: name, prev: <<>>})
      {:error, reason} -> stop_for_protocol_error(reason, state)
    end
  end

  defp parse_hello_reply(data, %__MODULE__{} = state) do
    case Message.parse(data) do
      {:ok, %Message{} = msg, rest} ->
        handle_hello_reply(msg, rest, state)

      nil ->
        # Incomplete message, store data for the next Hello reply receive.
        {:noreply, %{state | prev: data}, {:continue, :hello_reply}}

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
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
             :invalid_message_type
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
            {:ok, state}

          {from, pending} ->
            GenServer.reply(from, msg)
            {:ok, %{state | pending: pending}}
        end

      :error ->
        {:error, {:malformed_reply, :missing_reply_serial}}
    end
  end

  defp get_auth_id do
    {resp, 0} = System.cmd("id", ["-u"])

    resp
    |> String.trim()
    |> :binary.encode_hex()
  end
end
