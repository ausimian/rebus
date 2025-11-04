defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.SignalHandler
  alias Rebus.Message

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
    {:ok, sock} = :socket.open(family, :stream, :default)
    :ok = :socket.connect(sock, addr)

    auth = "AUTH EXTERNAL #{get_auth_id()}\r\n"
    :ok = :socket.send(sock, [0, auth])

    case :socket.recv(sock, 0) do
      {:ok, <<"OK ", guid::binary-size(32), "\r\n", rest::binary>>} ->
        :ok = :socket.send(sock, "BEGIN \r\n")
        {:ok, %__MODULE__{sock: sock, guid: guid, prev: rest}, {:continue, :hello}}

      {:ok, _} ->
        {:error, :auth_failed}

      error ->
        error
    end
  end

  @impl true
  def handle_info({:"$socket", s, :select, h}, %__MODULE__{sock: s, rref: h} = state) do
    {:noreply, %{state | rref: nil}, {:continue, :recv}}
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

  @impl true

  def handle_continue(:hello, %__MODULE__{} = state) do
    # Send the Hello method call
    {:ok, method} =
      Message.new(:method_call,
        path: "/",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "Hello"
      )

    {:ok, bin} = Message.encode(%{method | serial: state.serial})
    :ok = :socket.send(state.sock, bin)
    {:noreply, %{state | serial: state.serial + 1}, {:continue, :hello_reply}}
  end

  def handle_continue(:hello_reply, %__MODULE__{} = state) do
    # Wait for the Hello reply
    case :socket.recv(state.sock, 0, [], 5_000) do
      {:ok, data} ->
        case Message.parse(state.prev <> data) do
          {:ok, %Message{type: :method_return, header_fields: %{reply_serial: 1}} = msg, rest} ->
            {:noreply, %{state | name: hd(msg.body), prev: rest}, {:continue, :recv}}

          nil ->
            # Incomplete message, store data for next recv
            {:noreply, %{state | prev: state.prev <> data}, {:continue, :hello_reply}}

          error ->
            {:stop, error, state}
        end

      error ->
        {:stop, error, state}
    end
  end

  def handle_continue(:recv, %__MODULE__{rref: nil} = state) do
    case :socket.recv(state.sock, 0, [], :nowait) do
      {:ok, data} ->
        parse(state.prev <> data, %__MODULE__{state | prev: <<>>})

      {:select, {:select_info, :recv, handle}} ->
        {:noreply, %{state | rref: handle}}

      {:error, reason} ->
        {:stop, {:shutdown, reason}, state}
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
      {:ok, %Message{header_fields: %{reply_serial: 1}} = msg, rest} when is_nil(state.name) ->
        parse(rest, %{state | name: hd(msg.body)})

      {:ok, %Message{type: type} = msg, rest} when type in [:method_return, :error] ->
        parse(rest, reply(msg, state))

      {:ok, %Message{type: :signal} = msg, rest} ->
        parse(rest, notify(msg, state))

      nil ->
        # Incomplete message, store data for next recv
        {:noreply, %{state | prev: data}, {:continue, :recv}}

      {:error, reason} ->
        {:stop, reason}
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
    case Map.pop(state.pending, msg.header_fields.reply_serial) do
      {nil, _pending} ->
        state

      {from, pending} ->
        GenServer.reply(from, msg)
        %{state | pending: pending}
    end
  end

  defp get_auth_id do
    {resp, 0} = System.cmd("id", ["-u"])

    resp
    |> String.trim()
    |> :binary.encode_hex()
  end
end
