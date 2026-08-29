defmodule Rebus.TestServer do
  @moduledoc false
  use GenServer
  use TypedStruct

  alias Rebus.Message

  def get_listen_addr(svr) when is_pid(svr) do
    GenServer.call(svr, :get_listen_addr)
  end

  def push(svr, %Message{} = msg) do
    GenServer.cast(svr, {:push, msg})
  end

  def push_raw(svr, data) when is_binary(data) do
    GenServer.cast(svr, {:push_raw, data})
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  typedstruct enforce: true do
    field :svr_sock, :socket.socket()
    field :cli_sock, :socket.socket() | nil, default: nil
    field :handle, reference() | nil, default: nil
    field :prev, binary(), default: <<>>
    field :tap, pid()
    field :serial, non_neg_integer(), default: 1
    field :family, :inet | :local, default: :inet
    field :path, String.t() | nil, default: nil
    field :auth_response, binary(), default: "OK 30313233343536373839414243444546\r\n"
    field :close_after_begin, boolean(), default: false
  end

  @impl true
  def init(opts) do
    family = opts[:family] || :inet
    path = opts[:path]

    case family do
      :inet ->
        {:ok, sock} = :socket.open(:inet, :stream, :default)
        :ok = :socket.bind(sock, %{family: :inet, addr: :loopback, port: 0})
        :ok = :socket.listen(sock, 5)

        {:ok,
         %__MODULE__{
           svr_sock: sock,
           tap: opts[:tap],
           family: family,
           auth_response: opts[:auth_response] || "OK 30313233343536373839414243444546\r\n",
           close_after_begin: opts[:close_after_begin] || false
         }, {:continue, :accept}}

      :local ->
        {:ok, sock} = :socket.open(:local, :stream, :default)
        # For Unix sockets, the path should be passed as binary
        :ok = :socket.bind(sock, %{family: :local, path: path})
        :ok = :socket.listen(sock, 5)

        {:ok,
         %__MODULE__{
           svr_sock: sock,
           tap: opts[:tap],
           family: family,
           path: path,
           auth_response: opts[:auth_response] || "OK 30313233343536373839414243444546\r\n",
           close_after_begin: opts[:close_after_begin] || false
         }, {:continue, :accept}}
    end
  end

  @impl true
  def handle_continue(:accept, %__MODULE__{cli_sock: nil} = state) do
    case :socket.accept(state.svr_sock, :nowait) do
      {:ok, cli} ->
        {:ok, "\0AUTH " <> _} = :socket.recv(cli)
        :ok = :socket.send(cli, state.auth_response)

        case state.auth_response do
          <<"OK ", _::binary>> ->
            {:ok, "BEGIN \r\n"} = :socket.recv(cli, 8)

            if state.close_after_begin do
              :ok = :socket.close(cli)
              {:noreply, %{state | cli_sock: nil}, {:continue, :accept}}
            else
              {:noreply, %{state | cli_sock: cli}, {:continue, :recv}}
            end

          _ ->
            observe_client_close(cli, state)
        end

      {:select, {:select_info, :accept, handle}} ->
        {:noreply, %{state | handle: handle}}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  def handle_continue(:recv, %__MODULE__{cli_sock: cli, handle: nil} = state) do
    case :socket.recv(cli, 0, [], :nowait) do
      {:ok, data} ->
        parse(state.prev <> data, %__MODULE__{state | prev: <<>>})

      {:select, {:select_info, :recv, handle}} ->
        {:noreply, %{state | handle: handle}}

      {:error, :closed} ->
        {:stop, :normal, state}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  @impl true
  def handle_info({:"$socket", _, :select, h}, %__MODULE__{handle: h} = state) do
    action = if state.cli_sock == nil, do: :accept, else: :recv

    {:noreply, %{state | handle: nil}, {:continue, action}}
  end

  @impl true
  def handle_call(:get_listen_addr, _from, %__MODULE__{} = state) do
    {:reply, :socket.sockname(state.svr_sock), state}
  end

  @impl true
  def handle_cast({:push, %Message{} = msg}, %__MODULE__{} = state) do
    {:ok, bin} = Rebus.Message.encode(%{msg | serial: state.serial})
    :ok = :socket.send(state.cli_sock, bin)
    {:noreply, %{state | serial: state.serial + 1}}
  end

  def handle_cast({:push_raw, data}, %__MODULE__{} = state) do
    :ok = :socket.send(state.cli_sock, data)
    {:noreply, state}
  end

  @impl true
  def terminate(_reason, %__MODULE__{family: :local, path: path} = _state) when is_binary(path) do
    # Clean up Unix socket file
    File.rm(path)
    :ok
  end

  def terminate(_reason, _state), do: :ok

  defp observe_client_close(cli, %__MODULE__{} = state) do
    outcome =
      case :socket.recv(cli, 0, [], 1_000) do
        {:error, :closed} -> :client_closed
        result -> {:client_close_outcome, result}
      end

    _ = :socket.close(cli)
    send(state.tap, {self(), outcome})
    {:noreply, %{state | cli_sock: nil, handle: nil, prev: <<>>}, {:continue, :accept}}
  end

  defp parse(data, %__MODULE__{} = state) do
    case Message.parse(data) do
      {:ok, %Message{} = msg, rest} ->
        send(state.tap, {self(), msg})
        parse(rest, state)

      nil ->
        # Incomplete message, store data for next recv
        {:noreply, %{state | prev: data}, {:continue, :recv}}

      {:error, _reason} ->
        {:stop, :parse_error, state}
    end
  end
end
