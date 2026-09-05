defmodule Rebus.TestServer do
  @moduledoc false
  use GenServer
  use TypedStruct

  alias Rebus.Message

  def get_listen_addr(svr) when is_pid(svr) do
    GenServer.call(svr, :get_listen_addr)
  end

  def set_auto_hello(svr, enabled, send_name_acquired? \\ true)
      when is_pid(svr) and is_boolean(enabled) and is_boolean(send_name_acquired?) do
    GenServer.call(svr, {:set_auto_hello, enabled, send_name_acquired?})
  end

  def push(svr, %Message{} = msg) do
    GenServer.cast(svr, {:push, msg})
  end

  # Frames pushed at the client are stamped with the server's own serial.
  # These variants return that serial so a test can assert the `reply_serial`
  # of the frame the client sends back.
  def push_call(svr, %Message{} = msg) when is_pid(svr) do
    GenServer.call(svr, {:push_call, msg})
  end

  def push_call_with_fds(svr, %Message{} = msg, fds) when is_pid(svr) and is_list(fds) do
    GenServer.call(svr, {:push_call_with_fds, msg, fds})
  end

  def push_raw(svr, data) when is_binary(data) do
    GenServer.cast(svr, {:push_raw, data})
  end

  def push_raw_fragments(svr, data) when is_binary(data) do
    GenServer.call(svr, {:push_raw_fragments, data})
  end

  def push_raw_delayed_fragments(svr, fragments, delay)
      when is_list(fragments) and is_integer(delay) and delay >= 0 do
    GenServer.call(svr, {:push_raw_delayed_fragments, fragments, delay})
  end

  def push_with_fds(svr, %Message{} = msg, fds) when is_pid(svr) and is_list(fds) do
    GenServer.cast(svr, {:push_with_fds, msg, fds})
  end

  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  typedstruct enforce: true do
    field :svr_sock, :socket.socket()
    field :cli_sock, :socket.socket() | nil, default: nil
    field :handle, reference() | nil, default: nil
    field :prev, binary(), default: <<>>
    field :received_fds, [non_neg_integer()], default: []
    field :tap, pid()
    field :serial, non_neg_integer(), default: 1
    field :family, :inet | :inet6 | :local, default: :inet
    field :path, String.t() | nil, default: nil
    field :auth_response, binary(), default: "OK 30313233343536373839414243444546\r\n"
    field :auth_response_fragments, [binary()] | nil, default: nil
    field :auth_fragment_delay, non_neg_integer(), default: 0
    field :partial_auth, binary() | nil, default: nil
    field :close_after_begin, boolean(), default: false
    field :silent_auth, boolean(), default: false
    field :notify_auth, boolean(), default: false
    field :auto_hello, boolean(), default: true
    field :auto_hello_name_acquired?, boolean(), default: true
    field :unix_fd_response, binary(), default: "AGREE_UNIX_FD\r\n"
  end

  @impl true
  def init(opts) do
    family = opts[:family] || :inet
    path = opts[:path]

    case family do
      family when family in [:inet, :inet6] ->
        {:ok, sock} = :socket.open(family, :stream, :default)
        :ok = :socket.bind(sock, %{family: family, addr: loopback_address(family), port: 0})
        :ok = :socket.listen(sock, 5)

        {:ok,
         %__MODULE__{
           svr_sock: sock,
           tap: opts[:tap],
           family: family,
           auth_response: opts[:auth_response] || "OK 30313233343536373839414243444546\r\n",
           auth_response_fragments: opts[:auth_response_fragments],
           auth_fragment_delay: opts[:auth_fragment_delay] || 0,
           partial_auth: opts[:partial_auth],
           close_after_begin: opts[:close_after_begin] || false,
           silent_auth: opts[:silent_auth] || false,
           notify_auth: opts[:notify_auth] || false,
           auto_hello: Keyword.get(opts, :auto_hello, true),
           auto_hello_name_acquired?: Keyword.get(opts, :auto_hello_name_acquired?, true),
           unix_fd_response: Keyword.get(opts, :unix_fd_response, "AGREE_UNIX_FD\r\n")
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
           auth_response_fragments: opts[:auth_response_fragments],
           auth_fragment_delay: opts[:auth_fragment_delay] || 0,
           partial_auth: opts[:partial_auth],
           close_after_begin: opts[:close_after_begin] || false,
           silent_auth: opts[:silent_auth] || false,
           notify_auth: opts[:notify_auth] || false,
           auto_hello: Keyword.get(opts, :auto_hello, true),
           auto_hello_name_acquired?: Keyword.get(opts, :auto_hello_name_acquired?, true),
           unix_fd_response: Keyword.get(opts, :unix_fd_response, "AGREE_UNIX_FD\r\n")
         }, {:continue, :accept}}
    end
  end

  @impl true
  def handle_continue(:accept, %__MODULE__{cli_sock: nil} = state) do
    case :socket.accept(state.svr_sock, :nowait) do
      {:ok, cli} ->
        {:ok, "\0AUTH " <> _} = :socket.recv(cli)

        if state.notify_auth, do: send(state.tap, {self(), :auth_received})

        cond do
          state.silent_auth ->
            send(state.tap, {self(), :auth_received})
            {:noreply, %{state | cli_sock: cli}}

          is_binary(state.partial_auth) ->
            :ok = :socket.send(cli, state.partial_auth)
            send(state.tap, {self(), :auth_received})
            {:noreply, %{state | cli_sock: cli}}

          true ->
            send_auth_response(cli, state)

            case state.auth_response do
              <<"OK ", _::binary>> ->
                case receive_begin(cli, state) do
                  {:ok, "BEGIN \r\n"} ->
                    if state.close_after_begin do
                      :ok = :socket.close(cli)
                      {:noreply, %{state | cli_sock: nil}, {:continue, :accept}}
                    else
                      {:noreply, %{state | cli_sock: cli}, {:continue, :recv}}
                    end

                  {:error, :closed} ->
                    send(state.tap, {self(), :client_closed})
                    {:noreply, %{state | cli_sock: nil}, {:continue, :accept}}

                  _ ->
                    observe_client_close(cli, state)
                end

              _ ->
                observe_client_close(cli, state)
            end
        end

      {:select, {:select_info, :accept, handle}} ->
        {:noreply, %{state | handle: handle}}

      {:error, reason} ->
        {:stop, reason, state}
    end
  end

  def handle_continue(:recv, %__MODULE__{cli_sock: cli, handle: nil} = state) do
    recv_result =
      if state.family == :local,
        do: :socket.recvmsg(cli, 0, 256, [], :nowait),
        else: :socket.recv(cli, 0, [], :nowait)

    case recv_result do
      {:ok, %{iov: iov, ctrl: ctrl}} ->
        with {:ok, data} <- iodata_to_binary(iov),
             {:ok, fds} <- rights_fds(ctrl) do
          parse(state.prev <> data, %{state | prev: <<>>, received_fds: fds})
        else
          _ -> {:stop, :parse_error, state}
        end

      {:ok, data} ->
        parse(state.prev <> data, %__MODULE__{state | prev: <<>>})

      {:select, {:select_info, :recv, handle}} ->
        {:noreply, %{state | handle: handle}}

      {:select, {:select_info, :recvmsg, handle}} ->
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

  def handle_call({:set_auto_hello, enabled, send_name_acquired?}, _from, %__MODULE__{} = state) do
    {:reply, :ok, %{state | auto_hello: enabled, auto_hello_name_acquired?: send_name_acquired?}}
  end

  def handle_call({:push_call, %Message{} = msg}, _from, %__MODULE__{} = state) do
    {:reply, {:ok, state.serial}, send_message(msg, state)}
  end

  def handle_call({:push_call_with_fds, %Message{} = msg, fds}, _from, %__MODULE__{} = state) do
    {:reply, {:ok, state.serial}, send_message_with_fds(msg, fds, state)}
  end

  def handle_call({:push_raw_fragments, data}, _from, %__MODULE__{} = state) do
    for <<byte <- data>> do
      :ok = :socket.send(state.cli_sock, <<byte>>)
    end

    {:reply, :ok, state}
  end

  def handle_call(
        {:push_raw_delayed_fragments, fragments, delay},
        _from,
        %__MODULE__{} = state
      ) do
    fragments
    |> Enum.with_index()
    |> Enum.each(fn {fragment, index} ->
      :ok = :socket.send(state.cli_sock, fragment)

      if index < length(fragments) - 1 and delay > 0 do
        Process.sleep(delay)
      end
    end)

    {:reply, :ok, state}
  end

  @impl true
  def handle_cast({:push, %Message{} = msg}, %__MODULE__{} = state) do
    {:noreply, send_message(msg, state)}
  end

  def handle_cast({:push_raw, data}, %__MODULE__{} = state) do
    :ok = :socket.send(state.cli_sock, data)
    {:noreply, state}
  end

  def handle_cast({:push_with_fds, %Message{} = msg, fds}, %__MODULE__{} = state) do
    {:noreply, send_message_with_fds(msg, fds, state)}
  end

  @impl true
  def terminate(_reason, %__MODULE__{family: :local, path: <<0, _rest::binary>>} = _state) do
    :ok
  end

  def terminate(_reason, %__MODULE__{family: :local, path: path} = _state) when is_binary(path) do
    # Clean up Unix socket file
    File.rm(path)
    :ok
  end

  def terminate(_reason, _state), do: :ok

  defp send_message(%Message{} = msg, %__MODULE__{} = state) do
    {:ok, bin} = Message.encode(%{msg | serial: state.serial})
    :ok = :socket.send(state.cli_sock, bin)
    %{state | serial: state.serial + 1}
  end

  defp send_message_with_fds(%Message{} = msg, fds, %__MODULE__{} = state) do
    {:ok, bin} = Message.encode(%{msg | serial: state.serial})
    rights = for fd <- fds, into: <<>>, do: <<fd::native-signed-32>>

    :ok =
      :socket.sendmsg(
        state.cli_sock,
        %{
          iov: [IO.iodata_to_binary(bin)],
          ctrl: [%{level: :socket, type: :rights, data: rights}]
        },
        [],
        1_000
      )

    %{state | serial: state.serial + 1}
  end

  defp loopback_address(:inet), do: :loopback
  defp loopback_address(:inet6), do: {0, 0, 0, 0, 0, 0, 0, 1}

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

  defp send_auth_response(cli, %__MODULE__{} = state) do
    fragments = state.auth_response_fragments || [state.auth_response]

    fragments
    |> Enum.with_index()
    |> Enum.each(fn {fragment, index} ->
      :ok = :socket.send(cli, fragment)

      if index < length(fragments) - 1 and state.auth_fragment_delay > 0 do
        Process.sleep(state.auth_fragment_delay)
      end
    end)
  end

  defp receive_begin(cli, %__MODULE__{family: :local, unix_fd_response: response}) do
    case :socket.recv(cli, 0) do
      {:ok, "NEGOTIATE_UNIX_FD\r\n"} ->
        :ok = :socket.send(cli, response)
        :socket.recv(cli, 8)

      result ->
        result
    end
  end

  defp receive_begin(cli, _state), do: :socket.recv(cli, 8)

  defp parse(data, %__MODULE__{} = state) do
    case Message.parse(data) do
      {:ok, %Message{} = msg, rest} ->
        case Message.attach_unix_fds(msg, state.received_fds) do
          {:ok, msg} ->
            send(state.tap, {self(), msg})
            parse(rest, maybe_reply_hello(msg, %{state | received_fds: []}))

          {:error, _reason} ->
            Enum.each(state.received_fds, &Rebus.UnixFD.close/1)
            {:stop, :parse_error, state}
        end

      nil ->
        # Incomplete message, store data for next recv
        {:noreply, %{state | prev: data}, {:continue, :recv}}

      {:error, _reason} ->
        {:stop, :parse_error, state}
    end
  end

  defp maybe_reply_hello(
         %Message{type: :method_call, header_fields: %{member: "Hello"}, serial: serial},
         %__MODULE__{auto_hello: true, cli_sock: cli} = state
       ) do
    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        serial: state.serial,
        signature: "s",
        body: [":1.100"]
      )

    {:ok, encoded} = Message.encode(reply)
    :ok = :socket.send(cli, encoded)

    if state.auto_hello_name_acquired? do
      signal =
        Message.new!(:signal,
          sender: "org.freedesktop.DBus",
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "NameAcquired",
          destination: ":1.100",
          serial: state.serial + 1,
          signature: "s",
          body: [":1.100"]
        )

      {:ok, encoded} = Message.encode(signal)
      :ok = :socket.send(cli, encoded)
      %{state | serial: state.serial + 2}
    else
      %{state | serial: state.serial + 1}
    end
  end

  defp maybe_reply_hello(_msg, state), do: state

  defp iodata_to_binary(iodata) do
    {:ok, IO.iodata_to_binary(iodata)}
  rescue
    ArgumentError -> :error
  end

  defp rights_fds(ctrl) do
    fds =
      for %{level: :socket, type: :rights, data: data} <- ctrl,
          <<fd::native-signed-32 <- data>>,
          do: fd

    if Enum.all?(fds, &(&1 >= 0)), do: {:ok, fds}, else: :error
  end
end
