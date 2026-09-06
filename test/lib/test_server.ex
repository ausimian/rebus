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
    # Without this, a supervised shutdown kills the server outright and
    # `terminate/2` never unlinks the socket path.
    Process.flag(:trap_exit, true)

    family = opts[:family] || :inet
    path = opts[:path]

    {:ok, sock} = listen(family, path)

    {:ok, new_state(sock, family, path, opts), {:continue, :accept}}
  end

  defp listen(family, _path) when family in [:inet, :inet6] do
    {:ok, sock} = :socket.open(family, :stream, :default)
    :ok = :socket.bind(sock, %{family: family, addr: loopback_address(family), port: 0})
    :ok = :socket.listen(sock, 5)

    {:ok, sock}
  end

  defp listen(:local, path) do
    {:ok, sock} = :socket.open(:local, :stream, :default)
    # For Unix sockets, the path should be passed as binary
    :ok = :socket.bind(sock, %{family: :local, path: path})
    :ok = :socket.listen(sock, 5)

    {:ok, sock}
  end

  defp new_state(sock, family, path, opts) do
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
    }
  end

  @impl true
  def handle_continue(:accept, %__MODULE__{cli_sock: nil} = state) do
    case :socket.accept(state.svr_sock, :nowait) do
      {:ok, cli} ->
        await_auth(cli, state)

      {:select, {:select_info, :accept, handle}} ->
        {:noreply, %{state | handle: handle}}

      # No client was ever established here, so there is nothing to report to
      # the tap; a peer that connected and reset before the accept completed
      # just means waiting for the next one. `:closed` is deliberately not in
      # this list: at accept it says the listening socket itself is gone, and
      # retrying that would spin without ever reaching the mailbox.
      {:error, reason} when reason in [:econnaborted, :econnreset] ->
        {:noreply, state, {:continue, :accept}}

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

      # Past BEGIN, so there is nothing left to do but end the session; see
      # `peer_gone?/1` for what a gone peer means here.
      {:error, reason} ->
        session_error(reason, state)
    end
  end

  defp await_auth(cli, %__MODULE__{} = state) do
    case :socket.recv(cli) do
      {:ok, "\0AUTH " <> _} ->
        if state.notify_auth, do: send(state.tap, {self(), :auth_received})

        authenticate(cli, state)

      {:ok, other} ->
        {:stop, {:unexpected_auth, other}, state}

      {:error, reason} ->
        handshake_error(cli, reason, state)
    end
  end

  defp authenticate(cli, %__MODULE__{silent_auth: true} = state) do
    send(state.tap, {self(), :auth_received})
    {:noreply, %{state | cli_sock: cli}}
  end

  defp authenticate(cli, %__MODULE__{partial_auth: partial_auth} = state)
       when is_binary(partial_auth) do
    case :socket.send(cli, partial_auth) do
      :ok ->
        send(state.tap, {self(), :auth_received})
        {:noreply, %{state | cli_sock: cli}}

      {:error, reason} ->
        handshake_error(cli, reason, state)
    end
  end

  defp authenticate(cli, %__MODULE__{} = state) do
    case send_auth_response(cli, state) do
      :ok ->
        case state.auth_response do
          <<"OK ", _::binary>> -> await_begin(cli, state)
          _ -> observe_client_close(cli, state)
        end

      {:error, reason} ->
        handshake_error(cli, reason, state)
    end
  end

  defp await_begin(cli, %__MODULE__{} = state) do
    case receive_begin(cli, state) do
      {:ok, "BEGIN \r\n"} ->
        begin_session(cli, state)

      {:error, reason} ->
        # A gone peer is not a failure of the handshake, it is the end of it.
        # Anything else is still worth observing on the way out.
        if peer_gone?(reason),
          do: handshake_error(cli, reason, state),
          else: observe_client_close(cli, state)

      _ ->
        observe_client_close(cli, state)
    end
  end

  defp begin_session(cli, %__MODULE__{close_after_begin: true} = state) do
    _ = :socket.close(cli)
    {:noreply, %{state | cli_sock: nil}, {:continue, :accept}}
  end

  defp begin_session(cli, %__MODULE__{} = state) do
    {:noreply, %{state | cli_sock: cli}, {:continue, :recv}}
  end

  @impl true
  def handle_info({:"$socket", _, :select, h}, %__MODULE__{handle: h} = state) do
    action = if state.cli_sock == nil, do: :accept, else: :recv

    {:noreply, %{state | handle: nil}, {:continue, action}}
  end

  # Trapping exits means a linked process other than the parent would deliver
  # an EXIT message here; ignore it as the default implementation would.
  def handle_info(_message, %__MODULE__{} = state), do: {:noreply, state}

  @impl true
  def handle_call(:get_listen_addr, _from, %__MODULE__{} = state) do
    {:reply, :socket.sockname(state.svr_sock), state}
  end

  def handle_call({:set_auto_hello, enabled, send_name_acquired?}, _from, %__MODULE__{} = state) do
    {:reply, :ok, %{state | auto_hello: enabled, auto_hello_name_acquired?: send_name_acquired?}}
  end

  def handle_call({:push_call, %Message{} = msg}, _from, %__MODULE__{} = state) do
    # The reply carries the serial the frame was stamped with, not the next one.
    reply = {:ok, state.serial}

    case send_message(msg, state) do
      {:ok, state} -> {:reply, reply, state}
      {:error, reason} -> session_error_reply(reason, reply, state)
    end
  end

  def handle_call({:push_call_with_fds, %Message{} = msg, fds}, _from, %__MODULE__{} = state) do
    reply = {:ok, state.serial}

    case send_message_with_fds(msg, fds, state) do
      {:ok, state} -> {:reply, reply, state}
      {:error, reason} -> session_error_reply(reason, reply, state)
    end
  end

  def handle_call({:push_raw_fragments, data}, _from, %__MODULE__{} = state) do
    case send_fragments(state.cli_sock, for(<<byte <- data>>, do: <<byte>>), 0) do
      :ok -> {:reply, :ok, state}
      {:error, reason} -> session_error_reply(reason, :ok, state)
    end
  end

  def handle_call(
        {:push_raw_delayed_fragments, fragments, delay},
        _from,
        %__MODULE__{} = state
      ) do
    case send_fragments(state.cli_sock, fragments, delay) do
      :ok -> {:reply, :ok, state}
      {:error, reason} -> session_error_reply(reason, :ok, state)
    end
  end

  @impl true
  def handle_cast({:push, %Message{} = msg}, %__MODULE__{} = state) do
    case send_message(msg, state) do
      {:ok, state} -> {:noreply, state}
      {:error, reason} -> session_error(reason, state)
    end
  end

  def handle_cast({:push_raw, data}, %__MODULE__{} = state) do
    case :socket.send(state.cli_sock, data) do
      :ok -> {:noreply, state}
      {:error, reason} -> session_error(reason, state)
    end
  end

  def handle_cast({:push_with_fds, %Message{} = msg, fds}, %__MODULE__{} = state) do
    case send_message_with_fds(msg, fds, state) do
      {:ok, state} -> {:noreply, state}
      {:error, reason} -> session_error(reason, state)
    end
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

    case :socket.send(state.cli_sock, bin) do
      :ok -> {:ok, %{state | serial: state.serial + 1}}
      {:error, _reason} = error -> error
    end
  end

  defp send_message_with_fds(%Message{} = msg, fds, %__MODULE__{} = state) do
    {:ok, bin} = Message.encode(%{msg | serial: state.serial})
    rights = for fd <- fds, into: <<>>, do: <<fd::native-signed-32>>

    result =
      :socket.sendmsg(
        state.cli_sock,
        %{
          iov: [IO.iodata_to_binary(bin)],
          ctrl: [%{level: :socket, type: :rights, data: rights}]
        },
        [],
        1_000
      )

    case result do
      :ok -> {:ok, %{state | serial: state.serial + 1}}
      {:error, _reason} = error -> error
    end
  end

  defp loopback_address(:inet), do: :loopback
  defp loopback_address(:inet6), do: {0, 0, 0, 0, 0, 0, 0, 1}

  defp observe_client_close(cli, %__MODULE__{} = state) do
    outcome =
      case :socket.recv(cli, 0, [], 1_000) do
        {:error, reason} = result ->
          if peer_gone?(reason), do: :client_closed, else: {:client_close_outcome, result}

        result ->
          {:client_close_outcome, result}
      end

    _ = :socket.close(cli)
    send(state.tap, {self(), outcome})
    {:noreply, %{state | cli_sock: nil, handle: nil, prev: <<>>}, {:continue, :accept}}
  end

  defp send_auth_response(cli, %__MODULE__{} = state) do
    fragments = state.auth_response_fragments || [state.auth_response]

    send_fragments(cli, fragments, state.auth_fragment_delay)
  end

  # Writes each fragment in turn, stopping at the first failure so that a
  # vanished peer surfaces as `{:error, reason}` for the caller to act on.
  defp send_fragments(sock, fragments, delay) do
    last = length(fragments) - 1

    fragments
    |> Enum.with_index()
    |> Enum.reduce_while(:ok, fn {fragment, index}, :ok ->
      send_fragment(sock, fragment, if(index < last, do: delay, else: 0))
    end)
  end

  defp send_fragment(sock, fragment, delay) do
    case :socket.send(sock, fragment) do
      :ok ->
        if delay > 0, do: Process.sleep(delay)
        {:cont, :ok}

      {:error, _reason} = error ->
        {:halt, error}
    end
  end

  defp receive_begin(cli, %__MODULE__{family: :local, unix_fd_response: response}) do
    case :socket.recv(cli, 0) do
      {:ok, "NEGOTIATE_UNIX_FD\r\n"} ->
        case :socket.send(cli, response) do
          :ok -> :socket.recv(cli, 8)
          {:error, _reason} = error -> error
        end

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
            deliver_frame(msg, rest, %{state | received_fds: []})

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

  # The reply goes out before the tap is notified: `await_hello/1` returning
  # has to imply the reply is already on the wire, or a test that closes on it
  # races the send. The tap is told either way, even when that send failed: a
  # test blocked in `await_hello/1` must not hang because the peer vanished, so
  # it gets the frame and the server then stops normally underneath it.
  defp deliver_frame(%Message{} = msg, rest, %__MODULE__{} = state) do
    result = maybe_reply_hello(msg, state)
    send(state.tap, {self(), msg})

    case result do
      {:ok, state} -> parse(rest, state)
      {:error, reason} -> session_error(reason, state)
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

    case :socket.send(cli, encoded) do
      :ok -> maybe_send_name_acquired(cli, state)
      {:error, _reason} = error -> error
    end
  end

  defp maybe_reply_hello(_msg, state), do: {:ok, state}

  defp maybe_send_name_acquired(cli, %__MODULE__{auto_hello_name_acquired?: true} = state) do
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

    case :socket.send(cli, encoded) do
      :ok -> {:ok, %{state | serial: state.serial + 2}}
      {:error, _reason} = error -> error
    end
  end

  defp maybe_send_name_acquired(_cli, %__MODULE__{} = state) do
    {:ok, %{state | serial: state.serial + 1}}
  end

  # The single meaning of a vanished peer in this module, once a client exists.
  # `:closed` (a clean FIN), `:econnreset` (a peer that closed while frames it
  # never read were still queued) and `:epipe` (a write to a connection the
  # peer has already torn down) all say the same thing: the client is simply
  # gone. Nothing here can tell them apart - which of the three arrives is an
  # artefact of timing - so every socket call on a client treats them alike.
  # Before BEGIN that means reporting `:client_closed` to the tap and going
  # back to accepting; after BEGIN it means stopping normally rather than
  # logging a crash report for it. The accept call has its own, narrower rule
  # above: no client exists yet, and `:closed` there means the listener.
  #
  # On a stream socket the reason can also arrive wrapped as `{reason, rest}`,
  # carrying the tail that was left unsent or unread when a partially completed
  # operation was interrupted: a binary for `send` and `recv`, an iovec for
  # `sendmsg`. That is the common shape for a peer that vanishes mid-write, and
  # the wrapper says nothing about the peer beyond what the reason inside it
  # already says, so it is unwrapped and judged the same.
  defp peer_gone?({reason, rest}) when is_binary(rest) or is_list(rest),
    do: peer_gone?(reason)

  defp peer_gone?(reason), do: reason in [:closed, :econnreset, :epipe]

  # A socket error during the handshake: a gone peer never became a session, so
  # tell the tap and wait for the next one. Any other error is real and stops
  # the server with its own reason.
  defp handshake_error(cli, reason, %__MODULE__{} = state) do
    _ = :socket.close(cli)

    if peer_gone?(reason) do
      send(state.tap, {self(), :client_closed})
      {:noreply, %{state | cli_sock: nil, handle: nil, prev: <<>>}, {:continue, :accept}}
    else
      {:stop, reason, state}
    end
  end

  # A socket error once the session is running: the connection is all this
  # server is doing, so a gone peer ends it normally.
  defp session_error(reason, %__MODULE__{} = state) do
    if peer_gone?(reason), do: {:stop, :normal, state}, else: {:stop, reason, state}
  end

  # As `session_error/2`, for a `handle_call` that must still answer its caller:
  # it replies on the way out so the caller gets its answer rather than exiting
  # with the server.
  defp session_error_reply(reason, reply, %__MODULE__{} = state) do
    if peer_gone?(reason),
      do: {:stop, :normal, reply, state},
      else: {:stop, reason, reply, state}
  end

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
