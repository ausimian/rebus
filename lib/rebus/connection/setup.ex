defmodule Rebus.Connection.Setup do
  @moduledoc false

  # Everything a D-Bus connection does before it is established: resolving the
  # authentication identity, connecting the socket, driving
  # `Rebus.Connection.Handshake`, and — on a message-bus connection — sending
  # `org.freedesktop.DBus.Hello` and correlating its reply. It also owns the
  # connect-waiter lifecycle, the one-shot handshake between the process that
  # asked for a connection and the connection that eventually accepts it.
  #
  # Like `Rebus.Connection.Writer` this is not a pure data structure: it reads
  # and writes the socket, monitors the waiter and reads the clock. Unlike the
  # writer it works on the connection struct directly, because setup touches
  # most of it. What it never does is return a `GenServer` reply: every
  # function answers with a `t:Rebus.Connection.Dispatch.result/0`, which the
  # connection maps to a callback return in one place.

  alias Rebus.Connection
  alias Rebus.Connection.Dispatch
  alias Rebus.Connection.Handshake
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.SocketError
  alias Rebus.Connection.Writer
  alias Rebus.Message
  alias Rebus.WireValue

  require Logger

  @max_read_attempts 1

  @doc false
  @spec setup(Connection.t(), map()) :: Dispatch.result()
  def setup(%Connection{} = state, addr) do
    # Nothing is authenticated or connected on behalf of a process that has
    # already gone. The waiter is checked first, so an abandoned connect still
    # reports `:caller_gone` whether or not it also named an owner.
    cond do
      connect_waiter_gone?(state) -> {:shutdown, :caller_gone, state}
      owner_gone?(state) -> owner_down(state)
      true -> state |> initialize(addr) |> setup_result(state)
    end
  end

  # The waiter is live here, because `connect_waiter_gone?/1` was asked first,
  # so it is told why the connection is stopping instead of being left to read
  # the reason off its monitor. `Rebus.Connector.Supervised` can only monitor
  # the connection once `DynamicSupervisor.start_child/2` has returned, by
  # which time a connection that stops in setup may already be gone and the
  # monitor fires `:noproc`. The notification is in the waiter's mailbox
  # first, so `connect/2` reports `:owner_down` whatever the scheduler did.
  # The reason is not routed through `setup_result/2`: a waiter that dies
  # between the two checks must not turn this stop into `:caller_gone`.
  defp owner_down(%Connection{} = state) do
    notify_connect_waiter(state.connect_waiter, {:error, :owner_down})
    {:shutdown, :owner_down, state}
  end

  defp setup_result({:ok, initialized, {:continue, continuation}}, %Connection{}) do
    if is_nil(initialized.connect_waiter) do
      {:continue, continuation, initialized}
    else
      notify_connect_waiter(initialized.connect_waiter, {:ok, self()})
      {:ok, initialized}
    end
  end

  defp setup_result({:stop, reason}, %Connection{} = state) do
    if connect_waiter_alive?(state) do
      notify_connect_waiter(state.connect_waiter, {:error, reason})
      {:shutdown, reason, state}
    else
      {:shutdown, :caller_gone, state}
    end
  end

  # Send the Hello method call.
  @doc false
  @spec hello(Connection.t()) :: Dispatch.result()
  def hello(%Connection{} = state) do
    with {:ok, method} <-
           Message.new(:method_call,
             path: "/",
             interface: "org.freedesktop.DBus",
             destination: "org.freedesktop.DBus",
             member: "Hello"
           ),
         {:ok, bin} <- Message.encode(%{method | serial: Writer.serial(state.writer)}) do
      hello_sent(transport(state).send(state.sock, bin, [], state.write_timeout), state)
    else
      {:error, reason} -> {:protocol_error, reason, state}
    end
  end

  defp hello_sent(:ok, %Connection{} = state) do
    {:continue, :hello_reply_buffer,
     %{
       state
       | hello_serial: Writer.serial(state.writer),
         writer: Writer.consume_serial(state.writer)
     }}
  end

  defp hello_sent({:error, reason}, %Connection{} = state), do: {:transport_error, reason, state}
  defp hello_sent(_unexpected, %Connection{} = state), do: {:transport_error, :send_failed, state}

  # A peer-to-peer endpoint has no bus driver, so there is no Hello to send or
  # correlate. The connection is established as soon as the handshake finishes,
  # with no unique name, and joins the ordinary receive loop. Authentication may
  # already have read peer frames alongside its final response; those buffered
  # bytes are ordinary inbound traffic here.
  @doc false
  @spec established(Connection.t()) :: Dispatch.result()
  def established(%Connection{} = state) do
    case establish_connection(%{state | established?: true}) do
      {:ok, established} -> Dispatch.process_inbound(established, :recv)
      {:error, reason} -> {:shutdown, reason, state}
    end
  end

  # Authentication may have read D-Bus bytes along with its final response.
  @doc false
  @spec hello_reply_buffer(Connection.t()) :: Dispatch.result()
  def hello_reply_buffer(%Connection{} = state) do
    Dispatch.process_inbound(state, {:hello_reply, read_deadline(state.read_timeout)})
  end

  @doc false
  @spec hello_reply(Connection.t()) :: Dispatch.result()
  def hello_reply(%Connection{} = state),
    do: receive_hello_reply(state, read_deadline(state.read_timeout))

  @doc false
  @spec hello_reply(Connection.t(), integer()) :: Dispatch.result()
  def hello_reply(%Connection{} = state, deadline), do: receive_hello_reply(state, deadline)

  defp receive_hello_reply(%Connection{} = state, deadline) do
    case remaining_timeout(deadline, state.read_timeout) do
      :expired ->
        {:protocol_error, :read_timeout, state}

      {:ok, timeout} ->
        receive_hello_reply(state, deadline, timeout)
    end
  end

  defp receive_hello_reply(%Connection{} = state, deadline, timeout) do
    if state.unix_fd_transport? do
      receive_hello_reply_recvmsg(state, deadline, timeout)
    else
      case transport(state).recv(state.sock, 0, [], timeout) do
        {:ok, data} ->
          continue_hello_reply(data, state, deadline)

        {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
          continue_hello_reply(data, state, deadline)

        {:error, :timeout} ->
          {:protocol_error, :read_timeout, state}

        {:error, {:timeout, _data}} ->
          {:protocol_error, :read_timeout, state}

        {:error, reason} ->
          {:transport_error, reason, state}
      end
    end
  end

  # After local transport negotiation, every peer read—including the initial
  # Hello reply—must observe SCM_RIGHTS. A plain recv/4 here could discard
  # ancillary metadata outside the single close-or-deliver ownership path.
  defp receive_hello_reply_recvmsg(%Connection{} = state, deadline, timeout) do
    case transport(state).recvmsg(
           state.sock,
           Inbound.receive_size(state.inbound, Dispatch.max_read_chunk()),
           recvmsg_control_size(state),
           [],
           timeout
         ) do
      {:ok, message} when is_map(message) ->
        continue_hello_reply_recvmsg(message, state, deadline)

      {:error, {:timeout, message}} when is_map(message) ->
        continue_hello_reply_recvmsg(message, state, deadline)

      {:error, :timeout} ->
        {:protocol_error, :read_timeout, state}

      {:error, {:timeout, _message}} ->
        {:protocol_error, :read_timeout, state}

      {:error, reason} ->
        {:transport_error, reason, state}

      _unexpected ->
        {:transport_error, :receive_failed, state}
    end
  end

  defp recvmsg_control_size(%Connection{}), do: Dispatch.max_unix_fd_control_size()

  defp continue_hello_reply(data, %Connection{} = state, deadline) do
    case Dispatch.append_inbound(data, state, {:hello_reply, deadline}) do
      {:continue, {:hello_reply, _deadline}, %Connection{} = state} ->
        receive_hello_reply(state, deadline)

      result ->
        result
    end
  end

  defp continue_hello_reply_recvmsg(message, %Connection{} = state, deadline) do
    case Dispatch.append_recvmsg(message, state, {:hello_reply, deadline}) do
      {:continue, {:hello_reply, _deadline}, %Connection{} = state} ->
        receive_hello_reply(state, deadline)

      result ->
        result
    end
  end

  defp initialize(%Connection{aggregate_setup_timeout?: true} = state, addr) do
    sock = state.sock
    deadline = read_deadline(state.setup_timeout)

    with {:ok, auth_id} <- aggregate_setup_auth_id(state, deadline),
         {:ok, connect_timeout} <- remaining_setup_timeout(deadline, state.setup_timeout),
         :ok <- connect_socket(transport(state), sock, addr, connect_timeout),
         {:ok, %{guid: guid, unix_fd_negotiated?: unix_fd_negotiated?, rest: rest}} <-
           Handshake.run(
             sock,
             auth_id,
             deadline,
             state.setup_timeout,
             handshake_options(state)
           ) do
      initialized_connection(%{state | unix_fd_negotiated?: unix_fd_negotiated?}, guid, rest)
    else
      {:error, reason} -> stop_and_close(transport(state), sock, reason)
    end
  end

  defp initialize(%Connection{} = state, addr) do
    sock = state.sock

    with {:ok, auth_id} <- setup_auth_id(state, state.setup_timeout),
         :ok <- connect_socket(transport(state), sock, addr, state.setup_timeout),
         deadline = read_deadline(state.setup_timeout),
         {:ok, %{guid: guid, unix_fd_negotiated?: unix_fd_negotiated?, rest: rest}} <-
           Handshake.run(
             sock,
             auth_id,
             deadline,
             state.setup_timeout,
             handshake_options(state)
           ) do
      initialized_connection(%{state | unix_fd_negotiated?: unix_fd_negotiated?}, guid, rest)
    else
      {:error, reason} -> stop_and_close(transport(state), sock, reason)
    end
  end

  defp handshake_options(%Connection{impl: impl} = state) do
    %Handshake.Options{
      transport: impl.transport,
      identity: impl.identity,
      write_timeout: state.write_timeout,
      allow_anonymous?: state.allow_anonymous?,
      unix_fd_transport?: state.unix_fd_transport?,
      expected_guid: state.expected_guid
    }
  end

  defp initialized_connection(state, guid, rest) do
    {:ok,
     %{
       state
       | guid: guid,
         inbound: Inbound.new(rest)
     }, {:continue, continuation(state)}}
  end

  @doc false
  @spec continuation(Connection.t()) :: :established | :hello
  def continuation(%Connection{bus?: false}), do: :established
  def continuation(%Connection{}), do: :hello

  defp aggregate_setup_auth_id(%Connection{precomputed_auth_id: auth_id}, _deadline)
       when is_binary(auth_id),
       do: {:ok, auth_id}

  defp aggregate_setup_auth_id(%Connection{} = state, deadline) do
    with {:ok, auth_id_timeout} <- remaining_setup_timeout(deadline, state.setup_timeout) do
      Handshake.get_auth_id(auth_id_timeout, state.impl.identity)
    end
  end

  defp setup_auth_id(%Connection{precomputed_auth_id: auth_id}, _timeout) when is_binary(auth_id),
    do: {:ok, auth_id}

  defp setup_auth_id(%Connection{} = state, timeout),
    do: Handshake.get_auth_id(timeout, state.impl.identity)

  defp notify_connect_waiter({pid, ref}, result) when is_pid(pid) and is_reference(ref),
    do: send(pid, {ref, result})

  defp notify_connect_waiter(nil, _result), do: :ok

  @doc false
  @spec establish_connection(Connection.t()) ::
          {:ok, Connection.t()} | {:error, :caller_gone | :owner_down}
  def establish_connection(
        %Connection{connect_waiter: {pid, connect_ref}, connect_waiter_monitor: monitor_ref} =
          state
      ) do
    # The acknowledgement below is the ownership-transfer boundary. The queued
    # monitor events are read first, and the acknowledgement is sent before
    # the waiter's monitor is released: a caller that dies after this send
    # owns the normal established-connection lifecycle, while a prior death
    # wins. An owner that died while setup was blocked on the socket is a
    # prior death too, so its caller is told the connection failed rather than
    # handed a PID that stops on the next pass through the loop.
    cond do
      connect_waiter_down?(pid, monitor_ref) -> {:error, :caller_gone}
      owner_gone?(state) -> {:error, :owner_down}
      true -> accept_connect_waiter(state, pid, connect_ref)
    end
  end

  # A connection started without a connect waiter has no one to report to, but
  # a dead owner still ends it here rather than on the next pass through the
  # receive loop.
  def establish_connection(%Connection{} = state) do
    if owner_gone?(state), do: {:error, :owner_down}, else: {:ok, state}
  end

  defp connect_waiter_down?(pid, monitor_ref) do
    receive do
      {:DOWN, ^monitor_ref, :process, ^pid, _reason} -> true
    after
      0 -> false
    end
  end

  defp accept_connect_waiter(%Connection{} = state, pid, connect_ref) do
    send(pid, {connect_ref, :accepted})
    {:ok, release_connect_waiter(state)}
  end

  @doc false
  @spec monitor_connect_waiter({pid(), reference()} | nil) :: reference() | nil
  def monitor_connect_waiter({pid, _ref}) when is_pid(pid), do: Process.monitor(pid)
  def monitor_connect_waiter(nil), do: nil

  defp connect_waiter_alive?(%Connection{connect_waiter: nil}), do: true

  defp connect_waiter_alive?(%Connection{connect_waiter: {pid, _ref}}), do: Process.alive?(pid)

  defp connect_waiter_gone?(%Connection{connect_waiter: nil}), do: false

  defp connect_waiter_gone?(%Connection{
         connect_waiter: {pid, _ref},
         connect_waiter_monitor: monitor_ref
       })
       when is_reference(monitor_ref) do
    receive do
      {:DOWN, ^monitor_ref, :process, ^pid, _reason} -> true
    after
      0 -> not Process.alive?(pid)
    end
  end

  # The owner's monitor is never released, so an exit that happened while
  # setup was blocked on the socket is still queued here. Reading it costs a
  # mailbox scan and saves establishing a connection that is already doomed.
  # As with `connect_waiter_gone?/1`, an owner always carries its monitor:
  # `Connection.monitor_owner/1` installs one whenever the owner is a PID, so
  # there is no clause for an owner without a reference.
  defp owner_gone?(%Connection{owner: nil}), do: false

  defp owner_gone?(%Connection{owner: owner, owner_monitor: monitor_ref})
       when is_reference(monitor_ref) do
    receive do
      {:DOWN, ^monitor_ref, :process, ^owner, _reason} -> true
    after
      0 -> not Process.alive?(owner)
    end
  end

  defp release_connect_waiter(%Connection{connect_waiter_monitor: monitor_ref} = state)
       when is_reference(monitor_ref) do
    Process.demonitor(monitor_ref, [:flush])
    %{state | connect_waiter: nil, connect_waiter_monitor: nil, connect_accepted?: false}
  end

  defp release_connect_waiter(%Connection{} = state),
    do: %{state | connect_waiter: nil, connect_accepted?: false}

  defp stop_and_close(transport, sock, reason) do
    _ = transport.close(sock)
    {:stop, SocketError.normalize(reason)}
  end

  defp connect_socket(transport, sock, addr, timeout) do
    case transport.connect(sock, addr, timeout) do
      :ok -> :ok
      {:error, :timeout} -> {:error, :read_timeout}
      {:error, reason} -> {:error, reason}
    end
  end

  @doc false
  @spec unix_fd_transport_supported?(atom()) :: boolean()
  def unix_fd_transport_supported?(:local) do
    :os.type() in [{:unix, :linux}, {:unix, :darwin}] and
      function_exported?(:socket, :sendmsg, 4) and
      function_exported?(:socket, :recvmsg, 5)
  end

  def unix_fd_transport_supported?(_family), do: false

  @doc false
  @spec configure_receive_buffer(module(), :socket.socket()) :: :tuple | :scalar | :default
  def configure_receive_buffer(transport, sock) do
    # A zero-length receive returns the bytes currently available on every
    # supported OTP release. Keep the backing allocation independent of a
    # peer-declared D-Bus frame length. Some backends only accept the scalar
    # form, so failure to tune this hint must never make connections unavailable.
    case transport.setopt(sock, {:otp, :rcvbuf}, {@max_read_attempts, Dispatch.max_read_chunk()}) do
      :ok ->
        :tuple

      {:error, _reason} ->
        scalar_receive_buffer(transport, sock)

      _other ->
        default_receive_buffer()
    end
  end

  defp scalar_receive_buffer(transport, sock) do
    case transport.setopt(sock, {:otp, :rcvbuf}, Dispatch.max_read_chunk()) do
      :ok -> :scalar
      {:error, _reason} -> default_receive_buffer()
      _other -> default_receive_buffer()
    end
  end

  defp default_receive_buffer do
    Logger.warning("D-Bus connection is using OTP's default receive buffer")
    :default
  end

  @doc false
  @spec hello_reply_result(Message.t(), non_neg_integer()) ::
          {:ok, binary()} | {:error, term()}
  def hello_reply_result(
        %Message{
          type: :method_return,
          header_fields: %{reply_serial: hello_serial},
          body: [name | _]
        },
        hello_serial
      )
      when is_binary(name) do
    # Preserve compatibility with peers that include extra decoded values, but
    # retain only the validated unique-name result.
    if WireValue.valid_unique_name?(name),
      do: {:ok, :binary.copy(name)},
      else: {:error, {:hello_failed, :invalid_unique_name}}
  end

  def hello_reply_result(
        %Message{type: :method_return, header_fields: %{reply_serial: hello_serial}},
        hello_serial
      ) do
    {:error, {:hello_failed, :missing_unique_name}}
  end

  def hello_reply_result(
        %Message{type: :error, header_fields: %{reply_serial: hello_serial}} = msg,
        hello_serial
      ) do
    {:error, {:hello_failed, hello_error_reason(msg.header_fields)}}
  end

  def hello_reply_result(%Message{type: type}, _hello_serial) do
    {:error, {:unexpected_handshake_message, type}}
  end

  defp hello_error_reason(header_fields) do
    case Map.fetch(header_fields, :error_name) do
      :error ->
        :missing_error_name

      {:ok, error_name} ->
        if WireValue.valid_error_name?(error_name), do: error_name, else: :invalid_error_name
    end
  end

  @doc false
  @spec read_deadline(pos_integer()) :: integer()
  def read_deadline(timeout) when is_integer(timeout) and timeout > 0 do
    System.monotonic_time(:millisecond) + timeout
  end

  defp remaining_timeout(deadline, maximum) when is_integer(deadline) and maximum > 0 do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, min(remaining, maximum)}
      _ -> :expired
    end
  end

  defp remaining_setup_timeout(deadline, maximum) do
    case remaining_timeout(deadline, maximum) do
      {:ok, timeout} -> {:ok, timeout}
      :expired -> {:error, :read_timeout}
    end
  end

  defp transport(%Connection{impl: %{transport: transport}}), do: transport
end
