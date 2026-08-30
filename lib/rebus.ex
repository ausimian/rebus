defmodule Rebus do
  @moduledoc """
  An Elixir implementation of the D-Bus message protocol.

  Rebus provides a clean, Elixir-native interface for communicating over D-Bus,
  the inter-process communication (IPC) and remote procedure call (RPC) mechanism
  that is standard on Linux desktop systems.

  ## Overview

  D-Bus is a message bus system that allows multiple processes to communicate with
  each other in a structured way. Rebus implements the D-Bus wire protocol and provides
  an easy-to-use API for:

  - Connecting to D-Bus message buses (system and session buses)
  - Sending method calls and receiving replies
  - Emitting and receiving signals
  - Publishing and consuming D-Bus services

  ## Quick Start

      # Connect to the session bus
      {:ok, conn} = Rebus.connect(:session)

      message = Rebus.Message.new!(:method_call,
        path: "/org/freedesktop/DBus",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "ListNames"
      )

      %Rebus.Message{type: :method_return, body: [names]} = Rebus.call(conn, message)

      # Add a signal handler to receive all signals.
      case Rebus.add_signal_handler(conn) do
        ref when is_reference(ref) -> Rebus.delete_signal_handler(conn, ref)
        {:error, reason} -> {:error, reason}
      end

  ## Connection Types

  Rebus supports connecting to different types of D-Bus endpoints:

  - `:system` - Connects to the system bus using the address specified in
     application config (see below) or the `/run/dbus/system_bus_socket` by default.
  - `:session` - Connects to the session bus using the address specified in
     the `DBUS_SESSION_BUS_ADDRESS` environment variable.
  - `%{family: :local, path: path}` - Unix domain socket connection to a local D-Bus daemon
  - `%{family: :inet, addr: {ip, port}}` - TCP/IP connection to a remote D-Bus daemon

  ## Configuration

  You can configure the system bus address in your application's config:

      config :rebus, :system_bus_address, "unix:path=/run/dbus/system_bus_socket"

  ## Architecture

  When you connect to a D-Bus bus using `connect/2`, Rebus creates a supervised
  connection process that handles the low-level protocol details. The connection
  manages authentication, message serialization/deserialization, and maintains
  the persistent connection to the bus.

  ## Error Handling

  `connect/2` returns `{:ok, connection}` or `{:error, reason}`. `call/3`
  returns a `%Rebus.Message{}` reply directly on success, while `send/2` and
  `send/3` return `:ok`. Public operation failures are returned as
  `{:error, reason}` tuples.

  ## Examples

      # Connect to session bus with default options
      {:ok, conn} = Rebus.connect(:session)

      # Connect to a Unix domain socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/dbus-socket"})

  For more advanced usage, see the documentation for `Rebus.Message` and other
  modules in this package.
  """

  @type address :: :system | :session | :socket.sockaddr_in() | :socket.sockaddr_un()

  @type error_reason ::
          :timeout
          | {:reply_dropped, :method_return | {:error, binary()}}
          | :not_connected
          | :encode_failed
          | :disconnected
          | :reply_expected
          | :no_reply_expected
          | :serial_exhausted
          | :remote_connection_unsupported
          | {:invalid_message_type, Rebus.Message.message_type()}

  @default_system_bus_address "unix:path=/run/dbus/system_bus_socket"

  @doc """
  Establishes a connection to a D-Bus message bus.

  Creates a supervised connection process that handles D-Bus protocol communication.
  The connection automatically handles authentication and maintains the persistent
  connection to the specified D-Bus endpoint.

  ## Parameters

  - `address` - The D-Bus endpoint to connect to:
    - `:system` - Connects to the system bus using the address specified in
       application config (see below) or the `/run/dbus/system_bus_socket` by default.
    - `:session` - Connects to the session bus using the address specified in
       the `DBUS_SESSION_BUS_ADDRESS` environment variable.
    - `%{family: :local, path: path}` - Unix domain socket connection to a local D-Bus daemon
    - `%{family: :inet, addr: {ip, port}}` - TCP/IP connection to a remote D-Bus daemon

  - `opts` - Optional keyword list of connection options:
    - `:timeout` - Positive maximum time in milliseconds for the auth-ID lookup,
      each socket connect, and authentication read (default: 5000). This is the original
      public connection-timeout option. It has no effect after authentication.
      `:read_timeout`, when supplied, takes precedence for setup as well.
      `connect/2` has no aggregate timeout: its worst-case wait includes this
      bounded auth-ID lookup, one socket connect, one authentication read, the
      validated initial Hello reply at `:read_timeout`, and the `AUTH`, `BEGIN`,
      and Hello writes at `:write_timeout`.
    - `:name` - Optional local atom used to register the connection process.
      It is intended for local discovery and lifecycle management; pass the
      returned PID to `call/3`, `send/2`, `send/3`, and signal-handler APIs. The
      name is held from process start through setup and is released if setup fails
      or the connection stops. Established connections are supervisor-owned and
      outlive the process that connected them; call `close/1` when their local
      lifecycle is complete. A PID discovered with `Process.whereis/1` before
      its corresponding `connect/2` returns is still establishing; operations
      issued to it can time out before they reach the connection and are safe to
      retry after `connect/2` succeeds.
    - `:write_timeout` - Positive maximum time for each authentication write
      (default: 5000). Once connected it bounds how long an outbound frame may
      await socket readiness. If no bytes were accepted, only that caller times
      out; after a partial frame, the temporary connection is terminated and
      inflight callers receive `{:error, :disconnected}` (it does not restart).
    - `:read_timeout` - Positive maximum time in milliseconds for the complete
      initial Hello reply and gaps between inbound fragments after connection
      (default: 5000). When supplied, it also overrides `:timeout` for socket
      setup and the complete, line-framed authentication response before
      `connect/2` returns. Each setup operation has one
      total budget, so peer progress cannot extend an authentication response
      indefinitely. Expiry makes `connect/2` return
      `{:error, :read_timeout}`. `connect/2` waits for the validated initial
      Hello reply before returning `{:ok, pid}`; that reply is bounded from the
      time Hello is sent and peer progress cannot extend the setup budget. Once
      established, it bounds
      gaps between inbound fragments, is reset whenever a peer makes progress,
      and is inactive while no frame is buffered. Expiry then terminates the
      temporary connection; inflight callers receive `{:error, :disconnected}`.

  ## Return Values

  - `{:ok, pid}` - Returns the PID of the connection process
  - `{:error, :read_timeout}` - Socket setup or authentication did not finish
    within its configured per-operation budget.
  - `{:error, :auth_id_unavailable}` - The local numeric identity required for
    `EXTERNAL` authentication could not be obtained.
  - `{:error, :auth_failed}` - The peer sent an invalid authentication response.
  - `{:error, {:auth_rejected, mechanisms}}` - The peer rejected `EXTERNAL`
    authentication and advertised its supported mechanisms.
  - `{:error, {:hello_failed, :invalid_unique_name}}` - The peer's initial
    Hello reply did not contain a valid D-Bus unique name.
  - `{:error, {:hello_failed, :resource_limit}}` - The peer's initial Hello
    reply exceeded a local decoding safety cap.
  - `{:error, :invalid_timeout | :invalid_read_timeout | :invalid_write_timeout |
    :invalid_name}` - A connection option was invalid.
  - `{:error, {:name_taken, pid}}` - The requested local name is held by a
    setup or established connection process. The PID can be adopted or passed to
    `close/1` when it is no longer needed.
  - `{:error, {:name_registered, pid}}` - The requested local name belongs to
    another process, not a supervised Rebus connection.
  - `{:error, reason}` - Another socket or setup failure occurred.

  ## Examples

      # Connect to a custom Unix socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      # Connect to a TCP endpoint
      address = %{family: :inet, addr: {127, 0, 0, 1}, port: 12345}
      {:ok, conn} = Rebus.connect(address)

      # Explicitly release a named connection when its lifecycle is complete.
      {:ok, conn} = Rebus.connect(address, name: :local_bus)
      :ok = Rebus.close(conn)

  ## Notes

  The returned PID is for the connection process, which is the main interface for
  sending and receiving D-Bus messages. Connections are supervisor-owned; close
  them with `close/1` when they are no longer needed.

  """
  @spec connect(address(), keyword()) :: {:ok, pid()} | {:error, term()}
  def connect(address, opts \\ [])

  def connect(:system, opts) do
    case Application.get_env(:rebus, :system_bus_address, @default_system_bus_address) do
      nil ->
        {:error, :no_system_bus_address}

      "unix:path=" <> address ->
        connect(%{family: :local, path: address}, opts)
    end
  end

  def connect(:session, opts) do
    case System.get_env("DBUS_SESSION_BUS_ADDRESS") do
      nil ->
        {:error, :no_session_bus_address}

      "unix:path=" <> address ->
        connect(%{family: :local, path: address}, opts)
    end
  end

  def connect(%{family: family} = addr, opts) when family in [:inet, :local] do
    connect_ref = make_ref()

    args =
      opts
      |> Keyword.delete(:auth_id_fun)
      |> Keyword.put(:addr, addr)
      |> Keyword.put(:connect_waiter, {self(), connect_ref})

    child_spec = {Rebus.Connection, args}

    case DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, child_spec) do
      {:ok, pid} -> await_connection(pid, connect_ref, Process.monitor(pid))
      {:error, {:already_started, pid}} -> name_collision(pid)
      other -> other
    end
  end

  defp name_collision(pid) do
    if connection_child?(pid),
      do: {:error, {:name_taken, pid}},
      else: {:error, {:name_registered, pid}}
  end

  defp connection_child?(pid) do
    try do
      Enum.any?(DynamicSupervisor.which_children(Rebus.ConnectionSupervisor), fn
        {_id, ^pid, _type, _modules} -> true
        _child -> false
      end)
    catch
      :exit, _reason -> false
    end
  end

  defp await_connection(pid, connect_ref, monitor_ref) do
    receive do
      {^connect_ref, {:ok, ^pid}} ->
        Kernel.send(pid, {connect_ref, :accepted})
        await_accepted_connection(pid, connect_ref, monitor_ref)

      {^connect_ref, {:error, reason}} ->
        await_failed_connection(pid, monitor_ref, reason)

      {:DOWN, ^monitor_ref, :process, ^pid, {:shutdown, reason}} ->
        {:error, reason}

      {:DOWN, ^monitor_ref, :process, ^pid, reason} ->
        {:error, reason}
    end
  end

  defp await_failed_connection(pid, monitor_ref, reason) do
    receive do
      {:DOWN, ^monitor_ref, :process, ^pid, _stop_reason} -> {:error, reason}
    end
  end

  defp await_accepted_connection(pid, connect_ref, monitor_ref) do
    receive do
      {^connect_ref, :accepted} ->
        Process.demonitor(monitor_ref, [:flush])
        {:ok, pid}

      {:DOWN, ^monitor_ref, :process, ^pid, {:shutdown, reason}} ->
        {:error, reason}

      {:DOWN, ^monitor_ref, :process, ^pid, reason} ->
        {:error, reason}
    end
  end

  @doc """
  Stops a local connection process created by `connect/2`.

  Connections are supervised and remain alive after the connecting process exits.
  Use this function to release a named or otherwise no-longer-needed connection.
  It accepts only local connection PIDs; remote PIDs are not supported.

  ## Return Values

  - `:ok` - The supervised connection was stopped.
  - `{:error, :not_found}` - The PID is not a current Rebus connection.
  - `{:error, :remote_connection_unsupported}` - The PID belongs to another node.
  """
  @spec close(pid()) :: :ok | {:error, :not_found | :remote_connection_unsupported}
  def close(conn) when is_pid(conn) do
    if node(conn) == node() do
      try do
        case DynamicSupervisor.terminate_child(Rebus.ConnectionSupervisor, conn) do
          :ok -> :ok
          _ -> {:error, :not_found}
        end
      catch
        :exit, _reason -> {:error, :not_found}
      end
    else
      {:error, :remote_connection_unsupported}
    end
  end

  @doc """
  Same as `connect/2`, but raises an exception on failure.
  """
  @spec connect!(address(), keyword()) :: pid()
  def connect!(address, opts \\ []) do
    case connect(address, opts) do
      {:ok, pid} -> pid
      {:error, reason} -> raise "Failed to connect to D-Bus: #{inspect(reason)}"
    end
  end

  @doc """
  Sends a method call and waits for its correlated reply.

  `call/3` accepts only method calls that expect replies and returns the complete
  `%Rebus.Message{}` reply. A D-Bus error reply is
  returned as `%Rebus.Message{type: :error}` so callers can inspect its
  `:error_name` header and body. If no reply arrives before `timeout` milliseconds,
  it returns `{:error, :timeout}` and removes the request from the connection's
  pending-reply state. Messages that cannot be encoded return
  `{:error, :encode_failed}`. Invalid operations return
  `{:error, :no_reply_expected}` or `{:error, {:invalid_message_type, type}}`.
  A closed connection returns `{:error, :disconnected}`. A timed-out call may
  already have reached the peer, so callers must treat it as delivery-ambiguous.
  The exception is a PID discovered with `Process.whereis(name)` before its
  corresponding `connect/2` returns: while setup is blocked on authentication or
  Hello I/O, the request can time out before the connection reads it. That frame
  was definitely not written and is safe to retry after `connect/2` succeeds.
  `{:error, :serial_exhausted}` means all valid D-Bus serials are in use.
  `{:error, :not_connected}` means setup has not yet been accepted.
  `{:error, {:reply_dropped, :method_return}}` means the peer definitely
  received the request and produced a successful reply, but its payload
  exceeded a local decoding resource cap and was discarded.
  `{:error, {:reply_dropped, {:error, error_name}}}` means the peer definitely
  produced that D-Bus error reply; depending on the operation and error, the
  requested operation may not have executed. Neither outcome is
  delivery-ambiguous: decide whether to retry from the operation and error
  semantics, never by blindly retrying.

  Connections must be local to the calling node; remote connection PIDs return
  `{:error, :remote_connection_unsupported}`.

  ## Examples

      message = Rebus.Message.new!(:method_call,
        path: "/org/freedesktop/DBus",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "ListNames"
      )

      %Rebus.Message{type: :method_return, body: [names]} = Rebus.call(conn, message)

      # Use a custom timeout in milliseconds.
      {:error, :timeout} = Rebus.call(conn, message, 1_000)
  """
  @spec call(pid(), Rebus.Message.t(), non_neg_integer()) ::
          Rebus.Message.t() | {:error, error_reason()}
  def call(conn, %Rebus.Message{} = message, timeout \\ 5_000)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    Rebus.Connection.call(conn, message, timeout)
  end

  @doc """
  Sends a message without waiting for a reply.

  Use this for signals and method calls whose flags include `:no_reply_expected`.
  Reply-expecting method calls return `{:error, :reply_expected}`. It returns
  `:ok` once the message has been handed to the socket, or
  `{:error, :encode_failed}` if the message cannot be encoded. A closed
  connection returns `{:error, :disconnected}`. `{:error, :timeout}` means the
  message may already have reached the peer, so it must not be blindly retried.
  The exception is a PID discovered with `Process.whereis(name)` before its
  corresponding `connect/2` returns: while setup is blocked on authentication or
  Hello I/O, the send can time out before the connection reads it. That frame was
  definitely not written and is safe to retry after `connect/2` succeeds.
  `{:error, :serial_exhausted}` means all valid D-Bus serials are in use.
  `{:error, :not_connected}` means setup has not yet been accepted.
  `send/2` has a fixed five-second caller dispatch timeout; use `send/3` when
  the caller needs a different bound. This is distinct from the connection's
  `:write_timeout`, which bounds socket readiness for a frame.
  Remote connection PIDs return `{:error, :remote_connection_unsupported}`.
  """
  @spec send(pid(), Rebus.Message.t()) :: :ok | {:error, error_reason()}
  def send(conn, %Rebus.Message{} = message) when is_pid(conn),
    do: Rebus.Connection.send(conn, message)

  @doc """
  Sends a message with a custom dispatch timeout in milliseconds.

  A timeout is delivery-ambiguous: the message may already have reached the peer.
  """
  @spec send(pid(), Rebus.Message.t(), non_neg_integer()) :: :ok | {:error, error_reason()}
  def send(conn, %Rebus.Message{} = message, timeout)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    Rebus.Connection.send(conn, message, timeout)
  end

  @doc """
  Adds a signal handler to receive D-Bus signals on the connection.

  Signal handlers receive all D-Bus signals that arrive on the connection.
  Multiple signal handlers can be registered on the same connection, and each
  will receive copies of all signals.

  ## Parameters

  - `conn` - The connection PID returned from `connect/2`

  ## Return Values

  - `reference()` - A unique reference that identifies this signal handler
  - `{:error, :not_connected}` - Connection establishment has not completed.
  - `{:error, :timeout}` - The connection did not service the request promptly.
  - `{:error, :disconnected}` - The connection has stopped.

  ## Examples

      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      case Rebus.add_signal_handler(conn) do
        ref when is_reference(ref) ->
          # The calling process will now receive messages like:
          # {^ref, %Rebus.Message{type: :signal, ...}}
          ref

        {:error, reason} ->
          # Retry or handle the unavailable connection.
          {:error, reason}
      end

  ## Signal Message Format

  When a D-Bus signal is received, registered signal handlers will receive
  a message in the format:

      {^ref, %Rebus.Message{
        type: :signal,
        header_fields: %{
          path: "/path/to/object",
          interface: "com.example.Interface",
          member: "SignalName",
          sender: "com.example.Service"
        },
        body: [signal_args...],
        signature: "signal_signature"
      }}

  ## Notes

  Signal handlers should be prepared to handle a potentially high volume of
  messages depending on the activity on the D-Bus. Consider using selective
  receive or GenServer message handling for robust signal processing.

  Remember to call `delete_signal_handler/2` when you no longer need to
  receive signals to avoid message queue buildup.

  Signal handlers are automatically cleaned up when the connection is closed
  or when the handler exits.

  Returns `{:error, :not_connected}` while connection establishment is pending,
  `{:error, :timeout}` if the connection cannot service the request promptly,
  or `{:error, :disconnected}` if it has stopped.
  """
  @spec add_signal_handler(pid()) ::
          reference() | {:error, :not_connected | :timeout | :disconnected}
  defdelegate add_signal_handler(conn), to: Rebus.Connection

  @doc """
  Removes a previously registered signal handler from the connection.

  Stops the specified signal handler from receiving future D-Bus signals.
  The handler is identified by the reference returned from `add_signal_handler/1`.

  ## Parameters

  - `conn` - The connection PID returned from `connect/2`
  - `ref` - The reference returned from `add_signal_handler/1`

  ## Return Values

  - `:ok` - The signal handler was successfully removed
  - `{:error, :not_connected}` - Connection establishment has not completed.
  - `{:error, :timeout}` - The connection did not service the request promptly.
  - `{:error, :disconnected}` - The connection has stopped.

  ## Examples

      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      with ref when is_reference(ref) <- Rebus.add_signal_handler(conn),
           :ok <- Rebus.delete_signal_handler(conn, ref) do
        :ok
      else
        {:error, reason} -> {:error, reason}
      end

  ## Notes

  After deleting a signal handler, the calling process will no longer receive
  signal messages for that handler. Other signal handlers on the same connection
  (if any) will continue to receive signals normally.

  Deleting the same reference repeatedly returns `:ok` while the connection is
  available. It can return an error if the connection becomes unavailable.

  Returns the same connection-state errors as `add_signal_handler/1`.
  """
  @spec delete_signal_handler(pid(), reference()) ::
          :ok | {:error, :not_connected | :timeout | :disconnected}
  defdelegate delete_signal_handler(conn, ref), to: Rebus.Connection
end
