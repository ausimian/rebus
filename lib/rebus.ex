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

      # Add a signal handler to receive all signals
      ref = Rebus.add_signal_handler(conn)

      # Later, remove the signal handler
      Rebus.remove_signal_handler(conn, ref)

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
    - `:write_timeout` - Positive maximum time for each authentication write
      (default: 5000). Once connected it bounds how long an outbound frame may
      await socket readiness. If no bytes were accepted, only that caller times
      out; after a partial frame, the temporary connection is terminated and
      inflight callers receive `{:error, :disconnected}` (it does not restart).
    - `:read_timeout` - Positive maximum time in milliseconds to establish the
      socket or receive the complete, line-framed authentication response
      before `connect/2` returns (default: 5000). Each setup operation has one
      total budget, so peer progress cannot extend an authentication response
      indefinitely. Expiry makes `connect/2` return
      `{:error, :read_timeout}`. After `connect/2` returns `{:ok, pid}`, it
      bounds the complete initial Hello reply from the time it is sent; peer
      progress cannot extend that setup budget. Once established, it bounds
      gaps between inbound fragments, is reset whenever a peer makes progress,
      and is inactive while no frame is buffered. Expiry then terminates the
      temporary connection; inflight callers receive `{:error, :disconnected}`.

  ## Return Values

  - `{:ok, pid}` - Returns the PID of the connection process
  - `{:error, reason}` - Connection failed due to the specified reason

  ## Examples

      # Connect to a custom Unix socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      # Connect to a TCP endpoint
      address = %{family: :inet, addr: {127, 0, 0, 1}, port: 12345}
      {:ok, conn} = Rebus.connect(address)

  ## Notes

  The returned PID is for the connection process, which is the main interface for
  sending and receiving D-Bus messages.

  """
  @spec connect(address(), keyword()) :: DynamicSupervisor.on_start_child()
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
    args =
      opts
      |> Keyword.put(:addr, addr)

    child_spec = {Rebus.Connection, args}
    DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, child_spec)
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
  `{:error, :serial_exhausted}` means all valid D-Bus serials are in use.

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
  `{:error, :serial_exhausted}` means all valid D-Bus serials are in use.
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

  ## Examples

      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})
      ref = Rebus.add_signal_handler(conn)

      # The calling process will now receive messages like:
      # {^ref, %Rebus.Message{type: :signal, ...}}

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

  Remember to call `remove_signal_handler/2` when you no longer need to
  receive signals to avoid message queue buildup.

  Signal handlers are automatically cleaned up when the connection is closed
  or when the handler exits.
  """
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

  ## Examples

      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})
      ref = Rebus.add_signal_handler(conn)

      # ... handle signals ...

      # Remove the handler when done
      :ok = Rebus.delete_signal_handler(conn, ref)

  ## Notes

  After deleting a signal handler, the calling process will no longer receive
  signal messages for that handler. Other signal handlers on the same connection
  (if any) will continue to receive signals normally.

  It's safe to call this function multiple times with the same reference -
  subsequent calls will simply return `:ok` without error.
  """
  defdelegate delete_signal_handler(conn, ref), to: Rebus.Connection
end
