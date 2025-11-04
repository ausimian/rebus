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
      session = System.get_env("DBUS_SESSION_BUS_ADDRESS")
      {:ok, conn} = Rebus.connect(%{family: :local, path: session})

      # Add a signal handler to receive all signals
      ref = Rebus.add_signal_handler(conn)

      # Later, remove the signal handler
      Rebus.remove_signal_handler(conn, ref)

  ## Connection Types

  Rebus supports connecting to different types of D-Bus endpoints:

  - `%{family: :inet, addr: {ip, port}}` - TCP/IP connection to a remote D-Bus daemon
  - `%{family: :local, path: path}` - Unix domain socket connection to a local D-Bus daemon

  ## Architecture

  When you connect to a D-Bus bus using `connect/2`, Rebus creates a supervised
  connection process that handles the low-level protocol details. The connection
  manages authentication, message serialization/deserialization, and maintains
  the persistent connection to the bus.

  ## Error Handling

  All functions return standard Elixir `{:ok, result}` or `{:error, reason}` tuples.
  Connection failures, authentication errors, and protocol violations are properly
  propagated as error tuples.

  ## Examples

      # Connect to session bus with default options
      session = System.get_env("DBUS_SESSION_BUS_ADDRESS")
      {:ok, conn} = Rebus.connect(%{family: :local, path: session})

      # Connect to a Unix domain socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/dbus-socket"})

  For more advanced usage, see the documentation for `Rebus.Message` and other
  modules in this package.
  """

  @type address :: :session | :socket.sockaddr_in() | :socket.sockaddr_un()

  @doc """
  Establishes a connection to a D-Bus message bus.

  Creates a supervised connection process that handles D-Bus protocol communication.
  The connection automatically handles authentication and maintains the persistent
  connection to the specified D-Bus endpoint.

  ## Parameters

  - `address` - The D-Bus endpoint to connect to:
    - `%{family: :inet, addr: {ip, port}}` - TCP/IP connection
    - `%{family: :local, path: path}` - Unix domain socket connection

  - `opts` - Optional keyword list of connection options:
    - `:timeout` - Connection timeout in milliseconds (default: 5000)
    - `:name` - Optional name for the connection process
    - Additional options are passed to the underlying connection process

  ## Return Values

  - `{:ok, pid}` - Returns the PID of the transport process for the connection
  - `{:error, reason}` - Connection failed due to the specified reason

  ## Examples

      # Connect to a custom Unix socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      # Connect to a TCP endpoint
      address = %{family: :inet, addr: {{127, 0, 0, 1}, 12345}}
      {:ok, conn} = Rebus.connect(address)

  ## Notes

  The returned PID is for the connection process, which is the main interface for
  sending and receiving D-Bus messages.

  """
  @spec connect(address(), keyword()) :: DynamicSupervisor.on_start_child()
  def connect(address, opts \\ [])

  def connect(%{family: family} = addr, opts) when family in [:inet, :local] do
    args =
      opts
      |> Keyword.put(:addr, addr)

    child_spec = {Rebus.Connection, args}
    DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, child_spec)
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

      {:ok, conn} = Rebus.connect(:session)
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

      {:ok, conn} = Rebus.connect(:session)
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
