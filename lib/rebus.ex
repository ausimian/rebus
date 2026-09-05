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
  - Answering `org.freedesktop.DBus.Peer` and returning `UnknownMethod` for
    other inbound method calls (there is no service-side API yet)

  ## Quick Start

      # Connect to the session bus
      {:ok, conn} = Rebus.connect(:session)

      message = Rebus.Message.new!(:method_call,
        path: "/org/freedesktop/DBus",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "ListNames"
      )

      {:ok, %Rebus.Message{type: :method_return, body: [names]}} = Rebus.call(conn, message)

      signal = Rebus.Message.new!(:signal,
        path: "/org/example/Status",
        interface: "org.example.Status",
        member: "Changed",
        signature: "s",
        body: ["ready"]
      )

      :ok = Rebus.send(conn, signal)

      # Add a signal handler to receive all signals.
      case Rebus.add_signal_handler(conn) do
        {:ok, ref} -> Rebus.delete_signal_handler(conn, ref)
        {:error, reason} -> {:error, reason}
      end

  ## Supported platforms

  Rebus supports Linux and macOS, which are the platforms exercised in CI.
  Other Unix variants are untested; Unix file descriptor passing in particular
  is limited to Linux and macOS. Windows is not supported.

  ## Connection Types

  Rebus connects to the system bus, the session bus, a Unix domain socket, or
  a TCP endpoint. See `connect/2` for the accepted address forms, the
  connection options, and the errors each can return.

  ## Configuration

  You can configure the system bus address in your application's config:

      config :rebus, :system_bus_address, "unix:path=/run/dbus/system_bus_socket"

  ## Architecture

  When you connect to a D-Bus bus using `connect/2`, Rebus creates a supervised
  connection process that handles the low-level protocol details. The connection
  manages authentication, message serialization/deserialization, and maintains
  the persistent connection to the bus.

  Inbound method calls are answered by the connection itself. It implements
  `org.freedesktop.DBus.Peer` (`Ping` and `GetMachineId`) and replies to every
  other method call with an `org.freedesktop.DBus.Error.UnknownMethod` error,
  so a peer fails immediately instead of waiting for its own timeout. A call
  flagged `:no_reply_expected` is dropped silently, and any descriptor received
  with a call is closed. There is no API to serve method calls from
  application code.

  Signals are delivered by the connection process itself: each connection keeps
  its own table of registered handlers and sends every matching signal directly
  to the handler's owner, so signals received on one connection never reach
  handlers registered on another.

  ## Error Handling

  `connect/2` returns `{:ok, connection}` or `{:error, reason}`. `call/3`
  returns `{:ok, %Rebus.Message{}}` for a successful reply and
  `{:error, %Rebus.Message{type: :error}}` for a D-Bus error reply, while
  `send/2` and `send/3` return `:ok`. Public operation failures are returned as
  `{:error, reason}` tuples.

  ## Unix file descriptors

  On Linux and macOS, a local Unix-socket connection can carry raw file
  descriptors. See [Unix file descriptor passing](unix_fds.html).

  ## Examples

      # Connect to session bus with default options
      {:ok, conn} = Rebus.connect(:session)

      # Connect to a Unix domain socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/dbus-socket"})

  For more advanced usage, see the documentation for `Rebus.Message` and other
  modules in this package.
  """

  @type address ::
          :system
          | :session
          | :socket.sockaddr_in()
          | :socket.sockaddr_in6()
          | :socket.sockaddr_un()

  @typedoc """
  Failure reasons returned by `call/3`, `send/2` and `send/3`.

  Each function documents the subset it can return.
  """
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
          | :unix_fd_not_negotiated
          | :unix_fd_unsupported
          | :unix_fd_send_failed
          | :fd_claim_expired
          | {:invalid_message_type, Rebus.Message.message_type()}

  @typedoc """
  Failure reasons returned by `call/3`.

  A D-Bus error reply from the peer is a definitive answer rather than a
  transport failure, so it is returned as the complete
  `%Rebus.Message{type: :error}`. Everything else is a local or transport
  failure reason.
  """
  @type call_error :: Rebus.Message.t() | error_reason()

  @typedoc """
  Failure reasons returned by `add_match/3` and `remove_match/3`.

  See [Signal subscriptions and match rules](match_rules.html) for what each
  one means.
  """
  @type match_error_reason ::
          :timeout
          | :not_connected
          | :disconnected
          | :remote_connection_unsupported
          | :encode_failed
          | :serial_exhausted
          | :fd_claim_expired
          | :invalid_bus_reply
          | :match_rule_cleanup_pending
          | :match_subscription_state_lost
          | :sender_routing_ambiguous
          | :not_a_bus
          | {:reply_dropped, :method_return | {:error, binary()}}
          | {:bus_error, binary()}

  @default_system_bus_address "unix:path=/run/dbus/system_bus_socket"

  @doc """
  Connects to a D-Bus endpoint and returns its connection process.

  The call blocks until the connection is usable: authenticated and, on a
  message bus, holding its unique name. The returned PID is supervised and
  outlives the process that connected it, so release it with `close/1`.

  ## Addresses

  - `:system` - the system bus.
  - `:session` - the session bus.
  - `%{family: :local, path: "/tmp/my-dbus"}` - a Unix domain socket.
  - `%{family: :inet, addr: {127, 0, 0, 1}, port: 12345}` - a TCP endpoint,
    with `:inet6` and an eight-element address for IPv6.

  `:system` reads the `:system_bus_address` config key, which defaults to
  `#{@default_system_bus_address}`; `:session` reads
  `DBUS_SESSION_BUS_ADDRESS`. Both hold a D-Bus address list whose supported
  entries are tried in the order they are listed, and a `guid=` on the entry
  that answers must match the server's identity. See
  [Authentication](authentication.html) for the rest.

  ## Options

  | Option | Default | Bounds / meaning |
  | --- | --- | --- |
  | `:timeout` | `5000` | Positive milliseconds for setup: the identity lookup, any DNS lookup, the socket connect and authentication. It bounds nothing after that. |
  | `:read_timeout` | `5000` | Positive milliseconds for the initial `Hello` reply, and afterwards for gaps between inbound fragments. When given it also replaces `:timeout` for setup. |
  | `:write_timeout` | `5000` | Positive milliseconds an outbound frame, including every authentication write, may wait for the socket to accept it. |
  | `:name` | `nil` | Atom to register the connection process under, for local discovery only. |
  | `:allow_anonymous` | `false` | Allow the `ANONYMOUS` mechanism, which authenticates nothing and also requires `bus: false`. |
  | `:bus` | `true` | Pass `false` for a peer-to-peer endpoint, which sends no `Hello` and has no unique name. |

  ## Notes

  - A PID found by name before its `connect/2` returns is still being
    established. Operations sent to it may time out, and are safe to retry
    once `connect/2` succeeds.
  - A write timeout that accepted no bytes fails only that caller. After a
    partial frame the connection terminates and inflight callers receive
    `{:error, :disconnected}`.
  - For an address list, setup shares one budget across every candidate, so a
    slow candidate leaves less time for the entries after it.
  - Failed address-list attempts are logged at debug level, without any
    address, host, path or GUID.

  ## Return values

  Success is `{:ok, pid}`. Every failure is `{:error, reason}`:

  - Invalid option: `:invalid_timeout`, `:invalid_read_timeout`,
    `:invalid_write_timeout`, `:invalid_allow_anonymous`,
    `:invalid_bus_option`, `:invalid_name`.
  - Unusable address: `{:invalid_bus_address, reason}`,
    `:unsupported_bus_transport`, `{:tcp_resolution_failed, reason}`,
    `:no_system_bus_address`, `:no_session_bus_address`.
  - Refused authentication: `:auth_id_unavailable`, `:auth_cookie_unavailable`,
    `:auth_failed`, `{:auth_rejected, mechanisms}`, `:guid_mismatch`. See
    [Authentication](authentication.html).
  - Expired setup budget: `:read_timeout`, or `{:read_timeout, reason}` once an
    address-list attempt has already failed.
  - Refused or unusable `Hello` reply: `{:hello_failed, reason}`.
  - Taken `:name`: `{:name_taken, pid}` for another Rebus connection, or
    `{:name_registered, pid}` for any other process.
  - Any other socket or setup failure: the failure's own atom.

  ## Examples

      # A custom Unix socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      # A TCP endpoint
      address = %{family: :inet, addr: {127, 0, 0, 1}, port: 12345}
      {:ok, conn} = Rebus.connect(address)

      # Release a named connection when its lifecycle is complete
      {:ok, conn} = Rebus.connect(address, name: :local_bus)
      :ok = Rebus.close(conn)
  """
  @spec connect(address(), keyword()) :: {:ok, pid()} | {:error, term()}
  def connect(address, opts \\ [])

  def connect(:system, opts) do
    with :ok <- validate_bus_alias_option(opts) do
      case Application.get_env(:rebus, :system_bus_address, @default_system_bus_address) do
        nil -> {:error, :no_system_bus_address}
        address -> connect_bus_address(address, opts)
      end
    end
  end

  def connect(:session, opts) do
    with :ok <- validate_bus_alias_option(opts) do
      case System.get_env("DBUS_SESSION_BUS_ADDRESS") do
        nil -> {:error, :no_session_bus_address}
        address -> connect_bus_address(address, opts)
      end
    end
  end

  def connect(%{family: family} = addr, opts) when family in [:inet, :inet6, :local] do
    impl = build_impl(opts)
    impl.connector.connect(addr, {opts, %{impl: impl}})
  end

  # Implementation modules are chosen through the private `:__impl__` option.
  # Outside this project's own test build `Rebus.Impl.from_options/1` compiles
  # to a clause that ignores its argument, so nothing reads the option.
  defp build_impl(opts), do: Rebus.Impl.from_options(opts)

  # `:system` and `:session` name message buses, so they cannot be peer-to-peer
  # endpoints. Reject the option before any address lookup or I/O so the error
  # does not depend on the local environment.
  defp validate_bus_alias_option(opts) do
    case Keyword.get(opts, :bus, true) do
      true -> :ok
      _bus -> {:error, :invalid_bus_option}
    end
  end

  defp connect_bus_address(address, opts) do
    case Rebus.BusAddress.parse(address) do
      {:ok, candidates} -> Rebus.AddressList.connect(candidates, opts)
      {:error, _reason} = error -> error
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
      terminate_connection_child(conn)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  defp terminate_connection_child(conn) do
    case DynamicSupervisor.terminate_child(Rebus.ConnectionSupervisor, conn) do
      :ok -> :ok
      _ -> {:error, :not_found}
    end
  catch
    :exit, _reason -> {:error, :not_found}
  end

  @doc """
  Same as `connect/2`, but raises `ArgumentError` on failure.

  The exception message carries the reason `connect/2` would have returned.
  """
  @spec connect!(address(), keyword()) :: pid()
  def connect!(address, opts \\ []) do
    case connect(address, opts) do
      {:ok, pid} -> pid
      {:error, reason} -> raise ArgumentError, "failed to connect to D-Bus: #{inspect(reason)}"
    end
  end

  @doc """
  Sends a method call and waits for its correlated reply.

  Only a method call that expects a reply is accepted, and `timeout` is in
  milliseconds. Both result shapes carry the complete message, including any
  received descriptors in `:unix_fds`. A D-Bus error reply from the peer is
  returned as `{:error, %Rebus.Message{type: :error}}`, so read its
  `:error_name` header and body from there.

  ## Return values

  - `{:ok, %Rebus.Message{type: :method_return}}` - the peer replied
    successfully.
  - `{:error, %Rebus.Message{type: :error}}` - the peer returned a D-Bus error
    reply.
  - `{:error, :timeout}` - no reply arrived in time, and the request may
    already have reached the peer. A request sent to a named PID whose
    `connect/2` has not yet returned was never written and is safe to retry.
  - `{:error, {:reply_dropped, :method_return}}` - the peer replied
    successfully, but the reply was too large to decode and was discarded.
  - `{:error, {:reply_dropped, {:error, error_name}}}` - the peer returned that
    D-Bus error reply, which was discarded for the same reason. Neither dropped
    shape is delivery-ambiguous, so decide whether to retry from what the
    operation does.
  - `{:error, :fd_claim_expired}` - the reply carried descriptors and Rebus
    closed them instead of handing them over.
  - `{:error, :disconnected}` - the connection stopped before the reply, or
    before its descriptors transferred. A call carrying descriptors can return
    after `timeout`; see
    [Unix file descriptor passing](unix_fds.html).
  - Nothing was written for `:encode_failed`, `:no_reply_expected`,
    `{:invalid_message_type, type}`, `:not_connected`, `:serial_exhausted`,
    `:unix_fd_unsupported`, `:unix_fd_not_negotiated`, `:unix_fd_send_failed`
    and `:remote_connection_unsupported`.

  ## Examples

      message = Rebus.Message.new!(:method_call,
        path: "/org/freedesktop/DBus",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "ListNames"
      )

      {:ok, %Rebus.Message{type: :method_return, body: [names]}} = Rebus.call(conn, message)

      # Use a custom timeout in milliseconds.
      {:error, :timeout} = Rebus.call(conn, message, 1_000)
  """
  @spec call(pid(), Rebus.Message.t(), non_neg_integer()) ::
          {:ok, Rebus.Message.t()} | {:error, call_error()}
  def call(conn, %Rebus.Message{} = message, timeout \\ 5_000)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    Rebus.Connection.call(conn, message, timeout)
  end

  @doc """
  Sends a message without waiting for a reply.

  Use this for signals and for method calls flagged `:no_reply_expected`.
  `send/2` allows five seconds for the connection to accept the message;
  `send/3` takes that timeout as an argument.

  ## Return values

  - `:ok` - the frame was handed to the socket.
  - `{:error, :timeout}` - the message may already have reached the peer. A
    message sent to a named PID whose `connect/2` has not yet returned was
    never written and is safe to retry.
  - `{:error, :disconnected}` - the connection stopped.
  - Nothing was written for `:encode_failed`, `:reply_expected`,
    `{:invalid_message_type, type}`, `:not_connected`, `:serial_exhausted`,
    `:unix_fd_unsupported`, `:unix_fd_not_negotiated`, `:unix_fd_send_failed`
    and `:remote_connection_unsupported`.
  """
  @spec send(pid(), Rebus.Message.t()) :: :ok | {:error, error_reason()}
  def send(conn, %Rebus.Message{} = message) when is_pid(conn),
    do: Rebus.Connection.dispatch(conn, message)

  @doc """
  Sends a message with a custom dispatch timeout in milliseconds.

  Accepts the same messages as `send/2` and returns the same values. The
  timeout bounds how long the connection has to accept the message.
  `{:error, :timeout}` is delivery-ambiguous.
  """
  @spec send(pid(), Rebus.Message.t(), non_neg_integer()) :: :ok | {:error, error_reason()}
  def send(conn, %Rebus.Message{} = message, timeout)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    Rebus.Connection.dispatch(conn, message, timeout)
  end

  @doc """
  Subscribes the calling process to signals selected by a validated match rule.

  This registers the rule with `org.freedesktop.DBus.AddMatch` and returns a
  subscription reference. The process receives matching signals as
  `{reference, %Rebus.Message{}}`, just like `add_signal_handler/1`.

  `AddMatch` is a bus-driver method, so a connection opened with `bus: false`
  returns `{:error, :not_a_bus}` and nothing is sent.

  Build the rule with `Rebus.MatchRule.new/1`; raw match strings are not
  accepted. Rules are canonical and connection-scoped: equivalent rules share
  one remote AddMatch registration, while each successful call returns an
  independent reference. The bus rule remains active until the last reference
  is removed or its owning process exits.

  The supplied timeout is a single budget for local handler installation and
  the AddMatch reply. `{:error, :timeout}` is delivery-ambiguous: no
  subscription reference is returned, but the bus might already have installed
  the rule. A D-Bus error reply becomes `{:error, {:bus_error, error_name}}`,
  and an overlapping rule with a different sender is rejected as
  `{:error, :sender_routing_ambiguous}`.

  After an ambiguous outcome Rebus cleans up in the background, so a later
  subscription for the same rule can return
  `{:error, :match_rule_cleanup_pending}`, and an operation whose state is
  lost returns `{:error, :match_subscription_state_lost}`. See
  [Signal subscriptions and match rules](match_rules.html) for sender matching
  and the full list of reasons.

  ## Example

      rule = Rebus.MatchRule.new!(
        sender: "org.freedesktop.DBus",
        interface: "org.freedesktop.DBus",
        member: "NameOwnerChanged",
        args: %{0 => "org.example.Service"}
      )

      {:ok, ref} = Rebus.add_match(conn, rule)
      assert_receive {^ref, %Rebus.Message{type: :signal}}
  """
  @spec add_match(pid(), Rebus.MatchRule.t(), non_neg_integer()) ::
          {:ok, reference()} | {:error, match_error_reason()}
  def add_match(conn, %Rebus.MatchRule{} = rule, timeout \\ 5_000)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    Rebus.MatchSubscription.add(conn, rule, timeout)
  end

  @doc """
  Removes a match-rule subscription reference.

  Removing a reference is idempotent and scoped to the connection on which it
  was created. The final reference issues
  `org.freedesktop.DBus.RemoveMatch`; earlier references only stop their local
  handler, so a successfully removed reference cannot receive a later signal.
  A failed or timed-out removal keeps the reference so you can retry it.

  Rebus removes the local handler and the bus rule when the owning process
  exits, and closing the connection discards both. See
  [Signal subscriptions and match rules](match_rules.html) for what an
  ambiguous removal leaves behind.
  """
  @spec remove_match(pid(), reference(), non_neg_integer()) ::
          :ok | {:error, match_error_reason()}
  def remove_match(conn, ref, timeout \\ 5_000)
      when is_pid(conn) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    Rebus.MatchSubscription.remove(conn, ref, timeout)
  end

  @doc """
  Adds a signal handler to receive D-Bus signals on the connection.

  Signal handlers receive all D-Bus signals that arrive on the connection.
  Multiple signal handlers can be registered on the same connection, and each
  will receive copies of all signals.

  ## Parameters

  - `conn` - The connection PID returned from `connect/2`

  ## Return Values

  - `{:ok, reference()}` - A unique reference that identifies this signal handler
  - `{:error, :not_connected}` - Connection establishment has not completed.
  - `{:error, :timeout}` - The connection did not service the request promptly.
  - `{:error, :disconnected}` - The connection has stopped.

  ## Examples

      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})

      case Rebus.add_signal_handler(conn) do
        {:ok, ref} ->
          # The calling process will now receive messages like:
          # {^ref, %Rebus.Message{type: :signal, ...}}
          {:ok, ref}

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
  or when the handler exits. A handler is registered with the connection it was
  added to and only receives signals that arrive on that connection.

  Returns `{:error, :not_connected}` while connection establishment is pending,
  `{:error, :timeout}` if the connection cannot service the request promptly,
  or `{:error, :disconnected}` if it has stopped.
  """
  @spec add_signal_handler(pid()) ::
          {:ok, reference()} | {:error, :not_connected | :timeout | :disconnected}
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

      with {:ok, ref} <- Rebus.add_signal_handler(conn),
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
