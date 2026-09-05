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

  `:match_recovery_max_rules` caps how many distinct match rules a single
  connection may hold whose bus-side state is uncertain. The default is 64, and
  a connection that reaches the cap is closed. See
  [Signal subscriptions and match rules](match_rules.html).

      config :rebus, :match_recovery_max_rules, 64

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

  ## Return values

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
  milliseconds. Each result carries the complete message, including any received
  descriptors in `:unix_fds`. A D-Bus error reply from the peer is returned as
  `{:error, %Rebus.Message{type: :error}}`.

  ## Return values

  - `{:ok, %Rebus.Message{type: :method_return}}` - the peer replied successfully.
  - `{:error, %Rebus.Message{type: :error}}` - the peer returned a D-Bus error reply.
  - `{:error, :timeout}` - no reply arrived in time, and the request may already
    have reached the peer. A request sent to a named PID whose `connect/2` has
    not yet returned was never written and is safe to retry.
  - `{:error, {:reply_dropped, :method_return}}` or
    `{:error, {:reply_dropped, {:error, error_name}}}` - the peer definitely
    replied, but the reply was too large to decode and was discarded. Neither is
    delivery-ambiguous, so decide whether to retry from what the operation does.
  - `{:error, :fd_claim_expired}` - Rebus closed the reply's descriptors instead of handing them over.
  - `{:error, :disconnected}` - the connection stopped before the reply, or
    before its descriptors transferred. A call carrying descriptors can return
    after `timeout`; see [Unix file descriptor passing](unix_fds.html).
  - Nothing was written for `:encode_failed`, `:no_reply_expected`, `:not_connected`,
    `:serial_exhausted`, `:unix_fd_unsupported`, `:unix_fd_not_negotiated`,
    `:unix_fd_send_failed`, `{:invalid_message_type, type}` and `:remote_connection_unsupported`.

  ## Examples

      message = Rebus.Message.new!(:method_call,
        path: "/org/freedesktop/DBus",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "ListNames"
      )

      {:ok, %Rebus.Message{type: :method_return, body: [names]}} = Rebus.call(conn, message)
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
  Subscribes the calling process to signals selected by a match rule.

  Signals matching the rule arrive as `{ref, %Rebus.Message{}}`. Build the rule
  with `Rebus.MatchRule.new/1`; raw strings are not accepted. Equivalent rules
  share one bus registration, and each call gets its own reference.

  `{:error, :timeout}` is ambiguous: you get no reference, but the bus may
  already hold the rule. `{:error, :sender_routing_ambiguous}` means the rule
  overlaps an existing one with a different sender. `{:error, :not_a_bus}`
  means the connection was opened with `bus: false`, and nothing was sent.

  Rebus closes the connection when too many ambiguous cleanups accumulate. A reference
  that failed with `{:error, :match_subscription_state_lost}` stays unresolved until the
  connection is closed.

  ## Return values

  Success is `{:ok, reference}`. Every failure is `{:error, reason}`: `:timeout`, `:not_a_bus`,
  `:sender_routing_ambiguous`, `:match_rule_cleanup_pending`, `:match_subscription_state_lost`,
  `{:bus_error, error_name}`, `:invalid_bus_reply`, `:not_connected`, `:disconnected`,
  `:encode_failed`, `:serial_exhausted`, `:fd_claim_expired`, `{:reply_dropped, outcome}` and
  `:remote_connection_unsupported`. The [match rules guide](match_rules.html) lists what each
  one means.

  ## Example

      rule = Rebus.MatchRule.new!(interface: "org.example.Status", member: "Changed")
      {:ok, ref} = Rebus.add_match(conn, rule)
      receive do
        {^ref, %Rebus.Message{type: :signal}} -> :ok
      end
  """
  @spec add_match(pid(), Rebus.MatchRule.t(), non_neg_integer()) ::
          {:ok, reference()} | {:error, match_error_reason()}
  def add_match(conn, %Rebus.MatchRule{} = rule, timeout \\ 5_000)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    Rebus.MatchSubscription.add(conn, rule, timeout)
  end

  @doc """
  Removes a match-rule subscription reference.

  Removing a reference is idempotent and scoped to its own connection. A removed
  reference receives no further signals, and the last reference for a rule also removes
  the rule from the bus. A removal that times out or fails keeps the reference for a
  retry, while Rebus clears the rule in the background. Rebus removes the reference and
  the bus rule when the owning process exits. Closing the connection discards both.

  ## Return values

  Success is `:ok`. Failures are the `{:error, reason}` shapes listed for
  `add_match/3`, apart from `:not_a_bus` and `:sender_routing_ambiguous`.
  """
  @spec remove_match(pid(), reference(), non_neg_integer()) ::
          :ok | {:error, match_error_reason()}
  def remove_match(conn, ref, timeout \\ 5_000)
      when is_pid(conn) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    Rebus.MatchSubscription.remove(conn, ref, timeout)
  end

  @doc """
  Registers the calling process to receive every signal on the connection.

  This asks the bus for nothing; use `add_match/3` to have the bus route more
  signals here. A connection can carry several handlers, each receiving every
  signal that arrives on it. Rebus removes a handler when its process exits or
  the connection closes. Call `delete_signal_handler/2` as soon as you stop
  wanting signals, so a busy bus does not fill the process mailbox.

  ## Return values

  - `{:ok, ref}` - the handler is registered.
  - `{:error, :not_connected}` - connection setup has not completed.
  - `{:error, :timeout}` - the connection did not service the request in time.
  - `{:error, :disconnected}` - the connection has stopped.

  ## Examples

      {:ok, ref} = Rebus.add_signal_handler(conn)

  Each signal then arrives in the calling process as:

      {^ref, %Rebus.Message{
        type: :signal,
        header_fields: %{path: path, interface: interface, member: member, sender: sender},
        body: [signal_args]
      }}
  """
  @spec add_signal_handler(pid()) ::
          {:ok, reference()} | {:error, :not_connected | :timeout | :disconnected}
  defdelegate add_signal_handler(conn), to: Rebus.Connection

  @doc """
  Stops a signal handler registered by `add_signal_handler/1`.

  The handler receives no further signals; others on the same connection carry
  on. Deleting a reference is idempotent while the connection is available.

  ## Return values

  - `:ok` - the handler is gone.
  - `{:error, :not_connected}` - connection setup has not completed.
  - `{:error, :timeout}` - the connection did not service the request in time.
  - `{:error, :disconnected}` - the connection has stopped.

  ## Examples

      {:ok, ref} = Rebus.add_signal_handler(conn)
      :ok = Rebus.delete_signal_handler(conn, ref)
  """
  @spec delete_signal_handler(pid(), reference()) ::
          :ok | {:error, :not_connected | :timeout | :disconnected}
  defdelegate delete_signal_handler(conn, ref), to: Rebus.Connection
end
