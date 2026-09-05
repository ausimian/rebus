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

  Rebus supports connecting to different types of D-Bus endpoints:

  - `:system` - Connects to the system bus using the address specified in
     application config (see below) or the `/run/dbus/system_bus_socket` by default.
  - `:session` - Connects to the session bus using the address specified in
     the `DBUS_SESSION_BUS_ADDRESS` environment variable.
  - `%{family: :local, path: path}` - Unix domain socket connection to a local D-Bus daemon
  - `:socket.sockaddr_in()` or `:socket.sockaddr_in6()` - TCP/IP connection to a
    remote D-Bus daemon; for example,
    `%{family: :inet, addr: {127, 0, 0, 1}, port: 12345}`

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
  Establishes a connection to a D-Bus message bus.

  Creates a supervised connection process that handles D-Bus protocol communication.
  The connection automatically handles authentication and maintains the persistent
  connection to the specified D-Bus endpoint. This is the sole supported way to
  create a Rebus connection: do not start or manage a connection process
  directly. Release the returned PID with `close/1` when its lifecycle is
  complete.

  ## Parameters

  - `address` - The D-Bus endpoint to connect to:
    - `:system` - Connects to the system bus using the address specified in
       application config (see below) or the `/run/dbus/system_bus_socket` by default.
    - `:session` - Connects to the session bus using the address specified in
       the `DBUS_SESSION_BUS_ADDRESS` environment variable.
    - `%{family: :local, path: path}` - Unix domain socket connection to a local D-Bus daemon
    - `:socket.sockaddr_in()` or `:socket.sockaddr_in6()` - TCP/IP connection to
      a remote D-Bus daemon; for example,
      `%{family: :inet, addr: {127, 0, 0, 1}, port: 12345}`

  - `opts` - Optional keyword list of connection options:
    - `:timeout` - Positive maximum time in milliseconds for the auth-ID lookup,
      each socket connect, and authentication read (default: 5000). This is the original
      public connection-timeout option. It has no effect after authentication.
      `:read_timeout`, when supplied, takes precedence for setup as well.
      Direct socket addresses retain independent auth-ID lookup and socket
      connect budgets. After connecting, all D-Bus authentication exchanges and
      `BEGIN` share one setup budget; each authentication write is also capped
      by `:write_timeout`. The validated initial Hello reply separately uses
      `:read_timeout`.
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
      `{:error, :read_timeout}`. For bus connections, `connect/2` waits for the
      validated initial
      Hello reply before returning `{:ok, pid}`; that reply is bounded from the
      time Hello is sent and peer progress cannot extend the setup budget. Once
      established, it bounds
      gaps between inbound fragments, is reset whenever a peer makes progress,
      and is inactive while no frame is buffered. Expiry then terminates the
      temporary connection; inflight callers receive `{:error, :disconnected}`.
    - `:allow_anonymous` - Boolean, default `false`. When `true`, Rebus may
      use the peer-advertised `ANONYMOUS` mechanism, which authenticates
      nothing and also requires `bus: false`. See
      [Authentication](authentication.html).
    - `:bus` - Boolean, default `true`. Pass `false` for a peer-to-peer
      endpoint that is not a message bus. Rebus then sends no Hello, the
      connection has no unique name, and `add_match/3` returns
      `{:error, :not_a_bus}`. It is not allowed with `:system` or `:session`,
      which are message buses by definition.

  ## Return Values

  - `{:ok, pid}` - Returns the PID of the connection process
  - `{:error, :read_timeout}` - Socket setup or authentication did not finish
    within its configured per-operation budget.
  - `{:error, :auth_id_unavailable}` - The local numeric identity required for
    `EXTERNAL` authentication could not be obtained.
  - `{:error, :auth_cookie_unavailable}` - The peer offered
    `DBUS_COOKIE_SHA1`, but its local username or matching local cookie could
    not be read safely. For a bus address list this is terminal and no later
    candidate address or IP is attempted.
  - `{:error, :auth_failed}` - The peer sent malformed authentication data or
    rejected a DBUS_COOKIE_SHA1 response. Neither error includes peer data.
  - `{:error, :guid_mismatch}` - A configured bus-address GUID did not match
    the server's `AUTH OK` GUID. Rebus does not try another address or IP after
    this identity failure.
  - `{:error, {:auth_rejected, mechanisms}}` - The peer rejected an attempted
    mechanism and no safe advertised fallback remains. `mechanisms` is bounded
    and contains only validated mechanism names.
  - `{:error, {:hello_failed, :invalid_unique_name}}` - The peer's initial
    Hello reply did not contain a valid D-Bus unique name.
  - `{:error, {:hello_failed, :resource_limit}}` - The peer's initial Hello
    reply exceeded a local decoding safety cap.
  - `{:error, :invalid_timeout | :invalid_read_timeout | :invalid_write_timeout |
    :invalid_allow_anonymous | :invalid_bus_option | :invalid_name}` - A
    connection option was invalid.
  - `{:error, {:name_taken, pid}}` - The requested local name is held by a
    setup or established connection process. The PID can be adopted or passed to
    `close/1` when it is no longer needed.
  - `{:error, {:name_registered, pid}}` - The requested local name belongs to
    another process, not a supervised Rebus connection.
  - `{:error, {:invalid_bus_address, reason}}` - The configured system or
    session address was malformed. `reason` is a stable atom and never includes
    the address value.
  - `{:error, :unsupported_bus_transport}` - The configured address list did
    not contain a transport Rebus supports.
  - `{:error, {:tcp_resolution_failed, reason}}` - A TCP address resolved to
    no usable IP address. `reason` is a stable atom and never includes the
    configured host name.
  - `{:error, {:read_timeout, reason}}` - An address-list deadline elapsed
    after at least one setup attempt. `reason` is a stable, payload-free atom
    describing the last failed attempt. Before any attempt, expiry remains
    `{:error, :read_timeout}`.
  - `{:error, reason}` - Another socket or setup failure occurred.

  ## Bus address lists

  System and session address strings use the D-Bus
  `transport:key=value;next-transport:key=value` format. Rebus supports
  `unix:path=...`, `unix:abstract=...`, and `tcp:host=...,port=...` (with an
  optional `family=ipv4` or `family=ipv6`). Without `family`, Rebus resolves
  IPv6 addresses first and IPv4 addresses second, preserving each resolver
  result order and duplicates, trying at most the first four results per family
  before the next D-Bus entry. Values are percent-decoded; literal non-NUL bytes
  are accepted where they are not address separators. A 32-hex-digit `guid` is
  ignored for socket selection but compared case-insensitively with the server's
  `AUTH OK` GUID before `BEGIN` or Hello. A mismatch is `:guid_mismatch` and is
  never retried. Other syntactically valid transport parameters are ignored for
  forward compatibility. Parameterless unknown transports (for example
  `autolaunch:`) are skipped, while `unix:`, `unix:path=`, `unix:guid=...`, and
  `tcp:`/`tcp:family=...` return their missing-required-field errors. Rebus tries
  supported entries in their listed order until one establishes a connection;
  syntactically valid unsupported entries are skipped. A malformed entry rejects
  the whole list, and if every supported attempt fails the final attempt's error
  is returned.

  Rebus obtains the local `EXTERNAL` auth ID once in the calling process before
  it begins a supported address list, then privately supplies that value to all
  candidate connections. That value, and the per-candidate setup budget and
  expected GUID, reach each candidate outside the caller's options, so they
  cannot be supplied or overridden by the caller.
  For an address list only, `:timeout` (or `:read_timeout` when supplied) is
  one aggregate budget for DNS lookup and pre-Hello socket/authentication setup
  across all candidates. Each resolver and pre-Hello setup attempt gets
  `min(remaining, max(floor, floor(remaining / outstanding_attempts)))`
  milliseconds. The floor is 50 ms when the remaining budget can grant every
  outstanding attempt 50 ms; otherwise it is 1 ms, and it never exceeds the
  remaining time. Before DNS completes, outstanding attempts are resolver
  families plus later D-Bus entries; afterward they are the capped resolved IPs
  plus later entries. This does not change the independent `:write_timeout` or
  the normal `:read_timeout` budget for an initial Hello reply after a candidate
  has authenticated. Address-list failure diagnostics are emitted only at debug
  level and contain candidate/IP ordinals, transport, slice, and a bounded
  reason—never an address, host, path, or GUID.

  ## Authentication mechanisms

  Rebus tries `EXTERNAL` first, then `DBUS_COOKIE_SHA1`, and `ANONYMOUS` only
  with `allow_anonymous: true`. See [Authentication](authentication.html).

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

  ## Unix file descriptors

  A local Unix-socket connection negotiates descriptor passing with the peer
  during authentication. See
  [Unix file descriptor passing](unix_fds.html).
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

  `call/3` accepts only method calls that expect replies and returns the complete
  reply as `{:ok, %Rebus.Message{type: :method_return}}`. A D-Bus error reply is
  returned as `{:error, %Rebus.Message{type: :error}}` so callers can inspect its
  `:error_name` header and body. Both shapes carry the complete message,
  including any received descriptors in `:unix_fds`, which the caller owns and
  must close exactly once. If no reply arrives before `timeout` milliseconds,
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
  `{:error, :unix_fd_send_failed}` means an outbound borrowed descriptor could
  not be passed before any bytes of that frame were accepted; the connection
  remains usable and the descriptor remains owned by its sender.
  A reply carrying descriptors can return after `timeout`, because `call/3`
  waits for descriptor ownership to be settled rather than left undecided.
  `{:error, :fd_claim_expired}` then means Rebus closed those descriptors, and
  `{:error, :disconnected}` that the connection stopped first; see
  [Unix file descriptor passing](unix_fds.html).
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
    do: Rebus.Connection.dispatch(conn, message)

  @doc """
  Sends a message with a custom dispatch timeout in milliseconds.

  A timeout is delivery-ambiguous: the message may already have reached the peer.
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
