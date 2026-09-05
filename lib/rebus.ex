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

  ## Error Handling

  `connect/2` returns `{:ok, connection}` or `{:error, reason}`. `call/3`
  returns `{:ok, %Rebus.Message{}}` for a successful reply and
  `{:error, %Rebus.Message{type: :error}}` for a D-Bus error reply, while
  `send/2` and `send/3` return `:ok`. Public operation failures are returned as
  `{:error, reason}` tuples.

  ## Unix file descriptors

  On Linux and macOS local Unix sockets, Rebus requests D-Bus's optional
  `NEGOTIATE_UNIX_FD` extension during authentication. Pass outbound raw Unix
  descriptors as `fds: [...]` to `Rebus.Message.new/2`; they are borrowed and
  never closed by Rebus. A successfully delivered `call/3` reply can expose
  owned inbound descriptors in `message.unix_fds`, which the receiving caller
  must close exactly once with `Rebus.UnixFD.close/1` or adopt with an OTP/OS
  API. FD transfer is rejected over TCP or without peer agreement.

  Rebus closes and drops inbound FD-bearing signals: one raw descriptor cannot
  be safely shared across multiple signal subscribers. It keeps the connection
  alive after that complete-frame rejection. Outbound signals and method calls,
  and inbound method-call replies, are supported.

  ## Examples

      # Connect to session bus with default options
      {:ok, conn} = Rebus.connect(:session)

      # Connect to a Unix domain socket
      {:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/dbus-socket"})

  For more advanced usage, see the documentation for `Rebus.Message` and other
  modules in this package.
  """

  require Logger

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
          | {:reply_dropped, :method_return | {:error, binary()}}
          | {:bus_error, binary()}

  @default_system_bus_address "unix:path=/run/dbus/system_bus_socket"
  @default_connection_timeout 5_000
  @max_tcp_addresses_per_family 4
  @minimum_address_attempt_timeout 50

  @publicly_ignored_connection_options [
    :auth_id,
    :auth_id_fun,
    :auth_username_fun,
    :address_list_auth_id,
    :address_list_setup_timeout,
    :expected_guid,
    :precomputed_auth_id
  ]

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
      `{:error, :read_timeout}`. `connect/2` waits for the validated initial
      Hello reply before returning `{:ok, pid}`; that reply is bounded from the
      time Hello is sent and peer progress cannot extend the setup budget. Once
      established, it bounds
      gaps between inbound fragments, is reset whenever a peer makes progress,
      and is inactive while no frame is buffered. Expiry then terminates the
      temporary connection; inflight callers receive `{:error, :disconnected}`.
    - `:allow_anonymous` - Boolean, default `false`. When `true`, Rebus can
      use the peer-advertised D-Bus `ANONYMOUS` mechanism only after `EXTERNAL`
      is rejected and `DBUS_COOKIE_SHA1` was not advertised, or before cookie
      `AUTH` starts when the local username is unavailable. Once cookie `AUTH`
      starts or a cookie challenge is received, it is never selected; failures
      are terminal and Rebus sends neither `CANCEL` nor `AUTH ANONYMOUS`.
      `ANONYMOUS` provides no authentication, confidentiality, or integrity;
      use it only for deliberately unauthenticated peer-to-peer services.

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
    :invalid_allow_anonymous | :invalid_name}` - A connection option was invalid.
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
  candidate connections. Caller-provided auth-ID and address-list setup options
  are ignored; they cannot bypass this ownership or candidate identity checks.
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

  Rebus sends `AUTH EXTERNAL` first. On a valid bounded `REJECTED` list it
  prefers `DBUS_COOKIE_SHA1`, using the effective `id -un` username and only a
  private `$HOME/.dbus-keyrings/<context>` file owned by the effective UID.
  A final home symlink is followed only after its target directory is validated;
  the keyring directory and file must be non-symlink. The resolved home cannot
  be group/other writable and the keyring directory and file are inaccessible
  to group and other users. Unsupported owner/mode metadata fails closed.
  Cookie data, challenges, authorization identities, server GUIDs, and peer
  authentication payloads are not logged or returned. Cookie authentication can
  support local TCP endpoints where Unix credentials cannot be passed, but it
  provides no transport encryption or integrity.

  Rebus accepts either hex case from a peer or cookie file for interoperability,
  while always emitting the lower-case form required by DBUS_COOKIE_SHA1. Cookie
  contexts follow the D-Bus grammar and therefore cannot contain a period,
  whitespace, or path separator. Cookie reads accept at most 256 bounded lines,
  skipping malformed unrelated records but rejecting ambiguous target records.
  To bound peer-controlled state, a `REJECTED` list is limited to 64 mechanism
  names; larger lists are treated as malformed.

  `ANONYMOUS` remains disabled unless `allow_anonymous: true` is passed. It is
  appropriate only for intentionally unauthenticated peer-to-peer services and
  is not a safe message-bus or network trust mechanism.

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

  On Linux and macOS, local Unix-domain connections request the optional
  `NEGOTIATE_UNIX_FD` authentication extension. Construct a message with
  `fds: [fd, ...]` and use zero-based `h` body indexes. These outbound raw
  descriptors are borrowed: Rebus neither duplicates nor closes them. A live
  method reply may expose received descriptors in `message.unix_fds`; its
  receiving process owns and must close each raw descriptor exactly once with
  `Rebus.UnixFD.close/1` or adopt it using a suitable OTP/OS API. FD-bearing
  messages are rejected before writing over TCP or when the peer did not agree
  to negotiation. FD-bearing signals are closed and dropped because a
  descriptor cannot safely have multiple subscribers without a platform-level
  duplicate; this complete-frame rejection does not stop the connection.

  """
  @spec connect(address(), keyword()) :: {:ok, pid()} | {:error, term()}
  def connect(address, opts \\ [])

  def connect(:system, opts) do
    case Application.get_env(:rebus, :system_bus_address, @default_system_bus_address) do
      nil -> {:error, :no_system_bus_address}
      address -> connect_bus_address(address, opts)
    end
  end

  def connect(:session, opts) do
    case System.get_env("DBUS_SESSION_BUS_ADDRESS") do
      nil -> {:error, :no_session_bus_address}
      address -> connect_bus_address(address, opts)
    end
  end

  def connect(%{family: family} = addr, opts) when family in [:inet, :inet6, :local] do
    opts = strip_public_connection_options(opts)
    start_connection(addr, opts)
  end

  defp connect_address_candidate(%{family: family} = addr, opts)
       when family in [:inet, :inet6, :local] do
    case Keyword.pop(opts, :address_list_auth_id) do
      {auth_id, candidate_opts} when is_binary(auth_id) ->
        start_connection(addr, candidate_opts, auth_id)

      _missing_auth_id ->
        {:error, :invalid_bus_address_implementation}
    end
  end

  defp start_connection(addr, opts, precomputed_auth_id \\ nil) do
    connect_ref = make_ref()

    args =
      opts
      |> Keyword.put(:addr, addr)
      |> Keyword.put(:connect_waiter, {self(), connect_ref})

    args =
      if is_binary(precomputed_auth_id),
        do: Keyword.put(args, :precomputed_auth_id, precomputed_auth_id),
        else: args

    child_spec = {Rebus.Connection, args}

    case DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, child_spec) do
      {:ok, pid} -> await_connection(pid, connect_ref, Process.monitor(pid))
      {:error, {:already_started, pid}} -> name_collision(pid)
      other -> other
    end
  end

  defp strip_public_connection_options(opts) do
    Keyword.drop(opts, @publicly_ignored_connection_options)
  end

  defp connect_bus_address(address, opts) do
    case Rebus.BusAddress.parse(address) do
      {:ok, candidates} -> connect_address_candidates(candidates, opts)
      {:error, _reason} = error -> error
    end
  end

  @doc false
  @spec connect_address_candidates([Rebus.BusAddress.candidate()], keyword(), keyword()) ::
          {:ok, pid()} | {:error, term()}
  def connect_address_candidates(candidates, opts, implementation \\ [])
      when is_list(candidates) and is_list(opts) and is_list(implementation) do
    opts = strip_public_connection_options(opts)

    with {:ok, timeout} <- address_list_timeout(opts),
         {:ok, resolver} <- implementation_function(implementation, :resolver, 3, &resolve_tcp/3),
         {:ok, connector} <-
           implementation_function(implementation, :connector, 2, &connect_address_candidate/2),
         {:ok, auth_id_runner} <-
           implementation_function(
             implementation,
             :auth_id_runner,
             1,
             &Rebus.Connection.run_auth_id/1
           ),
         {:ok, monotonic_time} <-
           implementation_function(
             implementation,
             :monotonic_time,
             0,
             fn -> System.monotonic_time(:millisecond) end
           ) do
      deadline = monotonic_time.() + timeout

      case resolve_list_auth_id(candidates, deadline, auth_id_runner, monotonic_time) do
        {:ok, auth_id} ->
          connect_bus_candidates(
            candidates,
            Keyword.put(opts, :address_list_auth_id, auth_id),
            deadline,
            resolver,
            connector,
            monotonic_time,
            nil,
            1
          )

        {:error, {:address_list_timeout, reason}} ->
          {:error, reason}

        {:error, _reason} = error ->
          error
      end
    end
  end

  defp implementation_function(implementation, key, arity, default) do
    case Keyword.get(implementation, key, default) do
      function when is_function(function, arity) -> {:ok, function}
      _function -> {:error, :invalid_bus_address_implementation}
    end
  end

  defp resolve_list_auth_id([], _deadline, _auth_id_runner, _monotonic_time),
    do: {:error, :unsupported_bus_transport}

  defp resolve_list_auth_id(candidates, deadline, auth_id_runner, monotonic_time) do
    candidate_count = connectable_candidate_count(candidates)

    if candidate_count == 0 do
      {:error, :unsupported_bus_transport}
    else
      with {:ok, timeout} <-
             address_attempt_timeout(deadline, candidate_count + 1, monotonic_time, nil) do
        Rebus.Connection.get_auth_id(timeout, auth_id_runner)
      end
    end
  end

  defp connect_bus_candidates(
         [],
         _opts,
         _deadline,
         _resolver,
         _connector,
         _monotonic_time,
         nil,
         _candidate_ordinal
       ),
       do: {:error, :unsupported_bus_transport}

  defp connect_bus_candidates(
         [],
         _opts,
         _deadline,
         _resolver,
         _connector,
         _monotonic_time,
         error,
         _candidate_ordinal
       ),
       do: final_address_list_error(error)

  defp connect_bus_candidates(
         [:unsupported | candidates],
         opts,
         deadline,
         resolver,
         connector,
         monotonic_time,
         last_error,
         candidate_ordinal
       ) do
    connect_bus_candidates(
      candidates,
      opts,
      deadline,
      resolver,
      connector,
      monotonic_time,
      last_error,
      candidate_ordinal + 1
    )
  end

  defp connect_bus_candidates(
         [candidate | candidates],
         opts,
         deadline,
         resolver,
         connector,
         monotonic_time,
         last_error,
         candidate_ordinal
       ) do
    case connect_bus_candidate(
           candidate,
           candidates,
           opts,
           deadline,
           resolver,
           connector,
           monotonic_time,
           last_error,
           candidate_ordinal
         ) do
      {:ok, _pid} = result ->
        result

      {:error, {:address_list_timeout, reason}} ->
        {:error, reason}

      {:error, reason} = error ->
        if retryable_bus_address_error?(reason) do
          connect_bus_candidates(
            candidates,
            opts,
            deadline,
            resolver,
            connector,
            monotonic_time,
            reason,
            candidate_ordinal + 1
          )
        else
          error
        end
    end
  end

  defp connect_bus_candidate(
         {:local, path, expected_guid},
         remaining_candidates,
         opts,
         deadline,
         _resolver,
         connector,
         monotonic_time,
         last_error,
         candidate_ordinal
       ) do
    with {:ok, timeout} <-
           address_attempt_timeout(
             deadline,
             1 + connectable_candidate_count(remaining_candidates),
             monotonic_time,
             last_error
           ) do
      connect_with_address_diagnostic(
        connector,
        %{family: :local, path: path},
        opts
        |> Keyword.put(:address_list_setup_timeout, timeout)
        |> Keyword.put(:expected_guid, expected_guid),
        candidate_ordinal,
        0,
        :unix,
        timeout
      )
    end
  end

  defp connect_bus_candidate(
         {:tcp, host, port, family, expected_guid},
         remaining_candidates,
         opts,
         deadline,
         resolver,
         connector,
         monotonic_time,
         last_error,
         candidate_ordinal
       ) do
    with {:ok, addresses} <-
           resolve_tcp_addresses(
             host,
             family,
             deadline,
             resolver,
             monotonic_time,
             connectable_candidate_count(remaining_candidates),
             last_error,
             candidate_ordinal
           ) do
      connect_tcp_addresses(
        addresses,
        port,
        expected_guid,
        opts,
        deadline,
        connector,
        monotonic_time,
        connectable_candidate_count(remaining_candidates),
        last_error,
        candidate_ordinal,
        1
      )
    end
  end

  defp resolve_tcp_addresses(
         host,
         family,
         deadline,
         resolver,
         monotonic_time,
         remaining_candidate_count,
         last_error,
         candidate_ordinal
       ) do
    families = if family == :unspec, do: [:inet6, :inet], else: [family]

    resolve_tcp_families(
      host,
      families,
      deadline,
      resolver,
      monotonic_time,
      remaining_candidate_count,
      [],
      [],
      last_error,
      candidate_ordinal
    )
  end

  defp resolve_tcp_families(
         _host,
         [],
         _deadline,
         _resolver,
         _monotonic_time,
         _remaining_candidate_count,
         [],
         reasons,
         _last_error,
         _candidate_ordinal
       ),
       do: {:error, {:tcp_resolution_failed, List.first(reasons) || :no_addresses}}

  defp resolve_tcp_families(
         _host,
         [],
         _deadline,
         _resolver,
         _monotonic_time,
         _remaining_candidate_count,
         addresses,
         _reasons,
         _last_error,
         _candidate_ordinal
       ),
       do: {:ok, addresses}

  defp resolve_tcp_families(
         host,
         [family | families],
         deadline,
         resolver,
         monotonic_time,
         remaining_candidate_count,
         addresses,
         reasons,
         last_error,
         candidate_ordinal
       ) do
    with {:ok, timeout} <-
           address_attempt_timeout(
             deadline,
             length([family | families]) + remaining_candidate_count,
             monotonic_time,
             last_error
           ) do
      case resolver.(host, family, timeout) do
        {:ok, resolved} when is_list(resolved) ->
          resolved_addresses =
            resolved
            |> Enum.take(@max_tcp_addresses_per_family)
            |> Enum.map(&{family, &1})

          resolve_tcp_families(
            host,
            families,
            deadline,
            resolver,
            monotonic_time,
            remaining_candidate_count,
            addresses ++ resolved_addresses,
            reasons,
            last_error,
            candidate_ordinal
          )

        {:error, reason} ->
          safe_reason = safe_resolver_reason(reason)

          log_address_attempt(
            candidate_ordinal,
            0,
            :tcp,
            timeout,
            {:tcp_resolution_failed, safe_reason}
          )

          resolve_tcp_families(
            host,
            families,
            deadline,
            resolver,
            monotonic_time,
            remaining_candidate_count,
            addresses,
            [safe_reason | reasons],
            {:tcp_resolution_failed, safe_reason},
            candidate_ordinal
          )

        _other ->
          log_address_attempt(candidate_ordinal, 0, :tcp, timeout, :resolution_failed)

          resolve_tcp_families(
            host,
            families,
            deadline,
            resolver,
            monotonic_time,
            remaining_candidate_count,
            addresses,
            [:resolution_failed | reasons],
            {:tcp_resolution_failed, :resolution_failed},
            candidate_ordinal
          )
      end
    end
  end

  defp connect_tcp_addresses(
         [],
         _port,
         _expected_guid,
         _opts,
         _deadline,
         _connector,
         _monotonic_time,
         _remaining_candidate_count,
         nil,
         _candidate_ordinal,
         _ip_ordinal
       ),
       do: {:error, {:tcp_resolution_failed, :no_addresses}}

  defp connect_tcp_addresses(
         [],
         _port,
         _expected_guid,
         _opts,
         _deadline,
         _connector,
         _monotonic_time,
         remaining_candidate_count,
         error,
         _candidate_ordinal,
         _ip_ordinal
       )
       when remaining_candidate_count > 0,
       do: {:error, error}

  defp connect_tcp_addresses(
         [],
         _port,
         _expected_guid,
         _opts,
         _deadline,
         _connector,
         _monotonic_time,
         _remaining_candidate_count,
         error,
         _candidate_ordinal,
         _ip_ordinal
       ),
       do: final_address_list_error(error)

  defp connect_tcp_addresses(
         [{family, address} | addresses],
         port,
         expected_guid,
         opts,
         deadline,
         connector,
         monotonic_time,
         remaining_candidate_count,
         last_error,
         candidate_ordinal,
         ip_ordinal
       ) do
    with {:ok, timeout} <-
           address_attempt_timeout(
             deadline,
             length(addresses) + 1 + remaining_candidate_count,
             monotonic_time,
             last_error
           ) do
      case connect_with_address_diagnostic(
             connector,
             %{family: family, addr: address, port: port},
             opts
             |> Keyword.put(:address_list_setup_timeout, timeout)
             |> Keyword.put(:expected_guid, expected_guid),
             candidate_ordinal,
             ip_ordinal,
             :tcp,
             timeout
           ) do
        {:ok, _pid} = result ->
          result

        {:error, reason} = error ->
          if retryable_bus_address_error?(reason) do
            connect_tcp_addresses(
              addresses,
              port,
              expected_guid,
              opts,
              deadline,
              connector,
              monotonic_time,
              remaining_candidate_count,
              reason,
              candidate_ordinal,
              ip_ordinal + 1
            )
          else
            error
          end
      end
    end
  end

  defp connectable_candidate_count(candidates) do
    Enum.count(candidates, &connectable_candidate?/1)
  end

  defp connectable_candidate?({:local, _path, _expected_guid}), do: true
  defp connectable_candidate?({:tcp, _host, _port, _family, _expected_guid}), do: true
  defp connectable_candidate?(_candidate), do: false

  defp connect_with_address_diagnostic(
         connector,
         address,
         opts,
         candidate_ordinal,
         ip_ordinal,
         transport,
         timeout
       ) do
    case connector.(address, opts) do
      {:error, reason} = error ->
        log_address_attempt(candidate_ordinal, ip_ordinal, transport, timeout, reason)
        error

      result ->
        result
    end
  end

  defp log_address_attempt(candidate_ordinal, ip_ordinal, transport, timeout, reason) do
    safe_reason = safe_address_failure_reason(reason)

    Logger.debug(fn ->
      "D-Bus address attempt candidate=#{candidate_ordinal} ip=#{ip_ordinal} " <>
        "transport=#{transport} slice_ms=#{timeout} reason=#{safe_reason}"
    end)
  end

  defp address_attempt_timeout(deadline, attempt_count, monotonic_time, last_error) do
    case remaining_address_list_timeout(deadline, monotonic_time) do
      {:ok, remaining} ->
        {:ok, fair_address_attempt_timeout(remaining, attempt_count)}

      {:error, :read_timeout} ->
        {:error, {:address_list_timeout, address_list_timeout_error(last_error)}}
    end
  end

  defp fair_address_attempt_timeout(remaining, attempt_count) do
    attempt_count = max(attempt_count, 1)
    fair_share = max(1, div(remaining, attempt_count))

    floor =
      if remaining >= @minimum_address_attempt_timeout * attempt_count,
        do: @minimum_address_attempt_timeout,
        else: 1

    min(remaining, max(floor, fair_share))
  end

  defp final_address_list_error(:read_timeout), do: {:error, {:read_timeout, :read_timeout}}

  defp final_address_list_error({:read_timeout, reason}) do
    {:error, {:read_timeout, safe_address_failure_reason(reason)}}
  end

  defp final_address_list_error(error), do: {:error, error}

  defp address_list_timeout_error(nil), do: :read_timeout

  defp address_list_timeout_error(last_error) do
    {:read_timeout, safe_address_failure_reason(last_error)}
  end

  defp safe_address_failure_reason(reason) when is_atom(reason), do: reason
  defp safe_address_failure_reason({:tcp_resolution_failed, _reason}), do: :tcp_resolution_failed
  defp safe_address_failure_reason(_reason), do: :connection_failed

  defp address_list_timeout(opts) do
    case Keyword.fetch(opts, :read_timeout) do
      {:ok, timeout} when is_integer(timeout) and timeout > 0 ->
        {:ok, timeout}

      {:ok, _timeout} ->
        {:error, :invalid_read_timeout}

      :error ->
        case Keyword.get(opts, :timeout, @default_connection_timeout) do
          timeout when is_integer(timeout) and timeout > 0 -> {:ok, timeout}
          _timeout -> {:error, :invalid_timeout}
        end
    end
  end

  defp remaining_address_list_timeout(deadline, monotonic_time) do
    case deadline - monotonic_time.() do
      timeout when timeout > 0 -> {:ok, timeout}
      _timeout -> {:error, :read_timeout}
    end
  end

  defp resolve_tcp(host, family, timeout) do
    :inet.getaddrs(:binary.bin_to_list(host), family, timeout)
  catch
    _kind, _reason -> {:error, :resolution_failed}
  end

  defp safe_resolver_reason(reason) when is_atom(reason), do: reason
  defp safe_resolver_reason(_reason), do: :resolution_failed

  defp retryable_bus_address_error?(:invalid_timeout), do: false
  defp retryable_bus_address_error?(:invalid_read_timeout), do: false
  defp retryable_bus_address_error?(:invalid_write_timeout), do: false
  defp retryable_bus_address_error?(:invalid_allow_anonymous), do: false
  defp retryable_bus_address_error?(:invalid_name), do: false
  defp retryable_bus_address_error?(:invalid_auth_id_fun), do: false
  defp retryable_bus_address_error?(:invalid_auth_username_fun), do: false
  defp retryable_bus_address_error?(:auth_id_unavailable), do: false
  defp retryable_bus_address_error?(:auth_cookie_unavailable), do: false
  defp retryable_bus_address_error?(:guid_mismatch), do: false
  defp retryable_bus_address_error?({:read_timeout, _reason}), do: false
  defp retryable_bus_address_error?({:name_taken, _pid}), do: false
  defp retryable_bus_address_error?({:name_registered, _pid}), do: false
  defp retryable_bus_address_error?(_reason), do: true

  defp name_collision(pid) do
    if connection_child?(pid),
      do: {:error, {:name_taken, pid}},
      else: {:error, {:name_registered, pid}}
  end

  defp connection_child?(pid) do
    Enum.any?(DynamicSupervisor.which_children(Rebus.ConnectionSupervisor), fn
      {_id, ^pid, _type, _modules} -> true
      _child -> false
    end)
  catch
    :exit, _reason -> false
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
  A Unix-FD reply that reaches its private ownership handoff has a 100ms
  initial acknowledgement grace after the reply deadline. If that
  acknowledgement is already queued when the grace expires, `call/3` waits for
  the live connection to resolve it FIFO instead of returning an ambiguous
  ownership result; that wait has no separate fixed wall-clock limit.
  `{:error, :fd_claim_expired}` means Rebus definitively closed the still-owned
  received descriptors rather than exposing them after the claim deadline. A
  connection that stops during this resolution returns `{:error, :disconnected}`.
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

  Build the rule with `Rebus.MatchRule.new/1`; raw match strings are not
  accepted. Rules are canonical and connection-scoped: equivalent rules share
  one remote AddMatch registration, while each successful call returns an
  independent reference. The bus rule remains active until the last reference
  is removed or its owning process exits.

  The supplied timeout is a single budget for local handler installation and
  the AddMatch reply. `{:error, :timeout}` is delivery-ambiguous: no local
  subscription reference is returned, but the bus might already have installed
  the rule. Rebus starts bounded cleanup and retries transport-ambiguous
  removal with capped exponential backoff while the connection remains alive.
  Later subscriptions for the same rule wait behind that cleanup, subject to
  their own timeout; a completed AddMatch is committed only if its caller is
  still alive and within its deadline. D-Bus error replies become
  `{:error, {:bus_error, error_name}}`; reply bodies are intentionally not
  exposed. A well-known `sender` remains bus-owned because its forwarded
  header is normally the current unique owner. Because D-Bus does not identify
  the AddMatch rule that admitted a signal, Rebus rejects an overlapping rule
  with a different sender predicate as
  `{:error, :sender_routing_ambiguous}` rather than cross-delivering a signal
  to a sender-pinned subscription. Unique sender names are locally verified.

  Recovery tracks at most 64 distinct ambiguous rules per connection. Reaching
  that capacity closes the connection, which makes the bus discard its match
  rules, rather than retaining unbounded uncertain state. The first normal
  cleanup after an owner exits is separate from that ambiguity budget: up to
  16 initial removals run concurrently and the remainder queue safely.
  Definitive
  RemoveMatch errors are classified separately but still retry bounded cleanup:
  they cannot prove whether an earlier removal took effect. If a subscription
  worker restarts while an operation is in flight, Rebus returns
  `{:error, :match_subscription_state_lost}` until connection teardown clears
  the unresolved local and remote state; it never treats the reference as
  successfully removed based only on an absent worker map.

  Client-side filtering is applied only for the criteria accepted by
  `Rebus.MatchRule`: interface/member/path/destination headers, `argN`,
  `argNpath`, and `arg0namespace`. Sender matching is owned by the bus, since
  a well-known sender can be forwarded under its unique name. Rebus does not
  emulate eavesdropping or bus access policy.

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
  handler before it asks the bus to remove the final rule, so a successfully
  removed reference cannot receive a later signal. A handler-delete failure or
  timeout retains the reference for retry. If the final RemoveMatch returns a
  D-Bus error, the stopped reference is retained and can be retried. If it
  times out or disconnects, Rebus keeps the local handler stopped and retries
  bounded cleanup in the background. New subscriptions for that rule queue
  behind recovery and retain their own deadlines. A definitive D-Bus error
  keeps the stopped reference for an explicit retry; if its owner exits, Rebus
  keeps bounded background cleanup rather than treating remote state as absent.

  When the owner process exits, Rebus removes its local handler and performs a
  bounded best-effort RemoveMatch if it held the final reference. Closing the
  connection stops all local handlers and the bus discards every match rule for
  that connection; no RemoveMatch is attempted after teardown.
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
  or when the handler exits.

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
