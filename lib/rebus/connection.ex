defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.Connection.Dispatch
  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Handshake
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.InboundCalls
  alias Rebus.Connection.Objects
  alias Rebus.Connection.Pending
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Service
  alias Rebus.Connection.Setup
  alias Rebus.Connection.SocketError
  alias Rebus.Connection.Writer
  alias Rebus.Connection.Writer.Active
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.SafeCall
  alias Rebus.UnixFD
  alias Rebus.WireValue
  require Logger

  @default_write_timeout 5_000
  @default_read_timeout 5_000
  @default_max_pending_calls 1_024

  # The smallest accepted `:max_message_size`. A message-bus `Hello` reply and
  # the `NameAcquired` signal after it fit well inside this even with the
  # longest legal unique name, so no accepted cap can make setup impossible.
  @min_max_message_size 4_096
  @max_max_message_size Message.max_message_size()

  # The settings a connection carries verbatim into its state. Most are read
  # only while the connection is being set up.
  @setting_fields [
    :impl,
    :write_timeout,
    :read_timeout,
    :setup_timeout,
    :expected_guid,
    :precomputed_auth_id,
    :allow_anonymous?,
    :bus?,
    :connect_waiter,
    :owner,
    :max_pending_calls
  ]

  # The peer-supplied reasons a connection reports verbatim. Each list is
  # spelled once and serves both the type below it and the guard in
  # `sanitize_protocol_reason/1` that admits it; every other reason becomes
  # `:protocol_error` or `:invalid_error_name`.
  @protocol_reasons [
    :insufficient_data,
    :invalid_endianness,
    :invalid_message,
    :invalid_message_type,
    :invalid_unix_fds,
    :unix_fd_truncated,
    :message_too_large,
    :read_timeout,
    :resource_limit,
    :unsupported_protocol_version
  ]

  @hello_failed_reasons [
    :missing_unique_name,
    :missing_error_name,
    :invalid_error_name,
    :invalid_unique_name,
    :resource_limit
  ]

  @type protocol_reason ::
          unquote(Enum.reduce(Enum.reverse(@protocol_reasons), &{:|, [], [&1, &2]}))

  @type hello_failed_reason ::
          unquote(Enum.reduce(Enum.reverse(@hello_failed_reasons), &{:|, [], [&1, &2]}))

  @spec call(pid(), Message.t(), non_neg_integer()) ::
          {:ok, Message.t()} | {:error, Rebus.call_error()}
  def call(pid, %Message{} = msg, timeout)
      when is_pid(pid) and is_integer(timeout) and timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + timeout

      call_for_reply(pid, msg, deadline, request_ref, timeout)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  defp call_for_reply(pid, msg, deadline, request_ref, timeout) do
    SafeCall.call(pid, {:call, msg, deadline, request_ref}, timeout,
      cancel: {:cancel, request_ref},
      then: &FDClaims.Client.receive_claim(&1, pid, deadline)
    )
  end

  # The dispatch timeout bounds how long the caller waits for this connection
  # to accept the message. It is distinct from `:write_timeout`, which bounds
  # how long the socket has to accept the bytes of a frame.
  @spec dispatch(pid(), Message.t(), non_neg_integer()) :: :ok | {:error, term()}
  def dispatch(pid, %Message{} = msg, dispatch_timeout \\ @default_write_timeout)
      when is_pid(pid) and is_integer(dispatch_timeout) and dispatch_timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + dispatch_timeout

      call_for_dispatch(pid, msg, deadline, request_ref, dispatch_timeout)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  defp call_for_dispatch(pid, msg, deadline, request_ref, dispatch_timeout) do
    SafeCall.call(pid, {:send, msg, deadline, request_ref}, dispatch_timeout,
      cancel: {:cancel, request_ref}
    )
  end

  # Answers the inbound call `ref` with a `:method_return` or `:error` frame
  # its handler built. The call must still be pending, and the frame is
  # addressed from what the connection retained for it, so a handler can only
  # answer a call it was handed. It takes the `:send` path with the handler as
  # the waiting caller, so `:ok` means the frame was handed to the socket and
  # the dispatch timeout gives the handler backpressure. `dispatch/3` keeps
  # refusing these types, so callers cannot forge replies through
  # `Rebus.send/2`.
  @doc false
  @spec reply_call(pid(), reference(), Message.t(), non_neg_integer()) :: :ok | {:error, term()}
  def reply_call(pid, ref, %Message{} = msg, dispatch_timeout \\ @default_write_timeout)
      when is_pid(pid) and is_reference(ref) and is_integer(dispatch_timeout) and
             dispatch_timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + dispatch_timeout

      SafeCall.call(pid, {:reply_call, ref, msg, deadline, request_ref}, dispatch_timeout,
        cancel: {:cancel, request_ref}
      )
    else
      {:error, :remote_connection_unsupported}
    end
  end

  # Completes the inbound call `ref`, whose caller asked for no reply, without
  # sending anything. Only a call that carried descriptors is recorded, and
  # completing it releases them.
  @doc false
  @spec complete_call(pid(), reference(), non_neg_integer()) :: :ok | {:error, term()}
  def complete_call(pid, ref, timeout)
      when is_pid(pid) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    if node(pid) == node() do
      SafeCall.call(pid, {:complete_call, ref}, timeout)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  # Takes ownership of the descriptors the inbound call `ref` borrowed. The
  # connection hands them over as it hands over a reply's to `call/3`,
  # through an FD claim, so they move only when the claim is acknowledged
  # and are closed by the connection on any other outcome.
  @doc false
  @spec take_call_fds(pid(), reference(), non_neg_integer()) ::
          {:ok, Message.t()} | {:error, term()}
  def take_call_fds(pid, ref, timeout)
      when is_pid(pid) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + timeout

      SafeCall.call(pid, {:take_call_fds, ref, deadline, request_ref}, timeout,
        cancel: {:cancel, request_ref},
        then: &FDClaims.Client.receive_claim(&1, pid, deadline)
      )
    else
      {:error, :remote_connection_unsupported}
    end
  end

  # Makes a local call to an object this connection exports, built and
  # checked by `Rebus.Service.invoke/7`. It is routed, bounded and recorded
  # as a call from a peer would be, and answered with what `call/3` would
  # return for the same reply, but nothing is written to the socket. A caller
  # that stops waiting withdraws the call.
  @doc false
  @spec invoke(pid(), Message.t(), non_neg_integer()) ::
          {:ok, Message.t()} | {:error, {:dbus_error, Message.t()} | term()}
  def invoke(pid, %Message{type: :method_call} = msg, timeout)
      when is_pid(pid) and is_integer(timeout) and timeout >= 0 do
    if node(pid) == node() do
      request_ref = make_ref()

      SafeCall.call(pid, {:invoke, msg, request_ref}, timeout, cancel: {:cancel, request_ref})
    else
      {:error, :remote_connection_unsupported}
    end
  end

  # Exports an object, validated by `Rebus.Service`. The reference is made
  # here rather than by the connection so that a caller that stops waiting
  # can withdraw an export it never learned it had.
  @doc false
  @spec export(pid(), map(), non_neg_integer()) ::
          {:ok, reference()}
          | {:error,
             :not_connected
             | :timeout
             | :disconnected
             | :remote_connection_unsupported
             | {:already_exported, binary(), binary()}}
  def export(conn, spec, timeout)
      when is_pid(conn) and is_map(spec) and is_integer(timeout) and timeout >= 0 do
    if node(conn) == node() do
      ref = make_ref()

      safe_setup_call(conn, {:export, Map.put(spec, :ref, ref)}, {:unexport, ref}, timeout)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  @doc false
  @spec unexport(pid(), reference(), non_neg_integer()) ::
          :ok
          | {:error, :not_connected | :timeout | :disconnected | :remote_connection_unsupported}
  def unexport(conn, ref, timeout)
      when is_pid(conn) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    if node(conn) == node() do
      safe_setup_call(conn, {:unexport, ref}, nil, timeout)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  # The unique name Hello returned, or `nil` on a connection created with
  # `bus: false`, which sends no Hello.
  @doc false
  @spec unique_name(pid(), non_neg_integer()) ::
          {:ok, binary() | nil} | {:error, :timeout | :disconnected | :not_connected}
  def unique_name(conn, timeout \\ @default_read_timeout)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    safe_setup_call(conn, :unique_name, nil, timeout)
  end

  # Whether this connection completed the message-bus handshake. A connection
  # created with `bus: false` never sends Hello, so bus-driver methods such as
  # AddMatch cannot be served. The answer is fixed for the connection's life,
  # which lets callers cache it instead of asking per operation.
  @doc false
  @spec bus?(pid(), non_neg_integer()) ::
          boolean() | {:error, :timeout | :disconnected | :not_connected}
  def bus?(conn, timeout \\ @default_read_timeout)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    safe_setup_call(conn, :bus?, nil, timeout)
  end

  @spec add_signal_handler(pid()) ::
          {:ok, reference()} | {:error, :timeout | :disconnected | :not_connected}
  def add_signal_handler(conn) when is_pid(conn) do
    handler_ref = make_ref()

    safe_setup_call(
      conn,
      {:add_signal_handler, self(), handler_ref},
      {:cancel_signal_handler, handler_ref}
    )
  end

  # Match subscriptions install the local handler before their AddMatch method
  # call. That ordering closes the otherwise unavoidable race where the bus
  # accepts a rule and immediately forwards its first signal. This narrow
  # internal API preserves the existing all-signal public handler API.
  @doc false
  @spec add_signal_handler(pid(), pid(), reference(), MatchRule.t(), non_neg_integer()) ::
          {:ok, reference()} | {:error, :timeout | :disconnected | :not_connected}
  def add_signal_handler(conn, subscriber, handler_ref, %MatchRule{} = rule, timeout)
      when is_pid(conn) and is_pid(subscriber) and is_reference(handler_ref) and
             is_integer(timeout) and timeout >= 0 do
    safe_setup_call(
      conn,
      {:add_signal_handler, subscriber, handler_ref, rule},
      {:cancel_signal_handler, handler_ref},
      timeout
    )
  end

  # Owner tracking for the well-known names subscriptions depend on. The bus
  # round trips that answer these belong to the match-subscription worker; the
  # connection only holds the table dispatch reads.
  #
  # Marks a name tracked, resetting whatever the table held for it.
  @doc false
  @spec track_name_owner(pid(), binary(), non_neg_integer()) ::
          :ok | {:error, :timeout | :disconnected | :not_connected}
  def track_name_owner(conn, name, timeout)
      when is_pid(conn) and is_binary(name) and is_integer(timeout) and timeout >= 0 do
    safe_setup_call(conn, {:track_name_owner, name}, nil, timeout)
  end

  # Records the owner a GetNameOwner reply reported, but only while the name is
  # still unseeded: a NameOwnerChanged that arrived first is the newer fact.
  @doc false
  @spec seed_name_owner(pid(), binary(), binary() | nil, non_neg_integer()) ::
          :ok | {:error, :timeout | :disconnected | :not_connected}
  def seed_name_owner(conn, name, owner, timeout)
      when is_pid(conn) and is_binary(name) and (is_binary(owner) or is_nil(owner)) and
             is_integer(timeout) and timeout >= 0 do
    safe_setup_call(conn, {:seed_name_owner, name, owner}, nil, timeout)
  end

  @doc false
  @spec untrack_name_owner(pid(), binary(), non_neg_integer()) ::
          :ok | {:error, :timeout | :disconnected | :not_connected}
  def untrack_name_owner(conn, name, timeout)
      when is_pid(conn) and is_binary(name) and is_integer(timeout) and timeout >= 0 do
    safe_setup_call(conn, {:untrack_name_owner, name}, nil, timeout)
  end

  @spec delete_signal_handler(pid(), reference()) ::
          :ok | {:error, :timeout | :disconnected | :not_connected}
  def delete_signal_handler(conn, ref) when is_pid(conn) and is_reference(ref) do
    safe_setup_call(conn, {:delete_signal_handler, ref})
  end

  @doc false
  @spec delete_signal_handler(pid(), reference(), non_neg_integer()) ::
          :ok | {:error, :timeout | :disconnected | :not_connected}
  def delete_signal_handler(conn, ref, timeout)
      when is_pid(conn) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    safe_setup_call(conn, {:delete_signal_handler, ref}, nil, timeout)
  end

  # A handler that turns the bus driver's directed NameAcquired and NameLost
  # signals for `name` into `{:rebus_name, conn, :acquired | :lost, name}` for
  # `owner`. `Rebus.Bus` installs it before RequestName, since the driver
  # sends NameAcquired ahead of the method return. One owner watches a name at
  # most once, so a repeated request does not double its notices: `:existing`
  # means the owner was already watching and `handler_ref` was not used.
  @doc false
  @spec watch_name(pid(), pid(), binary(), non_neg_integer()) ::
          {:ok, :added | :existing}
          | {:error, :not_a_bus | :timeout | :disconnected | :not_connected}
  def watch_name(conn, owner, name, timeout)
      when is_pid(conn) and is_pid(owner) and is_binary(name) and is_integer(timeout) and
             timeout >= 0 do
    handler_ref = make_ref()

    safe_setup_call(
      conn,
      {:watch_name, owner, name, handler_ref},
      {:cancel_signal_handler, handler_ref},
      timeout
    )
  end

  # Removes the watch `owner` holds on `name`, or every watch on `name` for
  # `:all`, without waiting for the connection to answer: it cannot fail
  # while the connection lives, and runs after a caller's budget may be
  # spent. Being sent from the caller, it is handled after any `watch_name/4`
  # that caller made before it. Removing a watch that is already gone is a
  # no-op.
  @doc false
  @spec unwatch_name(pid(), binary(), pid() | :all) :: :ok
  def unwatch_name(conn, name, owner)
      when is_pid(conn) and is_binary(name) and (is_pid(owner) or owner == :all),
      do: GenServer.cast(conn, {:unwatch_name, name, owner})

  # Removes a handler without waiting for the connection to answer, for a
  # match subscription that has stopped relying on its calls. Removing a
  # handler that is already gone is a no-op.
  @doc false
  @spec cancel_signal_handler(pid(), reference()) :: :ok
  def cancel_signal_handler(conn, ref) when is_pid(conn) and is_reference(ref),
    do: GenServer.cast(conn, {:cancel_signal_handler, ref})

  # Forwards a match-subscription notice to the connection's `:owner`, and
  # drops it for a connection without one. The owner is only known here, and a
  # cast keeps a slow or stalled connection from blocking the worker that
  # reports the notice.
  @doc false
  @spec notify_owner(pid(), term()) :: :ok
  def notify_owner(conn, message) when is_pid(conn),
    do: GenServer.cast(conn, {:notify_owner, message})

  # Names the calling match-subscription worker, which has already linked
  # itself to the connection, so the connection can tell its exit apart from
  # any other. A cast follows the link from the same process, so it is always
  # handled before the `:EXIT` the worker's exit sends.
  @doc false
  @spec attach_match_worker(pid()) :: :ok
  def attach_match_worker(conn) when is_pid(conn),
    do: GenServer.cast(conn, {:attach_match_worker, self()})

  # Connections are started with the caller's public options and, separately,
  # the internal arguments Rebus itself computes: the resolved address, the
  # connect waiter, the address-list auth ID, per-candidate setup timeout and
  # expected GUID, and the implementation modules. Keeping them apart means no
  # internal name has to be reserved in, or stripped from, the public keyword
  # list.
  @doc false
  @spec start_link({keyword(), map()}) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link({opts, internal} = args) when is_list(opts) and is_map(internal) do
    case Keyword.get(opts, :name) do
      nil -> GenServer.start_link(__MODULE__, args)
      name when is_atom(name) -> GenServer.start_link(__MODULE__, args, name: name)
      _name -> {:error, :invalid_name}
    end
  end

  typedstruct enforce: true do
    field :sock, :socket.socket()
    field :guid, binary() | nil, default: nil
    # The pending receive select handle, or nil when no receive is outstanding.
    field :rref, :socket.select_handle() | nil, default: nil
    field :inbound, Inbound.t(), default: Inbound.new()
    # SCM_RIGHTS descriptors received with the frame currently being assembled,
    # and the taint bit recording ancillary data rejected before that frame's
    # boundary was known.
    field :inbound_fds, Rights.t(), default: Rights.new()
    field :name, binary() | nil, default: nil
    field :hello_serial, non_neg_integer() | nil, default: nil
    field :established?, boolean(), default: false
    field :write_timeout, pos_integer(), default: @default_write_timeout
    field :read_timeout, pos_integer(), default: @default_read_timeout
    field :setup_timeout, pos_integer(), default: @default_read_timeout
    field :expected_guid, binary() | nil, default: nil
    field :precomputed_auth_id, binary() | nil, default: nil
    field :allow_anonymous?, boolean(), default: false
    field :bus?, boolean(), default: true
    field :connect_waiter, {pid(), reference()} | nil, default: nil
    field :connect_waiter_monitor, reference() | nil, default: nil
    field :connect_accepted?, boolean(), default: false
    # The process this connection's lifetime is bound to, and the monitor that
    # reports its exit. Unlike the connect waiter, whose monitor is released
    # once setup is accepted, this one lives as long as the connection does.
    field :owner, pid() | nil, default: nil
    field :owner_monitor, reference() | nil, default: nil
    field :partial_frame_timer, {reference(), reference()} | nil, default: nil
    field :unix_fd_transport?, boolean(), default: false
    field :unix_fd_negotiated?, boolean(), default: false
    # `nil` until org.freedesktop.DBus.Peer.GetMachineId is first served;
    # `:unavailable` caches a definitive negative lookup.
    field :machine_id, binary() | :unavailable | nil, default: nil
    # The outbound write queue, the frame currently being written, and the
    # serial counter that numbers them.
    field :writer, Writer.t(), default: Writer.new()

    # The method calls waiting for a reply, and the indexes that find one by
    # request reference or by the monitor on its caller.
    field :pending, Pending.t(), default: Pending.new()
    # Replies whose descriptors have been handed to a caller but not yet
    # acknowledged, and the terminal outcomes retained just after.
    field :fd_claims, FDClaims.t(), default: FDClaims.new()
    # Inbound method calls handed to a handler and not yet answered, with the
    # descriptors they carried, and how many may wait at once.
    field :inbound_calls, InboundCalls.t(), default: InboundCalls.new()
    field :max_pending_calls, pos_integer(), default: @default_max_pending_calls
    # The objects this connection exports, and the monitors on their handlers.
    field :objects, Objects.t(), default: Objects.new()
    # Signal handlers registered against this connection. Delivery and match
    # filtering happen in the connection process itself, so a signal arriving
    # here can never reach a handler installed on another connection. A
    # handler with a `:name` is a name watch, which `watch_name/4` installs.
    field :handlers,
          %{
            reference() => %{
              required(:pid) => pid(),
              required(:monitor_ref) => reference(),
              required(:rule) => MatchRule.t() | nil,
              optional(:name) => binary()
            }
          },
          default: %{}

    field :handler_monitor_index, %{reference() => reference()}, default: %{}
    # The current owner of every well-known name a subscription depends on, as
    # the bus reports it. `:unknown` is tracked but not yet seeded, `nil` is a
    # name nobody owns, and a binary is the owner's unique name. Matching a
    # directed signal needs this answer synchronously, so it lives here rather
    # than in the process that performs the bus round trips.
    field :name_owners, %{binary() => :unknown | nil | binary()}, default: %{}
    # The match-subscription worker linked to this connection, once it has
    # attached. It holds the only record of the match rules and handlers it
    # installed here, so its exit stops this connection.
    field :match_worker, pid() | nil, default: nil
    # Implementation modules behind the connection's side effects. Production
    # always uses the defaults; tests substitute a module rather than reaching
    # into per-operation state.
    field :impl, Rebus.Impl.t(), default: Rebus.Impl.default()
  end

  @impl true
  def init({opts, internal}) do
    %{family: family} = addr = Map.fetch!(internal, :addr)
    settings = init_settings(opts, internal)

    case validate_settings(settings) do
      :ok -> open_socket(settings, family, addr)
      {:stop, _reason} = stop -> stop
    end
  end

  defp init_settings(opts, internal) do
    timeout = Keyword.get(opts, :timeout, @default_read_timeout)

    %{
      write_timeout: Keyword.get(opts, :write_timeout, @default_write_timeout),
      timeout: timeout,
      read_timeout: Keyword.get(opts, :read_timeout, @default_read_timeout),
      setup_timeout: Map.get(internal, :setup_timeout, Keyword.get(opts, :read_timeout, timeout)),
      expected_guid: Map.get(internal, :expected_guid),
      precomputed_auth_id: Map.get(internal, :precomputed_auth_id),
      allow_anonymous?: Keyword.get(opts, :allow_anonymous, false),
      bus?: Keyword.get(opts, :bus, true),
      name: Keyword.get(opts, :name),
      connect_waiter: Map.get(internal, :connect_waiter),
      owner: Keyword.get(opts, :owner),
      max_message_size: Keyword.get(opts, :max_message_size, Message.max_message_size()),
      max_pending_calls: Keyword.get(opts, :max_pending_calls, @default_max_pending_calls),
      impl: Map.get_lazy(internal, :impl, &Rebus.Impl.default/0)
    }
  end

  # The order of these clauses fixes which error a caller sees when more than
  # one option is invalid, so they are checked one at a time rather than
  # collected.
  defp validate_settings(settings) do
    with :ok <- validate_timeout(:invalid_write_timeout, settings.write_timeout),
         :ok <- validate_timeout(:invalid_timeout, settings.timeout),
         :ok <- validate_timeout(:invalid_read_timeout, settings.read_timeout),
         :ok <- validate_timeout(:invalid_setup_timeout, settings.setup_timeout),
         :ok <- validate_expected_guid(settings.expected_guid),
         :ok <- validate_precomputed_auth_id(settings.precomputed_auth_id),
         :ok <- validate_flag(:invalid_allow_anonymous, settings.allow_anonymous?),
         :ok <- validate_flag(:invalid_bus_option, settings.bus?),
         :ok <- validate_owner(settings.owner),
         :ok <- validate_max_message_size(settings.max_message_size),
         :ok <- validate_max_pending_calls(settings.max_pending_calls) do
      validate_registered_name(settings.name)
    end
  end

  defp validate_timeout(_reason, value) when is_integer(value) and value > 0, do: :ok
  defp validate_timeout(reason, _value), do: {:stop, reason}

  defp validate_expected_guid(nil), do: :ok

  defp validate_expected_guid(guid),
    do: if(Handshake.valid_guid?(guid), do: :ok, else: {:stop, :invalid_expected_guid})

  defp validate_precomputed_auth_id(nil), do: :ok
  defp validate_precomputed_auth_id(auth_id) when is_binary(auth_id), do: :ok
  defp validate_precomputed_auth_id(_auth_id), do: {:stop, :invalid_precomputed_auth_id}

  defp validate_flag(_reason, value) when is_boolean(value), do: :ok
  defp validate_flag(reason, _value), do: {:stop, reason}

  # An owner is monitored, and a monitor on a remote process reports node
  # failures as well as process exits. Binding a connection's life to that is
  # a different contract from the local one this option promises, so a remote
  # PID is refused rather than silently accepted. The remote clause is
  # unexercised by the suite, which runs on a single, undistributed node.
  defp validate_owner(nil), do: :ok
  defp validate_owner(owner) when is_pid(owner) and node(owner) == node(), do: :ok
  defp validate_owner(_owner), do: {:stop, :invalid_owner}

  defp validate_max_message_size(size)
       when is_integer(size) and size >= @min_max_message_size and
              size <= @max_max_message_size,
       do: :ok

  defp validate_max_message_size(_size), do: {:stop, :invalid_max_message_size}

  defp validate_max_pending_calls(limit) when is_integer(limit) and limit > 0, do: :ok
  defp validate_max_pending_calls(_limit), do: {:stop, :invalid_max_pending_calls}

  defp validate_registered_name(name) when is_nil(name) or is_atom(name), do: :ok
  defp validate_registered_name(_name), do: {:stop, :invalid_name}

  defp open_socket(settings, family, addr) do
    # Exits stay untrapped until setup has authenticated; see the `{:setup,
    # addr}` continuation.
    impl = settings.impl

    case impl.transport.open(family, :stream, :default) do
      {:ok, sock} ->
        _ = configure_receive_buffer(impl.transport, sock)

        # Every setting whose name is also a struct field is copied wholesale;
        # the rest of the settings map is either validated only (`timeout`),
        # means something else on the struct (`name` is the registered process
        # name, not the unique bus name), or belongs to the inbound buffer
        # (`max_message_size`).
        state =
          struct!(
            %__MODULE__{
              sock: sock,
              inbound: Inbound.new(<<>>, settings.max_message_size),
              connect_waiter_monitor: Setup.monitor_connect_waiter(settings.connect_waiter),
              owner_monitor: monitor_owner(settings.owner),
              unix_fd_transport?: Setup.unix_fd_transport_supported?(family)
            },
            Map.take(settings, @setting_fields)
          )

        {:ok, state, {:continue, {:setup, addr}}}

      {:error, reason} ->
        {:stop, normalize_socket_error(reason)}
    end
  end

  # A monitor rather than a link: a connection that dies must not take its
  # owner with it, and in-flight callers already learn of that death through
  # `{:error, :disconnected}`. An owner that is already dead answers with an
  # immediate `:DOWN`, which setup reads before it touches the socket.
  defp monitor_owner(nil), do: nil
  defp monitor_owner(owner) when is_pid(owner), do: Process.monitor(owner)

  @impl true
  def terminate(
        _reason,
        %__MODULE__{
          sock: sock,
          impl: impl,
          partial_frame_timer: timer_ref,
          inbound_fds: inbound_fds,
          fd_claims: fd_claims,
          inbound_calls: inbound_calls
        }
      ) do
    Dispatch.cancel_partial_frame_timer(timer_ref)
    _ = UnixFD.close_all(Rights.fds(inbound_fds))
    FDClaims.close_all(fd_claims)
    InboundCalls.close_all(inbound_calls)
    _ = impl.transport.close(sock)
    :ok
  end

  @impl true
  def handle_info(
        {connect_ref, :accepted},
        %__MODULE__{connect_waiter: {_pid, connect_ref}, connect_accepted?: false} = state
      ) do
    # The continuation runs before queued application calls, making Hello the
    # first D-Bus frame after setup acceptance. The final acknowledgement is
    # withheld until its correlated reply has established the connection. A
    # non-bus connection has no Hello to correlate, so it establishes directly.
    {:noreply, %{state | connect_accepted?: true}, {:continue, Setup.continuation(state)}}
  end

  def handle_info(
        {:DOWN, ref, :process, _pid, _reason},
        %__MODULE__{connect_waiter_monitor: ref} = state
      ) do
    {:stop, {:shutdown, :caller_gone}, state}
  end

  # An owned connection follows its owner out. The stop is ordinary, so
  # terminate/2 runs and closes the socket and every retained descriptor.
  # Whether the peer then reads a FIN or a reset is the kernel's decision, and
  # depends on bytes this side may not have read yet. This clause precedes the
  # general monitor handling below: the owner's reference indexes no request
  # of its own.
  def handle_info(
        {:DOWN, ref, :process, _pid, _reason},
        %__MODULE__{owner_monitor: ref} = state
      ) do
    {:stop, {:shutdown, :owner_down}, state}
  end

  # Replies this connection originates are queued from the inbound path, which
  # returns its own receive continuation. This starts the writer for them.
  def handle_info(:advance_writes, %__MODULE__{} = state), do: advance_writes(state)

  def handle_info({:"$socket", s, :select, h}, %__MODULE__{sock: s, rref: h} = state) do
    {:noreply, %{state | rref: nil}, {:continue, :recv}}
  end

  # Send select handles are deliberately kept separate from the receive handle.
  # A writable socket must not prevent us from continuing to drain inbound replies.
  def handle_info(
        {:"$socket", s, :select, h},
        %__MODULE__{sock: s, writer: %Writer{active: %Active{wait: {:select, continuation, h}}}} =
          state
      ) do
    state.writer
    |> Writer.resume_select(continuation, writer_context(state))
    |> writer_result(state)
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, rref: h} = state
      ) do
    stop_for_transport_error(reason, state)
  end

  def handle_info({:DOWN, ref, :process, _pid, _reason}, %__MODULE__{} = state) do
    case Map.pop(state.handler_monitor_index, ref) do
      {handler_ref, handler_monitor_index} when is_reference(handler_ref) ->
        {:noreply,
         %{
           state
           | handlers: Map.delete(state.handlers, handler_ref),
             handler_monitor_index: handler_monitor_index
         }}

      {nil, _handler_monitor_index} ->
        handle_down_for_export(ref, state)
    end
  end

  def handle_info({:inbound_call_timeout, ref}, %__MODULE__{} = state),
    do: {:noreply, Service.expire(state, ref)}

  def handle_info({:request_timeout, serial, request_ref}, %__MODULE__{} = state),
    do: serial |> Dispatch.request_timeout(request_ref, state) |> dispatch_result()

  def handle_info({:fd_claim_timeout, claim_ref}, %__MODULE__{} = state) do
    {:noreply, %{state | fd_claims: FDClaims.expire(state.fd_claims, claim_ref)}}
  end

  def handle_info({:fd_claim_outcome_timeout, claim_ref}, %__MODULE__{} = state) do
    {:noreply, %{state | fd_claims: FDClaims.expire_outcome(state.fd_claims, claim_ref)}}
  end

  def handle_info(
        {:partial_frame_timeout, token},
        %__MODULE__{
          partial_frame_timer: {_timer_ref, token}
        } = state
      ) do
    stop_for_protocol_error(:read_timeout, %{state | partial_frame_timer: nil})
  end

  def handle_info(
        {:write_timeout, request_ref},
        %__MODULE__{writer: %Writer{active: %Active{request_ref: request_ref}}} = state
      ) do
    state.writer |> Writer.write_timeout(writer_context(state)) |> writer_result(state)
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, writer: %Writer{active: %Active{wait: {:select, _continuation, h}}}} =
          state
      ),
      do: stop_for_transport_error(reason, state)

  # The match-subscription worker never exits while its connection is up
  # unless something is wrong with it, a bug in Rebus. The match rules and
  # handlers it installed here cannot be managed without it, so the connection
  # stops too, which makes the bus discard the rules. The worker reports its
  # own crash; this names the reason the connection stopped. A worker that
  # exits while setup is still running, with exits untrapped, stops the
  # connection with the worker's own reason instead.
  def handle_info({:EXIT, pid, reason}, %__MODULE__{match_worker: pid} = state)
      when is_pid(pid) do
    Logger.error("D-Bus match subscription worker exited, closing connection")
    {:stop, {:shutdown, {:match_subscription_exited, reason}}, state}
  end

  # With :trap_exit enabled, preserve the process-link behavior that would
  # otherwise terminate this GenServer. In particular, supervisor shutdown
  # reaches terminate/2 instead of bypassing descriptor cleanup. :kill remains
  # untrappable by the BEAM and is documented as outside that guarantee.
  def handle_info({:EXIT, _pid, :normal}, %__MODULE__{} = state), do: {:noreply, state}

  def handle_info({:EXIT, _pid, reason}, %__MODULE__{} = state),
    do: {:stop, reason, state}

  def handle_info(_message, %__MODULE__{} = state), do: {:noreply, state}

  # The handler of an export went down, which unexports it.
  defp handle_down_for_export(ref, %__MODULE__{} = state) do
    case Service.handler_down(state, ref) do
      {:ok, state} -> {:noreply, state}
      :error -> handle_down_for_request(ref, state)
    end
  end

  # A monitored caller can be waiting in exactly one place: the pending-reply
  # table, the write queue, or an unacknowledged FD claim. They are searched in
  # that order, and a reference found nowhere is a monitor this connection no
  # longer cares about.
  defp handle_down_for_request(ref, %__MODULE__{} = state) do
    case Pending.pop_by_monitor(state.pending, ref) do
      {nil, _pending} -> down_for_queued_write(ref, state)
      {_entry, pending} -> {:noreply, %{state | pending: pending}}
    end
  end

  defp down_for_queued_write(ref, %__MODULE__{} = state) do
    case Writer.pop_monitor(state.writer, ref) do
      :error ->
        down_for_fd_claim(ref, state)

      {request_ref, writer} ->
        state = %{state | writer: writer}

        writer
        |> Writer.cancel_monitored(request_ref, writer_context(state))
        |> writer_result(state)
    end
  end

  defp down_for_fd_claim(ref, %__MODULE__{} = state) do
    case FDClaims.fetch_by_monitor(state.fd_claims, ref) do
      {:ok, claim_ref} ->
        {:noreply, drop_fd_claim(state, claim_ref, close?: true, monitor_down?: true)}

      :error ->
        {:noreply, state}
    end
  end

  @impl true

  # DynamicSupervisor stops children with an exit signal, which a trapping
  # process reads only once the callback it is in returns. Setup blocks on
  # the identity lookup, the socket connect and the handshake for up to its
  # whole budget, so it runs with exits untrapped: `Rebus.close/1` stops a
  # connection stalled there at once instead of waiting out setup or the
  # child's shutdown timeout. Nothing it holds needs terminate/2. The socket
  # is closed along with the process that owns it, and the handshake reads
  # with plain recv, which never hands this process a descriptor.
  #
  # The Hello reply and every read after it can carry SCM_RIGHTS
  # descriptors, which only terminate/2 closes, so exits are trapped from
  # here on. The GenServer loop then returns :stop on a supervisor shutdown
  # and runs terminate/2, which closes raw descriptors retained in partial
  # frames or reply claims. The EXIT clauses above preserve normal link
  # semantics.
  def handle_continue({:setup, addr}, %__MODULE__{} = state) do
    result = Setup.setup(state, addr)
    Process.flag(:trap_exit, true)
    dispatch_result(result)
  end

  def handle_continue(:hello, %__MODULE__{} = state),
    do: state |> Setup.hello() |> dispatch_result()

  def handle_continue(:established, %__MODULE__{} = state),
    do: state |> Setup.established() |> dispatch_result()

  def handle_continue(:hello_reply_buffer, %__MODULE__{} = state),
    do: state |> Setup.hello_reply_buffer() |> dispatch_result()

  def handle_continue(:hello_reply, %__MODULE__{} = state),
    do: state |> Setup.hello_reply() |> dispatch_result()

  def handle_continue({:hello_reply, deadline}, %__MODULE__{} = state),
    do: state |> Setup.hello_reply(deadline) |> dispatch_result()

  def handle_continue(:recv, %__MODULE__{rref: nil} = state),
    do: state |> Dispatch.recv() |> dispatch_result()

  # A pending socket operation owns the receive continuation. Keeping this
  # catch-all prevents a stale continuation from crashing and exposing state.
  def handle_continue(:recv, %__MODULE__{} = state), do: {:noreply, state}

  def handle_continue(:write, %__MODULE__{} = state), do: advance_writes(state)

  @doc false
  @spec handle_receive_result(term(), t()) ::
          {:noreply, t()} | {:noreply, t(), {:continue, term()}} | {:stop, term(), t()}
  def handle_receive_result(result, %__MODULE__{} = state),
    do: result |> Dispatch.receive_result(state) |> dispatch_result()

  # The single place a `Rebus.Connection.Dispatch` or `Rebus.Connection.Setup`
  # decision becomes a `GenServer` callback return. Both terminal error kinds
  # are logged, sanitised and fail everything outstanding here rather than in
  # the module that detected them.
  defp dispatch_result({:ok, %__MODULE__{} = state}), do: {:noreply, state}

  defp dispatch_result({:continue, continuation, %__MODULE__{} = state}),
    do: {:noreply, state, {:continue, continuation}}

  defp dispatch_result({:protocol_error, reason, %__MODULE__{} = state}),
    do: stop_for_protocol_error(reason, state)

  defp dispatch_result({:transport_error, reason, %__MODULE__{} = state}),
    do: stop_for_transport_error(reason, state)

  defp dispatch_result({:shutdown, reason, %__MODULE__{} = state}),
    do: {:stop, {:shutdown, reason}, state}

  @impl true
  def handle_call(
        {:call, %Message{}, _deadline, _request_ref},
        _from,
        %__MODULE__{established?: false} = state
      ) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:call, %Message{} = msg, deadline, request_ref}, from, %__MODULE__{} = state) do
    case validate_call_message(msg) do
      :ok ->
        enqueue_write(state, %{
          kind: :call,
          from: from,
          msg: msg,
          deadline: deadline,
          request_ref: request_ref
        })

      {:error, _} = error ->
        {:reply, error, state}
    end
  end

  def handle_call(
        {:send, %Message{}, _deadline, _request_ref},
        _from,
        %__MODULE__{established?: false} = state
      ) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:send, %Message{} = msg, deadline, request_ref}, from, %__MODULE__{} = state),
    do: enqueue_send(validate_send_message(msg), state, from, msg, deadline, request_ref)

  # Nothing is routed to a handler before establishment, so a reply then
  # answers nothing, as does one for a call already answered or expired. The
  # frame is checked before the call is completed, so a handler whose reply
  # is refused here can still answer the call another way.
  def handle_call(
        {:reply_call, ref, %Message{} = msg, deadline, request_ref},
        from,
        %__MODULE__{} = state
      ) do
    with :ok <- validate_reply_message(msg),
         :ok <- validate_outbound_fd_transport(msg, state),
         {:ok, msg, state} <- Service.complete(state, ref, msg) do
      enqueue_send(:ok, state, from, msg, deadline, request_ref)
    else
      {:answered, state} -> {:reply, :ok, state}
      {:error, _} = error -> {:reply, error, state}
    end
  end

  def handle_call({:complete_call, ref}, _from, %__MODULE__{} = state) do
    case Service.release(state, ref) do
      {:ok, state} -> {:reply, :ok, state}
      {:error, _} = error -> {:reply, error, state}
    end
  end

  # The claim answers the taker itself, as a reply's claim answers `call/3`.
  def handle_call({:take_call_fds, ref, deadline, request_ref}, from, %__MODULE__{} = state) do
    case Service.take_fds(state, ref, from, deadline, request_ref) do
      {:ok, state} -> {:noreply, state}
      {:error, _} = error -> {:reply, error, state}
    end
  end

  def handle_call(
        {:invoke, %Message{}, _request_ref},
        _from,
        %__MODULE__{established?: false} = state
      ) do
    {:reply, {:error, :not_connected}, state}
  end

  # A local call is numbered from this connection's own serials, which no
  # frame on the wire will reuse, and sent from its own name.
  def handle_call({:invoke, %Message{} = msg, request_ref}, from, %__MODULE__{} = state) do
    fields =
      case state.name do
        nil -> Map.delete(msg.header_fields, :sender)
        name -> Map.put(msg.header_fields, :sender, name)
      end

    msg = %{msg | serial: Writer.serial(state.writer), header_fields: fields}
    state = %{state | writer: Writer.consume_serial(state.writer)}
    {:noreply, Service.invoke(msg, from, request_ref, state)}
  end

  def handle_call({:export, _spec}, _from, %__MODULE__{established?: false} = state) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:export, spec}, _from, %__MODULE__{} = state) do
    {reply, state} = Service.export(state, spec)
    {:reply, reply, state}
  end

  def handle_call({:unexport, _ref}, _from, %__MODULE__{established?: false} = state) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:unexport, ref}, _from, %__MODULE__{} = state) do
    {:reply, :ok, Service.unexport(state, ref)}
  end

  def handle_call(
        {:claim_fd_reply, claim_ref, delivery_ref, delivery_alias},
        {pid, _tag},
        %__MODULE__{} = state
      ) do
    {reply, claims} =
      FDClaims.claim(
        state.fd_claims,
        claim_ref,
        delivery_ref,
        delivery_alias,
        pid,
        fd_claims_context(state)
      )

    {:reply, reply, %{state | fd_claims: claims}}
  end

  def handle_call({:ack_fd_reply, claim_ref, delivery_ref}, {pid, _tag}, %__MODULE__{} = state) do
    {reply, claims} =
      FDClaims.ack(state.fd_claims, claim_ref, delivery_ref, pid, fd_claims_context(state))

    {:reply, reply, %{state | fd_claims: claims}}
  end

  def handle_call({:resolve_fd_claim, claim_ref, delivery_ref}, _from, %__MODULE__{} = state) do
    {reply, claims} = FDClaims.resolve(state.fd_claims, claim_ref, delivery_ref)
    {:reply, reply, %{state | fd_claims: claims}}
  end

  def handle_call({:discard_fd_claim, claim_ref}, {pid, _tag}, %__MODULE__{} = state) do
    {:reply, :ok, %{state | fd_claims: FDClaims.discard(state.fd_claims, claim_ref, pid)}}
  end

  def handle_call(:bus?, _from, %__MODULE__{} = state) do
    {:reply, state.bus?, state}
  end

  # Before establishment a bus connection has no name yet, which `nil` would
  # misreport as a peer-to-peer connection.
  def handle_call(:unique_name, _from, %__MODULE__{established?: false} = state) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call(:unique_name, _from, %__MODULE__{} = state) do
    {:reply, {:ok, state.name}, state}
  end

  def handle_call(
        {:add_signal_handler, _pid, _handler_ref},
        _from,
        %__MODULE__{established?: false} = state
      ) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call(
        {:add_signal_handler, _pid, _handler_ref, %MatchRule{}},
        _from,
        %__MODULE__{established?: false} = state
      ) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:add_signal_handler, pid, handler_ref}, _from, %__MODULE__{} = state) do
    add_signal_handler(state, pid, handler_ref, nil)
  end

  def handle_call(
        {:add_signal_handler, pid, handler_ref, %MatchRule{} = rule},
        _from,
        %__MODULE__{} = state
      ) do
    add_signal_handler(state, pid, handler_ref, rule)
  end

  # The reset is unconditional, so an entry a previous tracking cycle left
  # behind - an untrack whose final step failed after the bus rule was already
  # removed - cannot survive a later re-track and block every seed after it,
  # since the seed below writes only over `:unknown`. Forcing the value is safe
  # because tracking marks the name before it installs the tracking rule with
  # AddMatch, so no NameOwnerChanged for a freshly tracked name can have been
  # applied yet, and GetNameOwner reseeds immediately after; a signal that does
  # arrive between the reset and the seed still wins, exactly as it did before.
  def handle_call({:track_name_owner, name}, _from, %__MODULE__{} = state) do
    {:reply, :ok, %{state | name_owners: Map.put(state.name_owners, name, :unknown)}}
  end

  def handle_call({:seed_name_owner, name, owner}, _from, %__MODULE__{} = state) do
    case Map.fetch(state.name_owners, name) do
      {:ok, :unknown} ->
        {:reply, :ok, %{state | name_owners: Map.put(state.name_owners, name, owner)}}

      _seeded_or_untracked ->
        {:reply, :ok, state}
    end
  end

  def handle_call({:untrack_name_owner, name}, _from, %__MODULE__{} = state) do
    {:reply, :ok, %{state | name_owners: Map.delete(state.name_owners, name)}}
  end

  def handle_call({:delete_signal_handler, _ref}, _from, %__MODULE__{established?: false} = state) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:delete_signal_handler, ref}, _from, %__MODULE__{} = state) do
    {:reply, :ok, remove_signal_handler(state, ref)}
  end

  def handle_call(
        {:watch_name, _owner, _name, _ref},
        _from,
        %__MODULE__{established?: false} = state
      ) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:watch_name, _owner, _name, _ref}, _from, %__MODULE__{name: nil} = state) do
    {:reply, {:error, :not_a_bus}, state}
  end

  def handle_call({:watch_name, owner, name, handler_ref}, _from, %__MODULE__{} = state) do
    if Enum.any?(state.handlers, &name_watch?(&1, name, owner)) do
      {:reply, {:ok, :existing}, state}
    else
      rule = name_rule(name, state.name)
      state = put_signal_handler(state, owner, handler_ref, %{rule: rule, name: name})
      {:reply, {:ok, :added}, state}
    end
  end

  defp add_signal_handler(%__MODULE__{} = state, pid, handler_ref, rule) do
    {:reply, {:ok, handler_ref}, put_signal_handler(state, pid, handler_ref, %{rule: rule})}
  end

  defp put_signal_handler(%__MODULE__{} = state, pid, handler_ref, fields) do
    monitor_ref = Process.monitor(pid)

    %{
      state
      | handlers:
          Map.put(
            state.handlers,
            handler_ref,
            Map.merge(%{pid: pid, monitor_ref: monitor_ref}, fields)
          ),
        handler_monitor_index: Map.put(state.handler_monitor_index, monitor_ref, handler_ref)
    }
  end

  defp name_watch?({_handler_ref, %{name: name, pid: pid}}, name, owner),
    do: owner == :all or pid == owner

  defp name_watch?(_handler, _name, _owner), do: false

  # Only the bus driver can send with its own name as the sender, and the
  # sender of a directed signal is compared exactly, so naming this
  # connection as the destination keeps a peer's broadcast lookalike out.
  # Dispatch narrows the member to NameAcquired and NameLost, which one rule
  # cannot express.
  defp name_rule(name, unique_name) do
    MatchRule.new!(
      sender: "org.freedesktop.DBus",
      path: "/org/freedesktop/DBus",
      interface: "org.freedesktop.DBus",
      destination: unique_name,
      args: %{0 => name}
    )
  end

  @impl true
  def handle_cast({:cancel, request_ref}, %__MODULE__{} = state) do
    case Pending.pop_by_request(state.pending, request_ref) do
      {nil, _pending} -> cancel_invoke_claim_or_write(request_ref, state)
      {_entry, pending} -> {:noreply, %{state | pending: pending}}
    end
  end

  def handle_cast({:cancel_signal_handler, handler_ref}, %__MODULE__{} = state) do
    {:noreply, remove_signal_handler(state, handler_ref)}
  end

  def handle_cast({:unwatch_name, name, owner}, %__MODULE__{} = state) do
    state =
      state.handlers
      |> Enum.filter(&name_watch?(&1, name, owner))
      |> Enum.reduce(state, fn {handler_ref, _handler}, state ->
        remove_signal_handler(state, handler_ref)
      end)

    {:noreply, state}
  end

  # An export whose caller stopped waiting before it was answered.
  def handle_cast({:unexport, ref}, %__MODULE__{} = state) do
    {:noreply, Service.unexport(state, ref)}
  end

  def handle_cast({:notify_owner, message}, %__MODULE__{owner: owner} = state) do
    if is_pid(owner), do: send(owner, message)
    {:noreply, state}
  end

  def handle_cast({:attach_match_worker, worker}, %__MODULE__{} = state),
    do: {:noreply, %{state | match_worker: worker}}

  # A request that is not waiting for a reply may be a local call still
  # waiting for its handler.
  defp cancel_invoke_claim_or_write(request_ref, %__MODULE__{} = state) do
    case Service.cancel(state, request_ref) do
      {:ok, state} -> {:noreply, state}
      :error -> cancel_claim_or_write(request_ref, state)
    end
  end

  # A request that is not waiting for a reply is either an unacknowledged FD
  # claim or a frame still queued or being written.
  defp cancel_claim_or_write(request_ref, %__MODULE__{} = state) do
    case FDClaims.fetch_by_request(state.fd_claims, request_ref) do
      {:ok, claim_ref} ->
        {:noreply, drop_fd_claim(state, claim_ref, close?: true)}

      :error ->
        state.writer
        |> Writer.cancel(request_ref, writer_context(state))
        |> writer_result(state)
    end
  end

  defp safe_setup_call(conn, message, cancellation \\ nil, timeout \\ @default_read_timeout),
    do: SafeCall.call(conn, message, timeout, cancel: cancellation)

  defp remove_signal_handler(%__MODULE__{} = state, handler_ref) do
    case Map.pop(state.handlers, handler_ref) do
      {%{monitor_ref: monitor_ref}, handlers} ->
        Process.demonitor(monitor_ref, [:flush])

        %{
          state
          | handlers: handlers,
            handler_monitor_index: Map.delete(state.handler_monitor_index, monitor_ref)
        }

      {nil, _handlers} ->
        state
    end
  end

  @doc false
  @spec configure_receive_buffer(module(), :socket.socket()) :: :tuple | :scalar | :default
  defdelegate configure_receive_buffer(transport, sock), to: Setup

  @doc false
  @spec normalize_socket_error(term()) :: term()
  def normalize_socket_error(reason), do: SocketError.normalize(reason)

  defp stop_for_transport_error(reason, %__MODULE__{} = state) do
    reason = normalize_socket_error(reason)

    Logger.log(
      transport_stop_level(reason, state),
      "D-Bus connection transport stopped: #{inspect(reason)}",
      reason: reason
    )

    {:stop, {:shutdown, reason}, state |> Dispatch.discard_inbound_unix_fds() |> fail_pending()}
  end

  # A peer closing or resetting an established connection is how one normally
  # ends, for example when the bus restarts, so it is not worth a warning. The
  # same reasons before establishment mean setup failed, and stay warnings.
  defp transport_stop_level(reason, %__MODULE__{established?: true})
       when reason in [:closed, :econnreset],
       do: :info

  defp transport_stop_level(_reason, %__MODULE__{}), do: :warning

  defp stop_for_protocol_error(reason, %__MODULE__{} = state) do
    reason = sanitize_protocol_reason(reason)
    Logger.warning("D-Bus connection protocol stopped: #{inspect(reason)}", reason: reason)
    {:stop, {:shutdown, reason}, state |> Dispatch.discard_inbound_unix_fds() |> fail_pending()}
  end

  # Each zero-length receive returns data already available through the fixed
  # OTP buffer. Fixed-header validation still happens as soon as 16 bytes are
  # retained, without making allocation depend on a peer-declared frame length.
  @doc false
  @spec append_inbound_fragment(binary(), t(), term()) ::
          {:noreply, t()} | {:noreply, t(), {:continue, term()}} | {:stop, term(), t()}
  def append_inbound_fragment(data, %__MODULE__{} = state, continuation)
      when is_binary(data) do
    data |> Dispatch.append_inbound(state, continuation) |> dispatch_result()
  end

  @doc false
  @spec sanitize_protocol_reason(term()) ::
          protocol_reason()
          | :protocol_error
          | {:hello_failed, binary() | hello_failed_reason()}
          | {:malformed_reply, :missing_reply_serial}
          | {:unexpected_handshake_message, Message.message_type()}
  def sanitize_protocol_reason(reason) do
    case reason do
      {:hello_failed, reason} when reason in @hello_failed_reasons ->
        {:hello_failed, reason}

      {:hello_failed, error_name} when is_binary(error_name) ->
        if WireValue.valid_error_name?(error_name),
          do: {:hello_failed, :binary.copy(error_name)},
          else: {:hello_failed, :invalid_error_name}

      {:hello_failed, _reason} ->
        {:hello_failed, :invalid_error_name}

      {:unexpected_handshake_message, type} when is_atom(type) ->
        {:unexpected_handshake_message, type}

      {:malformed_reply, :missing_reply_serial} ->
        {:malformed_reply, :missing_reply_serial}

      reason when reason in @protocol_reasons ->
        reason

      _reason ->
        :protocol_error
    end
  end

  defp validate_call_message(%Message{type: :method_call, flags: flags}) do
    if :no_reply_expected in flags, do: {:error, :no_reply_expected}, else: :ok
  end

  defp validate_call_message(%Message{type: type}), do: {:error, {:invalid_message_type, type}}

  defp validate_send_message(%Message{type: :signal}), do: :ok

  defp validate_send_message(%Message{type: :method_call, flags: flags}) do
    if :no_reply_expected in flags, do: :ok, else: {:error, :reply_expected}
  end

  defp validate_send_message(%Message{type: type}), do: {:error, {:invalid_message_type, type}}

  # The only types a handler answers an inbound call with.
  defp validate_reply_message(%Message{type: type}) when type in [:method_return, :error],
    do: :ok

  defp validate_reply_message(%Message{type: type}), do: {:error, {:invalid_message_type, type}}

  # Both `Rebus.send/2` frames and handler replies are written on the `:send`
  # path; they differ only in which message types they accept.
  defp enqueue_send(:ok, %__MODULE__{} = state, from, msg, deadline, request_ref) do
    enqueue_write(state, %{
      kind: :send,
      from: from,
      msg: msg,
      deadline: deadline,
      request_ref: request_ref
    })
  end

  defp enqueue_send({:error, _} = error, %__MODULE__{} = state, _from, _msg, _deadline, _ref),
    do: {:reply, error, state}

  defp validate_outbound_fd_transport(%Message{unix_fds: []}, _state), do: :ok

  defp validate_outbound_fd_transport(%Message{}, %__MODULE__{unix_fd_transport?: false}),
    do: {:error, :unix_fd_unsupported}

  defp validate_outbound_fd_transport(%Message{}, %__MODULE__{unix_fd_negotiated?: false}),
    do: {:error, :unix_fd_not_negotiated}

  defp validate_outbound_fd_transport(%Message{}, %__MODULE__{}), do: :ok

  # Everything a claim operation borrows from the connection for one call.
  # `Rebus.Connection.Dispatch` opens claims of its own and asks for it here.
  @doc false
  @spec fd_claims_context(t()) :: FDClaims.context()
  def fd_claims_context(%__MODULE__{impl: %{hooks: hooks}}), do: %{hooks: hooks}

  defp drop_fd_claim(%__MODULE__{} = state, claim_ref, opts),
    do: %{state | fd_claims: FDClaims.drop(state.fd_claims, claim_ref, opts)}

  # Everything the writer borrows from the connection for one call. Building it
  # per call keeps the writer free of connection state, and means substituting
  # an implementation module on a running connection needs no writer update.
  defp writer_context(%__MODULE__{} = state) do
    %{
      sock: state.sock,
      transport: state.impl.transport,
      hooks: state.impl.hooks,
      write_timeout: state.write_timeout,
      pending: Pending.entries(state.pending),
      validate: &validate_outbound_fd_transport(&1, state)
    }
  end

  defp enqueue_write(%__MODULE__{} = state, operation) do
    state.writer |> Writer.enqueue(operation, writer_context(state)) |> writer_result(state)
  end

  defp advance_writes(%__MODULE__{} = state) do
    state.writer |> Writer.advance(writer_context(state)) |> writer_result(state)
  end

  defp writer_result({:ok, writer}, %__MODULE__{} = state),
    do: {:noreply, %{state | writer: writer}}

  defp writer_result({:continue, writer}, %__MODULE__{} = state),
    do: {:noreply, %{state | writer: writer}, {:continue, :write}}

  # A `:call` frame has reached the peer. The writer has started its request
  # timer and handed back the correlation entry, which only the connection can
  # index; the next frame waits until that entry is registered, because serial
  # allocation reads it.
  defp writer_result({:call_written, entry, writer}, %__MODULE__{} = state) do
    %{state | writer: writer} |> register_pending(entry) |> advance_writes()
  end

  defp writer_result({:stop, reason, writer}, %__MODULE__{} = state),
    do: stop_for_transport_error(reason, %{state | writer: writer})

  defp register_pending(%__MODULE__{} = state, entry),
    do: %{state | pending: Pending.put(state.pending, entry)}

  defp fail_pending(%__MODULE__{} = state) do
    writer = Writer.abandon_all(state.writer)

    %{
      state
      | pending: Pending.fail_all(state.pending),
        fd_claims: FDClaims.fail_all(state.fd_claims),
        inbound_calls: InboundCalls.fail_all(state.inbound_calls),
        writer: writer
    }
  end
end
