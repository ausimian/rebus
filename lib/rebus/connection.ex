defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.Connection.Dispatch
  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Handshake
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Pending
  alias Rebus.Connection.Rights
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

  # The settings a connection carries verbatim into its state. Most are read
  # only while the connection is being set up.
  @setting_fields [
    :impl,
    :write_timeout,
    :read_timeout,
    :setup_timeout,
    :aggregate_setup_timeout?,
    :expected_guid,
    :precomputed_auth_id,
    :allow_anonymous?,
    :bus?,
    :connect_waiter,
    :owner
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
  # Marks a name tracked, without disturbing an owner a signal already set.
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
    field :aggregate_setup_timeout?, boolean(), default: false
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
    # Signal handlers registered against this connection. Delivery and match
    # filtering happen in the connection process itself, so a signal arriving
    # here can never reach a handler installed on another connection.
    field :handlers,
          %{reference() => %{pid: pid(), monitor_ref: reference(), rule: MatchRule.t() | nil}},
          default: %{}

    field :handler_monitor_index, %{reference() => reference()}, default: %{}
    # The current owner of every well-known name a subscription depends on, as
    # the bus reports it. `:unknown` is tracked but not yet seeded, `nil` is a
    # name nobody owns, and a binary is the owner's unique name. Matching a
    # directed signal needs this answer synchronously, so it lives here rather
    # than in the process that performs the bus round trips.
    field :name_owners, %{binary() => :unknown | nil | binary()}, default: %{}
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
      aggregate_setup_timeout?: Map.has_key?(internal, :setup_timeout),
      expected_guid: Map.get(internal, :expected_guid),
      precomputed_auth_id: Map.get(internal, :precomputed_auth_id),
      allow_anonymous?: Keyword.get(opts, :allow_anonymous, false),
      bus?: Keyword.get(opts, :bus, true),
      name: Keyword.get(opts, :name),
      connect_waiter: Map.get(internal, :connect_waiter),
      owner: Keyword.get(opts, :owner),
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
         :ok <- validate_owner(settings.owner) do
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

  defp validate_registered_name(name) when is_nil(name) or is_atom(name), do: :ok
  defp validate_registered_name(_name), do: {:stop, :invalid_name}

  defp open_socket(settings, family, addr) do
    # DynamicSupervisor stops children with an exit signal. Trap it so the
    # GenServer loop can return :stop and therefore invoke terminate/2,
    # which closes raw SCM_RIGHTS descriptors retained in partial frames or
    # reply claims. The EXIT clauses below preserve normal link semantics.
    Process.flag(:trap_exit, true)
    impl = settings.impl

    case impl.transport.open(family, :stream, :default) do
      {:ok, sock} ->
        _ = configure_receive_buffer(impl.transport, sock)

        # Every setting whose name is also a struct field is copied wholesale;
        # the rest of the settings map is either validated only (`timeout`) or
        # means something else on the struct (`name` is the registered process
        # name, not the unique bus name).
        state =
          struct!(
            %__MODULE__{
              sock: sock,
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
          fd_claims: fd_claims
        }
      ) do
    Dispatch.cancel_partial_frame_timer(timer_ref)
    _ = UnixFD.close_all(Rights.fds(inbound_fds))
    FDClaims.close_all(fd_claims)
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
        handle_down_for_request(ref, state)
    end
  end

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

  # With :trap_exit enabled, preserve the process-link behavior that would
  # otherwise terminate this GenServer. In particular, supervisor shutdown
  # reaches terminate/2 instead of bypassing descriptor cleanup. :kill remains
  # untrappable by the BEAM and is documented as outside that guarantee.
  def handle_info({:EXIT, _pid, :normal}, %__MODULE__{} = state), do: {:noreply, state}

  def handle_info({:EXIT, _pid, reason}, %__MODULE__{} = state),
    do: {:stop, reason, state}

  def handle_info(_message, %__MODULE__{} = state), do: {:noreply, state}

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

  def handle_continue({:setup, addr}, %__MODULE__{} = state),
    do: state |> Setup.setup(addr) |> dispatch_result()

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

  def handle_call({:send, %Message{} = msg, deadline, request_ref}, from, %__MODULE__{} = state) do
    case validate_send_message(msg) do
      :ok ->
        enqueue_write(state, %{
          kind: :send,
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

  def handle_call({:track_name_owner, name}, _from, %__MODULE__{} = state) do
    {:reply, :ok, %{state | name_owners: Map.put_new(state.name_owners, name, :unknown)}}
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

  defp add_signal_handler(%__MODULE__{} = state, pid, handler_ref, rule) do
    monitor_ref = Process.monitor(pid)

    {:reply, {:ok, handler_ref},
     %{
       state
       | handlers:
           Map.put(state.handlers, handler_ref, %{
             pid: pid,
             monitor_ref: monitor_ref,
             rule: rule
           }),
         handler_monitor_index: Map.put(state.handler_monitor_index, monitor_ref, handler_ref)
     }}
  end

  @impl true
  def handle_cast({:cancel, request_ref}, %__MODULE__{} = state) do
    case Pending.pop_by_request(state.pending, request_ref) do
      {nil, _pending} -> cancel_claim_or_write(request_ref, state)
      {_entry, pending} -> {:noreply, %{state | pending: pending}}
    end
  end

  def handle_cast({:cancel_signal_handler, handler_ref}, %__MODULE__{} = state) do
    {:noreply, remove_signal_handler(state, handler_ref)}
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
    Logger.warning("D-Bus connection transport stopped: #{inspect(reason)}", reason: reason)
    {:stop, {:shutdown, reason}, state |> Dispatch.discard_inbound_unix_fds() |> fail_pending()}
  end

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
        writer: writer
    }
  end
end
