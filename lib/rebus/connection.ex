defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.Connection.Handshake
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.SocketError
  alias Rebus.Connection.Writer
  alias Rebus.MachineId
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.SignalHandler
  alias Rebus.UnixFD
  alias Rebus.WireValue
  require Logger

  @default_write_timeout 5_000
  @default_read_timeout 5_000
  @max_read_chunk 65_536
  @max_read_attempts 1
  @max_unix_fd_control_size 256
  # A reply carrying descriptors is first acknowledged through a small
  # connection-owned claim.  This deliberately avoids treating delivery to a
  # GenServer.call alias as ownership transfer: aliases can be deactivated
  # while their process remains alive after a caller-side timeout.
  # FD delivery starts in a short extension of the request's original absolute
  # deadline. It exists solely to close or hand off a descriptor safely after
  # a reply reaches the boundary of that deadline; it is not a second public
  # request timeout. A definitive resolver may wait longer if a live connection
  # has an acknowledgement queued ahead of it; see resolve_fd_claim/3.
  @fd_claim_handoff_grace 100
  @fd_claim_cleanup_grace 250

  # Every D-Bus connection is expected to implement org.freedesktop.DBus.Peer;
  # dbus-daemon, busctl and d-feet all call it. Every other inbound method call
  # is refused so a caller fails immediately instead of waiting for its own
  # timeout.
  @peer_interface "org.freedesktop.DBus.Peer"
  @unknown_method_error "org.freedesktop.DBus.Error.UnknownMethod"
  @failed_error "org.freedesktop.DBus.Error.Failed"
  @unknown_method_message "Method not handled by this connection"
  @machine_id_unavailable_message "Machine ID unavailable"

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
    pid
    |> GenServer.call({:call, msg, deadline, request_ref}, timeout)
    |> receive_fd_reply_claim(pid, deadline, request_ref)
  catch
    :exit, {:timeout, _call} ->
      GenServer.cast(pid, {:cancel, request_ref})
      {:error, :timeout}

    :exit, _reason ->
      {:error, :disconnected}
  end

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
    GenServer.call(pid, {:send, msg, deadline, request_ref}, dispatch_timeout)
  catch
    :exit, {:timeout, _call} ->
      GenServer.cast(pid, {:cancel, request_ref})
      {:error, :timeout}

    :exit, _reason ->
      {:error, :disconnected}
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
    field :inbound_unix_fds, [UnixFD.t()], default: []
    # Ancillary data rejected before a complete D-Bus frame is known belongs
    # to that frame, not to a later coalesced frame. The descriptors themselves
    # are closed immediately; this bit makes the eventual frame a recoverable
    # drop once its byte boundary is available.
    field :inbound_fd_tainted?, boolean(), default: false
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
    field :partial_frame_timer, {reference(), reference()} | nil, default: nil
    field :unix_fd_transport?, boolean(), default: false
    field :unix_fd_negotiated?, boolean(), default: false
    # `nil` until org.freedesktop.DBus.Peer.GetMachineId is first served;
    # `:unavailable` caches a definitive negative lookup.
    field :machine_id, binary() | :unavailable | nil, default: nil
    # The outbound write queue, the frame currently being written, and the
    # serial counter that numbers them.
    field :writer, Writer.t(), default: Writer.new()

    field :pending,
          %{
            non_neg_integer() =>
              {:gen_statem.from(), reference(), reference(), reference(), integer()}
          },
          default: %{}

    field :request_index, %{reference() => non_neg_integer()}, default: %{}
    field :monitor_index, %{reference() => non_neg_integer()}, default: %{}
    field :fd_claims, %{reference() => map()}, default: %{}
    field :fd_claim_request_index, %{reference() => reference()}, default: %{}
    field :fd_claim_monitor_index, %{reference() => reference()}, default: %{}

    field :fd_claim_outcomes, %{reference() => {:acknowledged | :closed, reference()}},
      default: %{}

    field :signal_handler_monitor_index, %{reference() => reference()}, default: %{}
    field :signal_handler_ref_index, %{reference() => reference()}, default: %{}
    # Implementation modules behind the connection's side effects. Production
    # always uses the defaults; tests substitute a module rather than reaching
    # into per-operation state.
    field :impl, Rebus.Impl.t(), default: Rebus.Impl.default()
  end

  @impl true
  def init({opts, internal}) do
    %{family: family} = addr = Map.fetch!(internal, :addr)
    write_timeout = Keyword.get(opts, :write_timeout, @default_write_timeout)
    timeout = Keyword.get(opts, :timeout, @default_read_timeout)
    read_timeout = Keyword.get(opts, :read_timeout, @default_read_timeout)

    setup_timeout =
      Map.get(internal, :setup_timeout, Keyword.get(opts, :read_timeout, timeout))

    aggregate_setup_timeout? = Map.has_key?(internal, :setup_timeout)
    expected_guid = Map.get(internal, :expected_guid)
    precomputed_auth_id = Map.get(internal, :precomputed_auth_id)
    allow_anonymous? = Keyword.get(opts, :allow_anonymous, false)
    bus? = Keyword.get(opts, :bus, true)
    name = Keyword.get(opts, :name)
    connect_waiter = Map.get(internal, :connect_waiter)
    impl = Map.get_lazy(internal, :impl, &Rebus.Impl.default/0)

    cond do
      not (is_integer(write_timeout) and write_timeout > 0) ->
        {:stop, :invalid_write_timeout}

      not (is_integer(timeout) and timeout > 0) ->
        {:stop, :invalid_timeout}

      not (is_integer(read_timeout) and read_timeout > 0) ->
        {:stop, :invalid_read_timeout}

      not (is_integer(setup_timeout) and setup_timeout > 0) ->
        {:stop, :invalid_setup_timeout}

      not (is_nil(expected_guid) or Handshake.valid_guid?(expected_guid)) ->
        {:stop, :invalid_expected_guid}

      not (is_nil(precomputed_auth_id) or is_binary(precomputed_auth_id)) ->
        {:stop, :invalid_precomputed_auth_id}

      not is_boolean(allow_anonymous?) ->
        {:stop, :invalid_allow_anonymous}

      not is_boolean(bus?) ->
        {:stop, :invalid_bus_option}

      not (is_nil(name) or is_atom(name)) ->
        {:stop, :invalid_name}

      true ->
        # DynamicSupervisor stops children with an exit signal. Trap it so the
        # GenServer loop can return :stop and therefore invoke terminate/2,
        # which closes raw SCM_RIGHTS descriptors retained in partial frames or
        # reply claims. The EXIT clauses below preserve normal link semantics.
        Process.flag(:trap_exit, true)

        case impl.transport.open(family, :stream, :default) do
          {:ok, sock} ->
            _ = configure_receive_buffer(impl.transport, sock)

            {:ok,
             %__MODULE__{
               sock: sock,
               impl: impl,
               write_timeout: write_timeout,
               read_timeout: read_timeout,
               setup_timeout: setup_timeout,
               aggregate_setup_timeout?: aggregate_setup_timeout?,
               expected_guid: expected_guid,
               precomputed_auth_id: precomputed_auth_id,
               allow_anonymous?: allow_anonymous?,
               bus?: bus?,
               connect_waiter: connect_waiter,
               connect_waiter_monitor: monitor_connect_waiter(connect_waiter),
               unix_fd_transport?: unix_fd_transport_supported?(family)
             }, {:continue, {:setup, addr}}}

          {:error, reason} ->
            {:stop, normalize_socket_error(reason)}
        end
    end
  end

  defp transport(%__MODULE__{impl: %{transport: transport}}), do: transport

  @impl true
  def terminate(
        _reason,
        %__MODULE__{
          sock: sock,
          impl: impl,
          partial_frame_timer: timer_ref,
          inbound_unix_fds: inbound_unix_fds,
          fd_claims: fd_claims
        }
      ) do
    cancel_partial_frame_timer(timer_ref)
    _ = UnixFD.close_all(inbound_unix_fds)
    close_fd_claims(fd_claims)
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
    {:noreply, %{state | connect_accepted?: true}, {:continue, setup_continuation(state)}}
  end

  def handle_info(
        {:DOWN, ref, :process, _pid, _reason},
        %__MODULE__{connect_waiter_monitor: ref} = state
      ) do
    {:stop, {:shutdown, :caller_gone}, state}
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
        %__MODULE__{sock: s, writer: %Writer{active: %{wait: {:select, continuation, h}}}} = state
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
    case Map.pop(state.signal_handler_monitor_index, ref) do
      {handler_ref, signal_handler_monitor_index} when is_reference(handler_ref) ->
        :gen_event.delete_handler(SignalHandler, {SignalHandler, handler_ref}, nil)

        {:noreply,
         %{
           state
           | signal_handler_monitor_index: signal_handler_monitor_index,
             signal_handler_ref_index: Map.delete(state.signal_handler_ref_index, handler_ref)
         }}

      {nil, _signal_handler_monitor_index} ->
        handle_down_for_request(ref, state)
    end
  end

  def handle_info({:gen_event_EXIT, {SignalHandler, ref}, _reason}, %__MODULE__{} = state) do
    # Because handlers are added via :gen_event.add_sup_handler/3, we receive
    # `:gen_event_EXIT` messages when they are removed. We can use this to clean
    # up the monitor
    {:noreply, remove_signal_handler_monitor(state, ref)}
  end

  def handle_info({:request_timeout, serial, request_ref}, %__MODULE__{} = state) do
    case Map.fetch(state.pending, serial) do
      {:ok, {from, _timer_ref, ^request_ref, monitor_ref, _deadline}} ->
        {_pending_entry, pending} = Map.pop(state.pending, serial)
        Process.demonitor(monitor_ref, [:flush])
        GenServer.reply(from, {:error, :timeout})
        {:noreply, remove_indexes(%{state | pending: pending}, request_ref, monitor_ref)}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:fd_claim_timeout, claim_ref}, %__MODULE__{} = state) do
    case Map.fetch(state.fd_claims, claim_ref) do
      {:ok, _claim} ->
        Logger.warning("D-Bus FD reply claim dropped: :claim_timeout", reason: :claim_timeout)
        {:noreply, drop_fd_claim(state, claim_ref, close?: true)}

      :error ->
        {:noreply, state}
    end
  end

  def handle_info({:fd_claim_outcome_timeout, claim_ref}, %__MODULE__{} = state) do
    case Map.pop(state.fd_claim_outcomes, claim_ref) do
      {nil, _outcomes} -> {:noreply, state}
      {_outcome, outcomes} -> {:noreply, %{state | fd_claim_outcomes: outcomes}}
    end
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
        %__MODULE__{writer: %Writer{active: %{request_ref: request_ref}}} = state
      ) do
    state.writer |> Writer.write_timeout(writer_context(state)) |> writer_result(state)
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, writer: %Writer{active: %{wait: {:select, _continuation, h}}}} =
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

  defp handle_down_for_request(ref, %__MODULE__{} = state) do
    case Map.pop(state.monitor_index, ref) do
      {nil, _index} ->
        case Writer.pop_monitor(state.writer, ref) do
          :error ->
            case Map.fetch(state.fd_claim_monitor_index, ref) do
              {:ok, claim_ref} ->
                {:noreply, drop_fd_claim(state, claim_ref, close?: true, monitor_down?: true)}

              :error ->
                {:noreply, state}
            end

          {request_ref, writer} ->
            state = %{state | writer: writer}

            writer
            |> Writer.cancel_monitored(request_ref, writer_context(state))
            |> writer_result(state)
        end

      {serial, monitor_index} ->
        {entry, pending} = Map.pop(state.pending, serial)
        {_from, timer_ref, request_ref, _monitor_ref, _deadline} = entry
        _ = Process.cancel_timer(timer_ref)

        {:noreply,
         %{
           state
           | pending: pending,
             monitor_index: monitor_index,
             request_index: Map.delete(state.request_index, request_ref)
         }}
    end
  end

  @impl true

  def handle_continue({:setup, addr}, %__MODULE__{} = state) do
    if connect_waiter_gone?(state) do
      {:stop, {:shutdown, :caller_gone}, state}
    else
      case initialize(state, addr) do
        {:ok, initialized, {:continue, continuation}} ->
          if is_nil(initialized.connect_waiter) do
            {:noreply, initialized, {:continue, continuation}}
          else
            notify_connect_waiter(initialized.connect_waiter, {:ok, self()})
            {:noreply, initialized}
          end

        {:stop, reason} ->
          if connect_waiter_alive?(state) do
            notify_connect_waiter(state.connect_waiter, {:error, reason})
            {:stop, {:shutdown, reason}, state}
          else
            {:stop, {:shutdown, :caller_gone}, state}
          end
      end
    end
  end

  def handle_continue(:hello, %__MODULE__{} = state) do
    # Send the Hello method call
    with {:ok, method} <-
           Message.new(:method_call,
             path: "/",
             interface: "org.freedesktop.DBus",
             destination: "org.freedesktop.DBus",
             member: "Hello"
           ),
         {:ok, bin} <- Message.encode(%{method | serial: Writer.serial(state.writer)}) do
      case transport(state).send(state.sock, bin, [], state.write_timeout) do
        :ok ->
          {:noreply,
           %{
             state
             | hello_serial: Writer.serial(state.writer),
               writer: Writer.consume_serial(state.writer)
           }, {:continue, :hello_reply_buffer}}

        {:error, reason} ->
          stop_for_transport_error(reason, state)

        _unexpected ->
          stop_for_transport_error(:send_failed, state)
      end
    else
      {:error, reason} -> stop_for_protocol_error(reason, state)
    end
  end

  # A peer-to-peer endpoint has no bus driver, so there is no Hello to send or
  # correlate. The connection is established as soon as the handshake finishes,
  # with no unique name, and joins the ordinary receive loop. Authentication may
  # already have read peer frames alongside its final response; those buffered
  # bytes are ordinary inbound traffic here.
  def handle_continue(:established, %__MODULE__{} = state) do
    case establish_connection(%{state | established?: true}) do
      {:ok, state} -> process_inbound(state, :recv)
      {:error, :caller_gone} -> {:stop, {:shutdown, :caller_gone}, state}
    end
  end

  def handle_continue(:hello_reply_buffer, %__MODULE__{} = state) do
    # Authentication may have read D-Bus bytes along with its final response.
    process_inbound(state, {:hello_reply, read_deadline(state.read_timeout)})
  end

  def handle_continue(:hello_reply, %__MODULE__{} = state) do
    receive_hello_reply(state, read_deadline(state.read_timeout))
  end

  def handle_continue({:hello_reply, deadline}, %__MODULE__{} = state) do
    receive_hello_reply(state, deadline)
  end

  def handle_continue(:recv, %__MODULE__{rref: nil} = state) do
    cond do
      state.unix_fd_negotiated? ->
        transport(state).recvmsg(
          state.sock,
          Inbound.receive_size(state.inbound, @max_read_chunk),
          @max_unix_fd_control_size,
          [],
          :nowait
        )
        |> handle_receive_result(state)

      # OTP documents CtrlSz=0 as its default control-buffer size, not as a
      # request to discard ancillary data. Keep the normal coalescing byte
      # path, but receive a bounded cmsg and close any illicit rights before a
      # partial frame can retain them.
      state.unix_fd_transport? ->
        transport(state).recvmsg(state.sock, 0, @max_unix_fd_control_size, [], :nowait)
        |> handle_receive_result(state)

      true ->
        handle_receive_result(transport(state).recv(state.sock, 0, [], :nowait), state)
    end
  end

  # A pending socket operation owns the receive continuation. Keeping this
  # catch-all prevents a stale continuation from crashing and exposing state.
  def handle_continue(:recv, %__MODULE__{} = state), do: {:noreply, state}

  def handle_continue(:write, %__MODULE__{} = state), do: advance_writes(state)

  @doc false
  def handle_receive_result({:ok, data}, %__MODULE__{} = state) when is_binary(data) do
    append_inbound(data, state, :recv)
  end

  def handle_receive_result({:ok, message}, %__MODULE__{} = state) when is_map(message) do
    append_recvmsg(message, state, :recv)
  end

  def handle_receive_result(
        {:select, {:select_info, :recv, handle}},
        %__MODULE__{} = state
      ) do
    {:noreply, %{state | rref: handle}}
  end

  def handle_receive_result(
        {:select, {:select_info, :recvmsg, handle}},
        %__MODULE__{} = state
      ) do
    {:noreply, %{state | rref: handle}}
  end

  def handle_receive_result(
        {:select, {{:select_info, :recv, handle}, data}},
        %__MODULE__{} = state
      )
      when is_binary(data) do
    append_inbound(data, %{state | rref: handle}, :recv)
  end

  def handle_receive_result(
        {:select, {{:select_info, :recvmsg, handle}, message}},
        %__MODULE__{} = state
      )
      when is_map(message) do
    append_recvmsg(message, %{state | rref: handle}, :recv)
  end

  def handle_receive_result({:error, reason}, %__MODULE__{} = state) do
    stop_for_transport_error(reason, state)
  end

  def handle_receive_result(_result, %__MODULE__{} = state) do
    stop_for_transport_error(:receive_failed, state)
  end

  defp receive_hello_reply(%__MODULE__{} = state, deadline) do
    case remaining_timeout(deadline, state.read_timeout) do
      :expired ->
        stop_for_protocol_error(:read_timeout, state)

      {:ok, timeout} ->
        receive_hello_reply(state, deadline, timeout)
    end
  end

  defp receive_hello_reply(%__MODULE__{} = state, deadline, timeout) do
    if state.unix_fd_transport? do
      receive_hello_reply_recvmsg(state, deadline, timeout)
    else
      case transport(state).recv(state.sock, 0, [], timeout) do
        {:ok, data} ->
          continue_hello_reply(data, state, deadline)

        {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
          continue_hello_reply(data, state, deadline)

        {:error, :timeout} ->
          stop_for_protocol_error(:read_timeout, state)

        {:error, {:timeout, _data}} ->
          stop_for_protocol_error(:read_timeout, state)

        {:error, reason} ->
          stop_for_transport_error(reason, state)
      end
    end
  end

  # After local transport negotiation, every peer read—including the initial
  # Hello reply—must observe SCM_RIGHTS. A plain recv/4 here could discard
  # ancillary metadata outside the single close-or-deliver ownership path.
  defp receive_hello_reply_recvmsg(%__MODULE__{} = state, deadline, timeout) do
    case transport(state).recvmsg(
           state.sock,
           Inbound.receive_size(state.inbound, @max_read_chunk),
           recvmsg_control_size(state),
           [],
           timeout
         ) do
      {:ok, message} when is_map(message) ->
        continue_hello_reply_recvmsg(message, state, deadline)

      {:error, {:timeout, message}} when is_map(message) ->
        continue_hello_reply_recvmsg(message, state, deadline)

      {:error, :timeout} ->
        stop_for_protocol_error(:read_timeout, state)

      {:error, {:timeout, _message}} ->
        stop_for_protocol_error(:read_timeout, state)

      {:error, reason} ->
        stop_for_transport_error(reason, state)

      _unexpected ->
        stop_for_transport_error(:receive_failed, state)
    end
  end

  defp recvmsg_control_size(%__MODULE__{}), do: @max_unix_fd_control_size

  defp continue_hello_reply(data, %__MODULE__{} = state, deadline) do
    case append_inbound(data, state, {:hello_reply, deadline}) do
      {:noreply, %__MODULE__{} = state, {:continue, {:hello_reply, _deadline}}} ->
        receive_hello_reply(state, deadline)

      result ->
        result
    end
  end

  defp continue_hello_reply_recvmsg(message, %__MODULE__{} = state, deadline) do
    case append_recvmsg(message, state, {:hello_reply, deadline}) do
      {:noreply, %__MODULE__{} = state, {:continue, {:hello_reply, _deadline}}} ->
        receive_hello_reply(state, deadline)

      result ->
        result
    end
  end

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

  # The public call's reply alias carries only the claim token. The
  # descriptor-bearing message uses a caller-created one-shot alias, which is
  # explicitly unaliased on every timeout path. That prevents a late internal
  # delivery from reaching application `handle_info/2` after Connection.call/3
  # has returned.
  def handle_call(
        {:claim_fd_reply, claim_ref, delivery_ref, delivery_alias},
        {pid, _tag},
        %__MODULE__{} = state
      ) do
    case Map.fetch(state.fd_claims, claim_ref) do
      {:ok, %{pid: ^pid, delivery_ref: nil, msg: msg} = claim}
      when is_reference(delivery_ref) and is_reference(delivery_alias) ->
        if fd_claim_live?(claim) and Process.alive?(pid) do
          claim = rearm_fd_claim(claim_ref, claim)
          state.impl.hooks.fd_claim_delivery()

          if fd_claim_live?(claim) and Process.alive?(pid) do
            send(delivery_alias, {:rebus_fd_reply, claim_ref, delivery_ref, msg})

            {:reply, :ok,
             %{
               state
               | fd_claims:
                   Map.put(state.fd_claims, claim_ref, %{
                     claim
                     | delivery_ref: delivery_ref,
                       delivery_alias: delivery_alias
                   })
             }}
          else
            {:reply, {:error, :fd_claim_expired},
             drop_fd_claim(state, claim_ref, close?: true, outcome: :closed)}
          end
        else
          {:reply, {:error, :fd_claim_expired},
           drop_fd_claim(state, claim_ref, close?: true, outcome: :closed)}
        end

      _ ->
        {:reply, {:error, :fd_claim_expired}, state}
    end
  end

  def handle_call({:ack_fd_reply, claim_ref, delivery_ref}, {pid, _tag}, %__MODULE__{} = state) do
    case Map.fetch(state.fd_claims, claim_ref) do
      {:ok, %{pid: ^pid, delivery_ref: ^delivery_ref} = claim} ->
        state.impl.hooks.fd_claim_ack(claim)

        # A call alias timing out does not revoke a queued acknowledgement. It
        # is the resolver's FIFO position after this message that makes its
        # outcome definitive. Never acknowledge after the claim deadline,
        # though: at that point the connection must retain and close the FD.
        if fd_claim_live?(claim) and Process.alive?(pid) do
          {:reply, :ok, drop_fd_claim(state, claim_ref, close?: false, outcome: :acknowledged)}
        else
          {:reply, {:error, :fd_claim_expired},
           drop_fd_claim(state, claim_ref, close?: true, outcome: :closed)}
        end

      _ ->
        {:reply, {:error, :fd_claim_expired}, state}
    end
  end

  # This ordered descriptor-free barrier is used only if the bounded ack call
  # times out. It serializes behind a queued acknowledgement: either that ack
  # transferred ownership (and we report it), or this handler closes the claim.
  # Connection.call/3 waits for this handler without another finite timeout so
  # it never reports a closed claim while an earlier acknowledgement can still
  # transfer ownership.
  def handle_call({:resolve_fd_claim, claim_ref, delivery_ref}, _from, %__MODULE__{} = state) do
    case Map.fetch(state.fd_claims, claim_ref) do
      {:ok, %{delivery_ref: ^delivery_ref}} ->
        {:reply, :closed, drop_fd_claim(state, claim_ref, close?: true, outcome: :closed)}

      _ ->
        {outcome, state} = take_fd_claim_outcome(state, claim_ref)
        {:reply, outcome || :fd_claim_expired, state}
    end
  end

  def handle_call({:discard_fd_claim, claim_ref}, {pid, _tag}, %__MODULE__{} = state) do
    case Map.fetch(state.fd_claims, claim_ref) do
      {:ok, %{pid: ^pid}} ->
        {:reply, :ok, drop_fd_claim(state, claim_ref, close?: true, outcome: :closed)}

      _ ->
        {:reply, :ok, state}
    end
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

  def handle_call({:delete_signal_handler, _ref}, _from, %__MODULE__{established?: false} = state) do
    {:reply, {:error, :not_connected}, state}
  end

  def handle_call({:delete_signal_handler, ref}, _from, %__MODULE__{} = state) do
    {:reply, :ok, remove_signal_handler(state, ref)}
  end

  defp add_signal_handler(%__MODULE__{} = state, pid, handler_ref, rule) do
    monitor_ref = Process.monitor(pid)

    handler_state =
      if is_nil(rule),
        do: {self(), pid, handler_ref},
        else: {self(), pid, handler_ref, rule}

    :ok =
      :gen_event.add_sup_handler(
        SignalHandler,
        {SignalHandler, handler_ref},
        handler_state
      )

    {:reply, {:ok, handler_ref},
     %{
       state
       | signal_handler_monitor_index:
           Map.put(state.signal_handler_monitor_index, monitor_ref, handler_ref),
         signal_handler_ref_index:
           Map.put(state.signal_handler_ref_index, handler_ref, monitor_ref)
     }}
  end

  @impl true
  def handle_cast({:cancel, request_ref}, %__MODULE__{} = state) do
    case Map.pop(state.request_index, request_ref) do
      {nil, _index} ->
        case Map.fetch(state.fd_claim_request_index, request_ref) do
          {:ok, claim_ref} ->
            {:noreply, drop_fd_claim(state, claim_ref, close?: true)}

          :error ->
            state.writer
            |> Writer.cancel(request_ref, writer_context(state))
            |> writer_result(state)
        end

      {serial, request_index} ->
        {entry, pending} = Map.pop(state.pending, serial)
        {_from, timer_ref, _request_ref, monitor_ref, _deadline} = entry
        _ = Process.cancel_timer(timer_ref)
        Process.demonitor(monitor_ref, [:flush])

        {:noreply,
         %{
           state
           | pending: pending,
             request_index: request_index,
             monitor_index: Map.delete(state.monitor_index, monitor_ref)
         }}
    end
  end

  def handle_cast({:cancel_signal_handler, handler_ref}, %__MODULE__{} = state) do
    {:noreply, remove_signal_handler(state, handler_ref)}
  end

  defp parse_complete_message(data, %__MODULE__{} = state, continuation) do
    parse_flat_messages(data, state, continuation, data)
  end

  # `data` is already flat when a complete frame is available. Parse every
  # coalesced frame directly from its sub-binary remainder, retaining only the
  # final incomplete tail. This avoids re-flattening a receive buffer per frame.
  defp parse_flat_messages(<<>>, %__MODULE__{} = state, continuation, _source) do
    process_inbound(state, continuation)
  end

  defp parse_flat_messages(data, %__MODULE__{} = state, continuation, source) do
    case Message.parse_inbound(data) do
      {:ok, %Message{} = msg, rest} ->
        case attach_inbound_fds(msg, state) do
          {:ok, msg, state} ->
            dispatch_inbound_message(msg, rest, state, continuation, source)

          {:error, reason, state} ->
            drop_recoverable_fd_frame(reason, rest, state, continuation, source)
        end

      nil ->
        append_inbound(Inbound.retain_remainder(data, source), state, continuation)

      {:error, :resource_limit, _envelope, _rest} when not is_nil(state.hello_serial) ->
        stop_for_protocol_error({:hello_failed, :resource_limit}, finish_frame(state))

      {:error, :resource_limit} when not is_nil(state.hello_serial) ->
        stop_for_protocol_error({:hello_failed, :resource_limit}, finish_frame(state))

      {:error, :resource_limit, envelope, rest} ->
        Logger.warning("D-Bus frame dropped: :resource_limit", reason: :resource_limit)
        state = discard_inbound_unix_fds(state)
        {:ok, state} = drop_resource_limited_reply(envelope, state)
        parse_flat_messages(rest, finish_frame(state), continuation, source)

      {:error, :resource_limit} ->
        Logger.warning("D-Bus frame dropped: :resource_limit", reason: :resource_limit)
        state = discard_inbound_unix_fds(state)

        case Message.expected_size(data) do
          {:ok, frame_size} ->
            <<_dropped::binary-size(^frame_size), rest::binary>> = data
            parse_flat_messages(rest, finish_frame(state), continuation, source)

          _ ->
            stop_for_protocol_error(:invalid_message, state)
        end

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  defp attach_inbound_fds(%Message{} = msg, %__MODULE__{} = state) do
    fds = state.inbound_unix_fds
    tainted? = state.inbound_fd_tainted?
    state = %{state | inbound_unix_fds: [], inbound_fd_tainted?: false}

    with :ok <- inbound_fd_frame_clean?(tainted?),
         :ok <- inbound_fd_negotiated?(msg, state),
         {:ok, msg} <- Message.attach_unix_fds(msg, fds) do
      {:ok, msg, state}
    else
      {:error, reason} ->
        _ = UnixFD.close_all(fds)
        {:error, reason, state}
    end
  end

  defp inbound_fd_frame_clean?(false), do: :ok
  defp inbound_fd_frame_clean?(true), do: {:error, :invalid_unix_fds}

  # Count/index/negotiation checks run after a complete D-Bus frame and its
  # ancillary data have been collected. The stream boundary is therefore known:
  # close the descriptors, drop only this frame, and continue with a coalesced
  # successor rather than letting a peer kill unrelated calls or handlers.
  defp drop_recoverable_fd_frame(reason, rest, state, continuation, source) do
    reason = fd_drop_reason(reason)
    Logger.warning("D-Bus FD frame dropped: #{inspect(reason)}", reason: reason)
    parse_flat_messages(rest, finish_frame(state), continuation, source)
  end

  defp fd_drop_reason(reason)
       when reason in [:invalid_unix_fds, :unix_fd_not_negotiated, :unix_fd_limit],
       do: reason

  defp fd_drop_reason(_reason), do: :invalid_unix_fds

  defp inbound_fd_negotiated?(%Message{header_fields: header_fields, unix_fds: fds}, state) do
    if state.unix_fd_negotiated? or (Map.get(header_fields, :unix_fds, 0) == 0 and fds == []) do
      :ok
    else
      {:error, :unix_fd_not_negotiated}
    end
  end

  defp dispatch_inbound_message(
         %Message{} = msg,
         rest,
         %__MODULE__{hello_serial: hello_serial} = state,
         _continuation,
         source
       )
       when not is_nil(hello_serial) do
    state = finish_frame(state)

    # dbus-daemon's bus/dispatch.c replies to Hello before emitting the
    # directed NameAcquired signal. Until that reply supplies our unique name,
    # any other frame is a protocol error rather than application traffic.
    if msg.unix_fds != [] do
      close_message_fds(msg)
      stop_for_protocol_error(:invalid_unix_fds, state)
    else
      case hello_reply_result(msg, hello_serial) do
        {:ok, name} ->
          case establish_connection(%{state | name: name, hello_serial: nil, established?: true}) do
            {:ok, state} -> parse_flat_messages(rest, state, :recv, source)
            {:error, :caller_gone} -> {:stop, {:shutdown, :caller_gone}, state}
          end

        {:error, reason} ->
          stop_for_protocol_error(reason, state)
      end
    end
  end

  defp dispatch_inbound_message(
         %Message{type: type} = msg,
         rest,
         %__MODULE__{} = state,
         continuation,
         source
       )
       when type in [:method_return, :error] do
    state = finish_frame(state)

    case reply(msg, state) do
      {:ok, state} -> parse_flat_messages(rest, state, continuation, source)
      {:error, reason} -> stop_for_protocol_error(reason, state)
    end
  end

  defp dispatch_inbound_message(
         %Message{type: :signal} = msg,
         rest,
         %__MODULE__{} = state,
         continuation,
         source
       ) do
    # Signals may have multiple subscribers. Without a per-subscriber dup(2)
    # primitive, one raw descriptor cannot be transferred safely to all of
    # them, so FD-bearing signals are rejected and closed.
    if msg.unix_fds == [] do
      parse_flat_messages(rest, notify(msg, finish_frame(state)), continuation, source)
    else
      close_message_fds(msg)
      Logger.warning("D-Bus FD frame dropped: :signal_ownership", reason: :signal_ownership)
      parse_flat_messages(rest, finish_frame(state), continuation, source)
    end
  end

  defp dispatch_inbound_message(
         %Message{type: :method_call} = msg,
         rest,
         %__MODULE__{} = state,
         continuation,
         source
       ) do
    # No method served by this connection takes a descriptor, so any received
    # descriptor is closed before the call is answered.
    close_message_fds(msg)
    state = answer_method_call(msg, finish_frame(state))
    parse_flat_messages(rest, state, continuation, source)
  end

  defp dispatch_inbound_message(%Message{} = msg, rest, state, continuation, source) do
    close_message_fds(msg)
    parse_flat_messages(rest, finish_frame(state), continuation, source)
  end

  # Rebus has no service-side API, so every inbound method call is answered
  # here: `org.freedesktop.DBus.Peer` is implemented, everything else is
  # refused with `UnknownMethod`. A caller that asked for no reply gets none.
  defp answer_method_call(%Message{flags: flags} = msg, %__MODULE__{} = state) do
    cond do
      :no_reply_expected in flags ->
        state

      Writer.replies_saturated?(state.writer) ->
        %{state | writer: Writer.refuse_reply(state.writer)}

      true ->
        {reply_opts, state} = method_call_reply(msg, state)
        queue_method_call_reply(reply_opts, msg, state)
    end
  end

  defp method_call_reply(%Message{header_fields: header_fields}, %__MODULE__{} = state) do
    interface = Map.get(header_fields, :interface)
    member = Map.get(header_fields, :member)

    case {interface, member} do
      {interface, "Ping"} when interface in [nil, @peer_interface] ->
        {[type: :method_return], state}

      {interface, "GetMachineId"} when interface in [nil, @peer_interface] ->
        machine_id_reply(state)

      _other ->
        {unknown_method_reply(), state}
    end
  end

  defp machine_id_reply(%__MODULE__{} = state) do
    case machine_id(state) do
      {{:ok, id}, state} ->
        {[type: :method_return, signature: "s", body: [id]], state}

      {{:error, :unavailable}, state} ->
        {[
           type: :error,
           error_name: @failed_error,
           signature: "s",
           body: [@machine_id_unavailable_message]
         ], state}
    end
  end

  # The reply body never echoes caller-supplied data: a fixed sentence keeps
  # peer-controlled bytes out of the frames this connection emits.
  defp unknown_method_reply do
    [
      type: :error,
      error_name: @unknown_method_error,
      signature: "s",
      body: [@unknown_method_message]
    ]
  end

  # The machine id is read on first use and then cached for the connection's
  # life, including a negative result: a peer that floods GetMachineId must not
  # turn into a stream of file reads.
  defp machine_id(%__MODULE__{machine_id: nil} = state) do
    case MachineId.read() do
      {:ok, id} -> {{:ok, id}, %{state | machine_id: id}}
      {:error, :unavailable} -> {{:error, :unavailable}, %{state | machine_id: :unavailable}}
    end
  end

  defp machine_id(%__MODULE__{machine_id: :unavailable} = state),
    do: {{:error, :unavailable}, state}

  defp machine_id(%__MODULE__{machine_id: id} = state), do: {{:ok, id}, state}

  defp queue_method_call_reply(reply_opts, %Message{} = msg, %__MODULE__{} = state) do
    {type, reply_opts} = Keyword.pop!(reply_opts, :type)
    reply_opts = reply_opts ++ [reply_serial: msg.serial] ++ reply_destination(msg)

    case Message.new(type, reply_opts) do
      {:ok, reply} ->
        writer =
          Writer.queue(state.writer, %{
            kind: :reply,
            from: nil,
            msg: reply,
            deadline: System.monotonic_time(:millisecond) + state.write_timeout,
            request_ref: make_ref()
          })

        kick_writes(%{state | writer: writer})

      {:error, reason} ->
        Logger.warning("D-Bus internal reply dropped: #{inspect(reason)}", reason: reason)
        state
    end
  end

  # A bus sets `sender` on every forwarded frame; a peer-to-peer caller has no
  # name, and its reply carries no destination. The name is copied so a small
  # retained header cannot pin the receive buffer it was parsed from.
  defp reply_destination(%Message{header_fields: %{sender: sender}}) when is_binary(sender),
    do: [destination: :binary.copy(sender)]

  defp reply_destination(%Message{}), do: []

  # The inbound path owns its own continuation (it must keep parsing coalesced
  # frames and re-arm the reader), so it cannot return the writer's. This
  # self-message starts the writer instead, after the current callback returns.
  defp kick_writes(%__MODULE__{} = state) do
    send(self(), :advance_writes)
    state
  end

  defp close_message_fds(%Message{unix_fds: fds}), do: UnixFD.close_all(fds)

  defp initialize(%__MODULE__{aggregate_setup_timeout?: true} = state, addr) do
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

  defp initialize(%__MODULE__{} = state, addr) do
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

  defp handshake_options(%__MODULE__{impl: impl} = state) do
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
     }, {:continue, setup_continuation(state)}}
  end

  defp setup_continuation(%__MODULE__{bus?: false}), do: :established
  defp setup_continuation(%__MODULE__{}), do: :hello

  defp aggregate_setup_auth_id(%__MODULE__{precomputed_auth_id: auth_id}, _deadline)
       when is_binary(auth_id),
       do: {:ok, auth_id}

  defp aggregate_setup_auth_id(%__MODULE__{} = state, deadline) do
    with {:ok, auth_id_timeout} <- remaining_setup_timeout(deadline, state.setup_timeout) do
      Handshake.get_auth_id(auth_id_timeout, state.impl.identity)
    end
  end

  defp setup_auth_id(%__MODULE__{precomputed_auth_id: auth_id}, _timeout) when is_binary(auth_id),
    do: {:ok, auth_id}

  defp setup_auth_id(%__MODULE__{} = state, timeout),
    do: Handshake.get_auth_id(timeout, state.impl.identity)

  defp notify_connect_waiter({pid, ref}, result) when is_pid(pid) and is_reference(ref),
    do: send(pid, {ref, result})

  defp notify_connect_waiter(nil, _result), do: :ok

  defp establish_connection(
         %__MODULE__{connect_waiter: {pid, connect_ref}, connect_waiter_monitor: monitor_ref} =
           state
       ) do
    receive do
      {:DOWN, ^monitor_ref, :process, ^pid, _reason} ->
        {:error, :caller_gone}
    after
      0 ->
        # This acknowledgement is the ownership-transfer boundary. Check the
        # queued monitor event first, then send the acknowledgement before
        # releasing the monitor: a caller that dies after this send owns the
        # normal established-connection lifecycle, while a prior death wins.
        send(pid, {connect_ref, :accepted})
        {:ok, release_connect_waiter(state)}
    end
  end

  defp establish_connection(%__MODULE__{} = state), do: {:ok, state}

  defp safe_setup_call(conn, message, cancellation \\ nil, timeout \\ @default_read_timeout) do
    GenServer.call(conn, message, timeout)
  catch
    :exit, {:timeout, _call} ->
      if cancellation, do: GenServer.cast(conn, cancellation)
      {:error, :timeout}

    :exit, _reason ->
      {:error, :disconnected}
  end

  defp receive_fd_reply_claim({:fd_claim, claim_ref}, conn, deadline, _request_ref)
       when is_reference(claim_ref) do
    delivery_ref = make_ref()
    # An alias is the delivery address, not a process mailbox convention. On
    # timeout `unalias/1` atomically rejects in-flight sends; the small drain
    # below merely consumes a message already enqueued before that operation.
    delivery_alias = :erlang.alias([:reply])

    await_fd_reply(conn, claim_ref, delivery_ref, delivery_alias, deadline)
  end

  defp receive_fd_reply_claim(%Message{} = msg, _conn, _deadline, _request_ref),
    do: reply_result(msg)

  defp receive_fd_reply_claim(result, _conn, _deadline, _request_ref), do: result

  # A D-Bus error reply is a definitive peer answer, not a transport failure,
  # but callers should not have to test the type to branch on it. The complete
  # message is retained in either shape so its error name, body and any owned
  # descriptors stay available to the caller.
  defp reply_result(%Message{type: :error} = msg), do: {:error, msg}
  defp reply_result(%Message{} = msg), do: {:ok, msg}

  defp await_fd_reply(conn, claim_ref, delivery_ref, delivery_alias, deadline) do
    with {:ok, timeout} <- fd_claim_remaining_timeout(deadline),
         :ok <- claim_fd_reply(conn, claim_ref, delivery_ref, delivery_alias, timeout) do
      receive do
        {:rebus_fd_reply, ^claim_ref, ^delivery_ref, %Message{} = msg} ->
          # Ownership moves only after the server acknowledges the claim.
          # The first acknowledgement is bounded by the original request
          # deadline plus the handoff grace. If its reply races that bound,
          # the FIFO resolver waits for the definitive transfer-or-close
          # outcome rather than returning an ambiguous raw descriptor.
          case acknowledge_fd_reply(conn, claim_ref, delivery_ref, deadline) do
            :ok -> reply_result(msg)
            {:error, _reason} = error -> error
          end
      after
        timeout ->
          discard_fd_claim(conn, claim_ref, deadline)
          {:error, :timeout}
      end
    else
      {:error, :timeout} ->
        discard_fd_claim(conn, claim_ref, deadline)
        {:error, :timeout}

      {:error, _reason} = error ->
        discard_fd_claim(conn, claim_ref, deadline)
        error
    end
  after
    :erlang.unalias(delivery_alias)
    drain_fd_reply_delivery(claim_ref, delivery_ref)
  end

  defp claim_fd_reply(conn, claim_ref, delivery_ref, delivery_alias, timeout) do
    case GenServer.call(
           conn,
           {:claim_fd_reply, claim_ref, delivery_ref, delivery_alias},
           timeout
         ) do
      :ok -> :ok
      {:error, _reason} = error -> error
      _unexpected -> {:error, :fd_claim_expired}
    end
  catch
    :exit, {:timeout, _call} -> {:error, :timeout}
    :exit, _reason -> {:error, :disconnected}
  end

  defp acknowledge_fd_reply(conn, claim_ref, delivery_ref, deadline) do
    case fd_claim_remaining_timeout(deadline) do
      {:ok, timeout} ->
        call_ack_fd_reply(conn, claim_ref, delivery_ref, timeout)

      :error ->
        resolve_fd_claim(conn, claim_ref, delivery_ref)
    end
  end

  defp call_ack_fd_reply(conn, claim_ref, delivery_ref, timeout) do
    case GenServer.call(conn, {:ack_fd_reply, claim_ref, delivery_ref}, timeout) do
      :ok -> :ok
      {:error, _reason} = error -> error
      _unexpected -> {:error, :fd_claim_expired}
    end
  catch
    :exit, {:timeout, _call} -> resolve_fd_claim(conn, claim_ref, delivery_ref)
    :exit, _reason -> {:error, :disconnected}
  end

  defp resolve_fd_claim(conn, claim_ref, delivery_ref) do
    # The bounded acknowledgement call may time out after its message is
    # already queued. This call is deliberately FIFO and unbounded: every
    # production Connection callback after setup uses :nowait socket I/O and
    # bounded local work, so a live process will dispatch it. A test seam can
    # stall a callback to cover that ordering; the public docs make the rare
    # extended wait explicit. If the connection dies, its monitor makes the
    # only indeterminate case explicit as :disconnected.
    monitor_ref = Process.monitor(conn)

    await_fd_claim_resolution(conn, claim_ref, delivery_ref, monitor_ref)
  end

  defp await_fd_claim_resolution(conn, claim_ref, delivery_ref, monitor_ref) do
    case GenServer.call(conn, {:resolve_fd_claim, claim_ref, delivery_ref}, :infinity) do
      :acknowledged -> :ok
      _ -> {:error, :fd_claim_expired}
    end
  catch
    :exit, _reason -> {:error, :disconnected}
  after
    Process.demonitor(monitor_ref, [:flush])
  end

  defp discard_fd_claim(conn, claim_ref, deadline) do
    case fd_claim_cleanup_remaining_timeout(deadline) do
      {:ok, timeout} ->
        call_discard_fd_claim(conn, claim_ref, timeout)

      :error ->
        :ok
    end
  end

  defp call_discard_fd_claim(conn, claim_ref, timeout) do
    _ = GenServer.call(conn, {:discard_fd_claim, claim_ref}, timeout)
    :ok
  catch
    :exit, _reason -> :ok
  end

  defp drain_fd_reply_delivery(claim_ref, delivery_ref) do
    receive do
      {:rebus_fd_reply, ^claim_ref, ^delivery_ref, %Message{}} -> :ok
    after
      0 -> :ok
    end
  end

  defp fd_claim_remaining_timeout(deadline) do
    remaining = deadline + @fd_claim_handoff_grace - System.monotonic_time(:millisecond)
    if remaining > 0, do: {:ok, remaining}, else: :error
  end

  defp fd_claim_cleanup_remaining_timeout(deadline) do
    remaining = deadline + @fd_claim_cleanup_grace - System.monotonic_time(:millisecond)
    if remaining > 0, do: {:ok, remaining}, else: :error
  end

  defp monitor_connect_waiter({pid, _ref}) when is_pid(pid), do: Process.monitor(pid)
  defp monitor_connect_waiter(nil), do: nil

  defp connect_waiter_alive?(%__MODULE__{connect_waiter: nil}), do: true

  defp connect_waiter_alive?(%__MODULE__{connect_waiter: {pid, _ref}}), do: Process.alive?(pid)

  defp connect_waiter_gone?(%__MODULE__{connect_waiter: nil}), do: false

  defp connect_waiter_gone?(%__MODULE__{
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

  defp release_connect_waiter(%__MODULE__{connect_waiter_monitor: monitor_ref} = state)
       when is_reference(monitor_ref) do
    Process.demonitor(monitor_ref, [:flush])
    %{state | connect_waiter: nil, connect_waiter_monitor: nil, connect_accepted?: false}
  end

  defp release_connect_waiter(%__MODULE__{} = state),
    do: %{state | connect_waiter: nil, connect_accepted?: false}

  defp remove_signal_handler(%__MODULE__{} = state, handler_ref) do
    case pop_signal_handler_monitor(state, handler_ref) do
      {:ok, state} ->
        :gen_event.delete_handler(SignalHandler, {SignalHandler, handler_ref}, nil)
        state

      :error ->
        state
    end
  end

  defp remove_signal_handler_monitor(%__MODULE__{} = state, handler_ref) do
    case pop_signal_handler_monitor(state, handler_ref) do
      {:ok, state} -> state
      :error -> state
    end
  end

  defp pop_signal_handler_monitor(%__MODULE__{} = state, handler_ref) do
    case Map.pop(state.signal_handler_ref_index, handler_ref) do
      {monitor_ref, signal_handler_ref_index} when is_reference(monitor_ref) ->
        Process.demonitor(monitor_ref, [:flush])

        {:ok,
         %{
           state
           | signal_handler_monitor_index:
               Map.delete(state.signal_handler_monitor_index, monitor_ref),
             signal_handler_ref_index: signal_handler_ref_index
         }}

      {nil, _signal_handler_ref_index} ->
        :error
    end
  end

  defp stop_and_close(transport, sock, reason) do
    _ = transport.close(sock)
    {:stop, normalize_socket_error(reason)}
  end

  defp connect_socket(transport, sock, addr, timeout) do
    case transport.connect(sock, addr, timeout) do
      :ok -> :ok
      {:error, :timeout} -> {:error, :read_timeout}
      {:error, reason} -> {:error, reason}
    end
  end

  defp unix_fd_transport_supported?(:local) do
    :os.type() in [{:unix, :linux}, {:unix, :darwin}] and
      function_exported?(:socket, :sendmsg, 4) and
      function_exported?(:socket, :recvmsg, 5)
  end

  defp unix_fd_transport_supported?(_family), do: false

  @doc false
  @spec configure_receive_buffer(module(), :socket.socket()) :: :tuple | :scalar | :default
  def configure_receive_buffer(transport, sock) do
    # A zero-length receive returns the bytes currently available on every
    # supported OTP release. Keep the backing allocation independent of a
    # peer-declared D-Bus frame length. Some backends only accept the scalar
    # form, so failure to tune this hint must never make connections unavailable.
    case transport.setopt(sock, {:otp, :rcvbuf}, {@max_read_attempts, @max_read_chunk}) do
      :ok ->
        :tuple

      {:error, _reason} ->
        case transport.setopt(sock, {:otp, :rcvbuf}, @max_read_chunk) do
          :ok ->
            :scalar

          {:error, _reason} ->
            default_receive_buffer()

          _other ->
            default_receive_buffer()
        end

      _other ->
        default_receive_buffer()
    end
  end

  defp default_receive_buffer do
    Logger.warning("D-Bus connection is using OTP's default receive buffer")
    :default
  end

  @doc false
  @spec normalize_socket_error(term()) :: term()
  def normalize_socket_error(reason), do: SocketError.normalize(reason)

  defp stop_for_transport_error(reason, %__MODULE__{} = state) do
    reason = normalize_socket_error(reason)
    Logger.warning("D-Bus connection transport stopped: #{inspect(reason)}", reason: reason)
    {:stop, {:shutdown, reason}, state |> discard_inbound_unix_fds() |> fail_pending()}
  end

  defp stop_for_protocol_error(reason, %__MODULE__{} = state) do
    reason = sanitize_protocol_reason(reason)
    Logger.warning("D-Bus connection protocol stopped: #{inspect(reason)}", reason: reason)
    {:stop, {:shutdown, reason}, state |> discard_inbound_unix_fds() |> fail_pending()}
  end

  defp discard_inbound_unix_fds(%__MODULE__{inbound_unix_fds: fds} = state) do
    _ = UnixFD.close_all(fds)
    %{state | inbound_unix_fds: [], inbound_fd_tainted?: false}
  end

  # Each zero-length receive returns data already available through the fixed
  # OTP buffer. Fixed-header validation still happens as soon as 16 bytes are
  # retained, without making allocation depend on a peer-declared frame length.
  @doc false
  @spec append_inbound_fragment(binary(), t(), term()) ::
          {:noreply, t()} | {:noreply, t(), {:continue, term()}} | {:stop, term(), t()}
  def append_inbound_fragment(data, %__MODULE__{} = state, continuation)
      when is_binary(data) do
    append_inbound(data, state, continuation)
  end

  defp append_recvmsg(
         %{iov: iov, ctrl: ctrl, flags: flags},
         %__MODULE__{} = state,
         continuation
       )
       when is_list(ctrl) and is_list(flags) do
    fds = recvmsg_fds(ctrl, flags)

    case recvmsg_data(iov) do
      {:ok, data} ->
        append_recvmsg_fds(fds, data, state, continuation)

      {:error, reason} ->
        # Validate control data before iodata so descriptors cannot leak when
        # the recvmsg shape is invalid. No frame bytes are usable in this case.
        _ = close_recvmsg_fds(fds)
        stop_for_protocol_error(reason, state)
    end
  end

  defp append_recvmsg(%{ctrl: ctrl, flags: flags}, %__MODULE__{} = state, _continuation)
       when is_list(ctrl) and is_list(flags) do
    _ = close_recvmsg_fds(recvmsg_fds(ctrl, flags))
    stop_for_protocol_error(:invalid_unix_fds, state)
  end

  defp append_recvmsg(%{ctrl: ctrl}, %__MODULE__{} = state, _continuation) when is_list(ctrl) do
    _ = close_recvmsg_fds(recvmsg_fds(ctrl, []))
    stop_for_protocol_error(:invalid_unix_fds, state)
  end

  defp append_recvmsg(_message, %__MODULE__{} = state, _continuation),
    do: stop_for_protocol_error(:invalid_unix_fds, state)

  defp append_recvmsg_fds({:ok, []}, data, state, continuation),
    do: append_inbound(data, state, continuation)

  defp append_recvmsg_fds({:ok, fds}, data, state, continuation) do
    cond do
      not state.unix_fd_negotiated? ->
        _ = UnixFD.close_all(fds)
        quarantine_ancillary_frame(data, state, continuation)

      data == <<>> ->
        _ = UnixFD.close_all(fds)
        # A rights-only recvmsg result has no byte offset to associate with a
        # D-Bus frame, so it cannot be recovered without risking later frame
        # ownership.
        stop_for_protocol_error(:invalid_unix_fds, state)

      Inbound.pending?(state.inbound) or state.inbound_unix_fds != [] ->
        _ = UnixFD.close_all(fds)
        quarantine_ancillary_frame(data, state, continuation)

      true ->
        append_inbound(data, %{state | inbound_unix_fds: fds}, continuation)
    end
  end

  defp append_recvmsg_fds({:error, :unix_fd_truncated, fds}, _data, state, _continuation) do
    _ = UnixFD.close_all(fds)
    # MSG_CTRUNC means the kernel may have installed descriptors omitted from
    # the returned control data. Their identities are unknowable, so this
    # cannot be quarantined frame-locally and must fail closed.
    stop_for_protocol_error(:unix_fd_truncated, state)
  end

  defp append_recvmsg_fds({:error, _reason, fds}, data, state, continuation) do
    _ = UnixFD.close_all(fds)
    # We decoded every complete descriptor before finding the malformed or
    # oversized tail. Close them now and drop only the byte-aligned frame.
    quarantine_ancillary_frame(data, state, continuation)
  end

  defp close_recvmsg_fds({:ok, fds}), do: UnixFD.close_all(fds)
  defp close_recvmsg_fds({:error, _reason, fds}), do: UnixFD.close_all(fds)

  defp recvmsg_data(iov) do
    data = IO.iodata_to_binary(iov)

    if byte_size(data) <= @max_read_chunk,
      do: {:ok, data},
      else: {:error, :message_too_large}
  rescue
    ArgumentError -> {:error, :invalid_unix_fds}
  end

  defp recvmsg_fds(ctrl, flags) do
    case {extract_rights_fds(ctrl), :ctrunc in flags} do
      {{:ok, fds}, true} ->
        {:error, :unix_fd_truncated, fds}

      {{:error, _reason, fds}, true} ->
        # Preserve every complete descriptor decoded before the malformed or
        # oversized tail so the single fail-closed path can close it. CTRUNC
        # takes precedence because the kernel may have omitted more descriptors
        # whose identities are unknowable.
        {:error, :unix_fd_truncated, fds}

      {result, _ctrunc?} ->
        result
    end
  end

  defp quarantine_ancillary_frame(<<>>, %__MODULE__{} = state, _continuation) do
    stop_for_protocol_error(:invalid_unix_fds, state)
  end

  defp quarantine_ancillary_frame(data, %__MODULE__{} = state, continuation) do
    append_inbound(data, %{state | inbound_fd_tainted?: true}, continuation)
  end

  defp extract_rights_fds(ctrl) do
    {fds, reason} =
      Enum.reduce(ctrl, {[], nil}, fn
        %{level: :socket, type: :rights, data: data}, {fds, reason} when is_binary(data) ->
          case decode_rights_data(data) do
            {:ok, received} ->
              append_received_fds(fds, received, reason)

            # A malformed control payload can still contain complete
            # descriptors before its invalid tail. Continue scanning later
            # cmsgs too, retaining every descriptor for the single close path.
            {:error, received} ->
              append_received_fds(fds, received, reason || :invalid_unix_fds)
          end

        # An SCM_RIGHTS item with a non-binary payload must fail closed, but
        # later rights cmsgs can still carry descriptors which must be closed.
        %{level: :socket, type: :rights}, {fds, reason} ->
          {fds, reason || :invalid_unix_fds}

        _cmsg, acc ->
          acc
      end)

    case reason do
      nil -> {:ok, fds}
      reason -> {:error, reason, fds}
    end
  end

  defp append_received_fds(fds, received, reason) do
    fds = fds ++ received

    reason =
      reason ||
        if length(fds) > Message.max_unix_fds(), do: :unix_fd_limit

    {fds, reason}
  end

  defp decode_rights_data(data) do
    complete_size = div(byte_size(data), 4) * 4
    <<complete::binary-size(^complete_size), _tail::binary>> = data
    fds = for <<fd::native-signed-32 <- complete>>, do: fd

    cond do
      Enum.any?(fds, &(&1 < 0)) -> {:error, fds}
      complete_size == byte_size(data) -> {:ok, fds}
      true -> {:error, fds}
    end
  end

  defp append_inbound(data, %__MODULE__{} = state, continuation) do
    case Inbound.append(state.inbound, data) do
      {:ok, inbound} ->
        process_inbound(%{state | inbound: inbound}, continuation)

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  defp process_inbound(%__MODULE__{} = state, continuation) do
    case Inbound.next(state.inbound) do
      {:frame, data, inbound} ->
        parse_complete_message(data, %{state | inbound: inbound}, continuation)

      {:incomplete, inbound} ->
        buffer_incomplete_message(%{state | inbound: inbound}, continuation)

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  # A timer exists only while a nonempty frame is incomplete. Each retained
  # fragment replaces it, so a peer that is making progress remains connected
  # while a peer that stops or dribbles too slowly cannot pin retained data.
  defp buffer_incomplete_message(%__MODULE__{} = state, continuation) do
    state =
      if Inbound.pending?(state.inbound) do
        %{state | partial_frame_timer: restart_partial_frame_timer(state)}
      else
        clear_partial_frame(state)
      end

    if is_nil(state.rref) do
      {:noreply, state, {:continue, continuation}}
    else
      {:noreply, state}
    end
  end

  defp clear_partial_frame(%__MODULE__{} = state) do
    %{
      state
      | inbound: Inbound.clear(state.inbound),
        partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)
    }
  end

  defp finish_frame(%__MODULE__{} = state) do
    %{state | partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)}
  end

  defp restart_partial_frame_timer(%__MODULE__{} = state) do
    cancel_partial_frame_timer(state.partial_frame_timer)
    token = make_ref()
    timer_ref = Process.send_after(self(), {:partial_frame_timeout, token}, state.read_timeout)
    {timer_ref, token}
  end

  defp cancel_partial_frame_timer(nil), do: nil

  defp cancel_partial_frame_timer({timer_ref, _token}) do
    _ = Process.cancel_timer(timer_ref)
    nil
  end

  defp hello_reply_result(
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

  defp hello_reply_result(
         %Message{type: :method_return, header_fields: %{reply_serial: hello_serial}},
         hello_serial
       ) do
    {:error, {:hello_failed, :missing_unique_name}}
  end

  defp hello_reply_result(
         %Message{type: :error, header_fields: %{reply_serial: hello_serial}} = msg,
         hello_serial
       ) do
    {:error, {:hello_failed, hello_error_reason(msg.header_fields)}}
  end

  defp hello_reply_result(%Message{type: type}, _hello_serial) do
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
  @spec sanitize_protocol_reason(term()) ::
          :insufficient_data
          | :invalid_endianness
          | :invalid_message
          | :invalid_message_type
          | :invalid_unix_fds
          | :unix_fd_truncated
          | :message_too_large
          | :read_timeout
          | :resource_limit
          | :unsupported_protocol_version
          | :protocol_error
          | {:hello_failed,
             binary()
             | :invalid_error_name
             | :invalid_unique_name
             | :missing_error_name
             | :missing_unique_name
             | :resource_limit}
          | {:malformed_reply, :missing_reply_serial}
          | {:unexpected_handshake_message, Message.message_type()}
  def sanitize_protocol_reason(reason) do
    case reason do
      {:hello_failed, reason}
      when reason in [
             :missing_unique_name,
             :missing_error_name,
             :invalid_error_name,
             :invalid_unique_name,
             :resource_limit
           ] ->
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

      reason
      when reason in [
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
           ] ->
        reason

      _reason ->
        :protocol_error
    end
  end

  # A non-bus connection has no unique name, so nothing can be its own
  # NameAcquired signal. Match the absent name explicitly rather than letting
  # `nil` participate in the header comparison below.
  defp notify(%Message{} = msg, %__MODULE__{name: nil} = state) do
    Rebus.SignalHandler.notify(msg)
    state
  end

  defp notify(%Message{} = msg, %__MODULE__{name: name} = state) do
    case msg do
      %Message{header_fields: %{member: "NameAcquired", destination: ^name}, body: [^name]} ->
        # Ignore our own NameAcquired signals
        :ok

      _ ->
        Rebus.SignalHandler.notify(msg)
    end

    state
  end

  defp reply(%Message{} = msg, %__MODULE__{} = state) do
    case Map.fetch(msg.header_fields, :reply_serial) do
      {:ok, reply_serial} ->
        case Map.pop(state.pending, reply_serial) do
          {nil, _pending} ->
            close_message_fds(msg)
            Logger.info("Ignoring late or orphaned D-Bus reply for serial #{reply_serial}")
            {:ok, state}

          {{from, timer_ref, request_ref, monitor_ref, deadline}, pending} ->
            _ = Process.cancel_timer(timer_ref)

            if msg.unix_fds == [] do
              Process.demonitor(monitor_ref, [:flush])

              if live_from?(from) do
                GenServer.reply(from, msg)
              else
                close_message_fds(msg)
              end

              {:ok, remove_indexes(%{state | pending: pending}, request_ref, monitor_ref)}
            else
              # A live PID alone cannot prove a GenServer.call alias still
              # accepts messages. Hold FD ownership in a claimed state until
              # Connection.call/3 has consumed the regular-process delivery.
              claim_ref = make_ref()

              claim_deadline = fd_claim_deadline(deadline)

              timer_ref =
                Process.send_after(
                  self(),
                  {:fd_claim_timeout, claim_ref},
                  fd_claim_timer_timeout(claim_deadline)
                )

              {pid, _tag} = from

              claim = %{
                pid: pid,
                msg: msg,
                request_ref: request_ref,
                monitor_ref: monitor_ref,
                timer_ref: timer_ref,
                delivery_ref: nil,
                delivery_alias: nil,
                deadline: claim_deadline
              }

              state.impl.hooks.fd_claim_handoff()
              GenServer.reply(from, {:fd_claim, claim_ref})

              {:ok,
               %{
                 state
                 | pending: pending,
                   request_index: Map.delete(state.request_index, request_ref),
                   monitor_index: Map.delete(state.monitor_index, monitor_ref),
                   fd_claims: Map.put(state.fd_claims, claim_ref, claim),
                   fd_claim_request_index:
                     Map.put(state.fd_claim_request_index, request_ref, claim_ref),
                   fd_claim_monitor_index:
                     Map.put(state.fd_claim_monitor_index, monitor_ref, claim_ref)
               }}
            end
        end

      :error ->
        {:error, {:malformed_reply, :missing_reply_serial}}
    end
  end

  defp live_from?({pid, _tag}) when is_pid(pid), do: Process.alive?(pid)
  defp live_from?(_from), do: false

  defp drop_fd_claim(%__MODULE__{} = state, claim_ref, opts) do
    case Map.pop(state.fd_claims, claim_ref) do
      {nil, _claims} ->
        state

      {%{msg: msg, request_ref: request_ref, monitor_ref: monitor_ref, timer_ref: timer_ref},
       claims} ->
        _ = Process.cancel_timer(timer_ref)

        close? = Keyword.get(opts, :close?, false)
        if close?, do: close_message_fds(msg)

        unless Keyword.get(opts, :monitor_down?, false) do
          Process.demonitor(monitor_ref, [:flush])
        end

        state = %{
          state
          | fd_claims: claims,
            fd_claim_request_index: Map.delete(state.fd_claim_request_index, request_ref),
            fd_claim_monitor_index: Map.delete(state.fd_claim_monitor_index, monitor_ref)
        }

        case Keyword.get(opts, :outcome, if(close?, do: :closed, else: nil)) do
          outcome when outcome in [:acknowledged, :closed] ->
            put_fd_claim_outcome(state, claim_ref, outcome)

          _ ->
            state
        end
    end
  end

  defp close_fd_claims(claims) do
    Enum.each(claims, fn {_claim_ref, %{msg: msg, timer_ref: timer_ref}} ->
      _ = Process.cancel_timer(timer_ref)
      close_message_fds(msg)
    end)
  end

  defp rearm_fd_claim(claim_ref, %{timer_ref: timer_ref, deadline: deadline} = claim) do
    _ = Process.cancel_timer(timer_ref)

    %{
      claim
      | timer_ref:
          Process.send_after(
            self(),
            {:fd_claim_timeout, claim_ref},
            fd_claim_timer_timeout(deadline)
          )
    }
  end

  defp fd_claim_deadline(request_deadline), do: request_deadline + @fd_claim_cleanup_grace

  defp fd_claim_live?(%{deadline: deadline}) when is_integer(deadline) do
    deadline > System.monotonic_time(:millisecond)
  end

  defp fd_claim_timer_timeout(deadline) do
    max(0, deadline - System.monotonic_time(:millisecond))
  end

  defp put_fd_claim_outcome(%__MODULE__{} = state, claim_ref, outcome) do
    {old, outcomes} = Map.pop(state.fd_claim_outcomes, claim_ref)

    if old, do: Process.cancel_timer(elem(old, 1))

    timer_ref =
      Process.send_after(self(), {:fd_claim_outcome_timeout, claim_ref}, @fd_claim_cleanup_grace)

    %{state | fd_claim_outcomes: Map.put(outcomes, claim_ref, {outcome, timer_ref})}
  end

  defp take_fd_claim_outcome(%__MODULE__{} = state, claim_ref) do
    case Map.pop(state.fd_claim_outcomes, claim_ref) do
      {nil, _outcomes} ->
        {nil, state}

      {{outcome, timer_ref}, outcomes} ->
        _ = Process.cancel_timer(timer_ref)
        {outcome, %{state | fd_claim_outcomes: outcomes}}
    end
  end

  defp drop_resource_limited_reply(
         %{type: :method_return, reply_serial: reply_serial},
         %__MODULE__{} = state
       )
       when is_integer(reply_serial) and reply_serial > 0 do
    drop_resource_limited_pending(reply_serial, :method_return, state)
  end

  defp drop_resource_limited_reply(
         %{type: :error, reply_serial: reply_serial, error_name: error_name},
         %__MODULE__{} = state
       )
       when is_integer(reply_serial) and reply_serial > 0 and is_binary(error_name) do
    drop_resource_limited_pending(reply_serial, {:error, error_name}, state)
  end

  defp drop_resource_limited_reply(_envelope, %__MODULE__{} = state), do: {:ok, state}

  defp drop_resource_limited_pending(reply_serial, reply_kind, %__MODULE__{} = state) do
    case Map.pop(state.pending, reply_serial) do
      {nil, _pending} ->
        Logger.info("Ignoring late or orphaned D-Bus reply for serial #{reply_serial}")
        {:ok, state}

      {{from, timer_ref, request_ref, monitor_ref, _deadline}, pending} ->
        _ = Process.cancel_timer(timer_ref)
        Process.demonitor(monitor_ref, [:flush])
        GenServer.reply(from, {:error, {:reply_dropped, reply_kind}})
        {:ok, remove_indexes(%{state | pending: pending}, request_ref, monitor_ref)}
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

  defp read_deadline(timeout) when is_integer(timeout) and timeout > 0 do
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

  # Everything the writer borrows from the connection for one call. Building it
  # per call keeps the writer free of connection state, and means substituting
  # an implementation module on a running connection needs no writer update.
  defp writer_context(%__MODULE__{} = state) do
    %{
      sock: state.sock,
      transport: state.impl.transport,
      hooks: state.impl.hooks,
      write_timeout: state.write_timeout,
      pending: state.pending,
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

  defp register_pending(%__MODULE__{} = state, entry) do
    %{
      state
      | pending:
          Map.put(
            state.pending,
            entry.serial,
            {entry.from, entry.timer_ref, entry.request_ref, entry.monitor_ref, entry.deadline}
          ),
        request_index: Map.put(state.request_index, entry.request_ref, entry.serial),
        monitor_index: Map.put(state.monitor_index, entry.monitor_ref, entry.serial)
    }
  end

  defp fail_pending(%__MODULE__{} = state) do
    writer = Writer.abandon_all(state.writer)

    Enum.each(state.pending, fn {_serial, {from, timer_ref, _request_ref, monitor_ref, _deadline}} ->
      _ = Process.cancel_timer(timer_ref)
      Process.demonitor(monitor_ref, [:flush])
      GenServer.reply(from, {:error, :disconnected})
    end)

    close_fd_claims(state.fd_claims)

    Enum.each(state.fd_claims, fn {_claim_ref, %{monitor_ref: monitor_ref}} ->
      Process.demonitor(monitor_ref, [:flush])
    end)

    Enum.each(state.fd_claim_outcomes, fn {_claim_ref, {_outcome, timer_ref}} ->
      _ = Process.cancel_timer(timer_ref)
    end)

    %{
      state
      | pending: %{},
        request_index: %{},
        monitor_index: %{},
        fd_claims: %{},
        fd_claim_request_index: %{},
        fd_claim_monitor_index: %{},
        fd_claim_outcomes: %{},
        writer: writer
    }
  end

  defp remove_indexes(state, request_ref, monitor_ref) do
    %{
      state
      | request_index: Map.delete(state.request_index, request_ref),
        monitor_index: Map.delete(state.monitor_index, monitor_ref)
    }
  end
end
