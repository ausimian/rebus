defmodule Rebus.Connection do
  @moduledoc false
  use GenServer, restart: :temporary
  use TypedStruct

  alias Rebus.Auth
  alias Rebus.MachineId
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.SignalHandler
  alias Rebus.UnixFD
  alias Rebus.WireValue
  require Logger

  @default_write_timeout 5_000
  @default_read_timeout 5_000
  @max_auth_line_size 1_024
  @max_auth_id_output 64
  @max_read_chunk 65_536
  @max_read_attempts 1
  @max_inbound_segments 64
  @max_serial 4_294_967_295
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

  # A peer that floods method calls without reading its socket would otherwise
  # grow the write queue without bound: replies are produced per inbound frame
  # but drain only as fast as the transport accepts them. Beyond this many
  # queued connection-originated replies, further calls go unanswered, exactly
  # as if their reply had expired before it could be written.
  @max_queued_replies 64

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

  @doc false
  @spec start_link(keyword()) :: :ignore | {:error, any()} | {:ok, pid()}
  def start_link(args) do
    case Keyword.get(args, :name) do
      nil -> GenServer.start_link(__MODULE__, args)
      name when is_atom(name) -> GenServer.start_link(__MODULE__, args, name: name)
      _name -> {:error, :invalid_name}
    end
  end

  typedstruct enforce: true do
    field :sock, :socket.socket()
    field :guid, binary() | nil, default: nil
    field :rref, term() | nil, default: nil
    field :inbound_segments, [{pos_integer(), binary()}], default: []
    field :inbound_size, non_neg_integer(), default: 0
    field :inbound_expected_size, pos_integer() | nil, default: nil
    field :inbound_flatten_count, non_neg_integer(), default: 0
    field :inbound_unix_fds, [UnixFD.t()], default: []
    # Ancillary data rejected before a complete D-Bus frame is known belongs
    # to that frame, not to a later coalesced frame. The descriptors themselves
    # are closed immediately; this bit makes the eventual frame a recoverable
    # drop once its byte boundary is available.
    field :inbound_fd_tainted?, boolean(), default: false
    field :name, binary() | nil, default: nil
    field :serial, non_neg_integer(), default: 1
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
    field :auth_id_runner, function() | nil, default: nil
    field :auth_username_runner, function() | nil, default: nil
    field :partial_frame_timer, {reference(), reference()} | nil, default: nil
    field :unix_fd_transport?, boolean(), default: false
    field :unix_fd_negotiated?, boolean(), default: false
    # `nil` until org.freedesktop.DBus.Peer.GetMachineId is first served;
    # `:unavailable` caches a definitive negative lookup.
    field :machine_id, binary() | :unavailable | nil, default: nil
    # Connection-originated replies waiting behind the active write, and
    # whether the cap was hit since the queue last drained below it (so the
    # refusal is logged once per saturation episode, not once per call).
    field :queued_replies, non_neg_integer(), default: 0
    field :reply_queue_saturated?, boolean(), default: false

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

    field :active_write, map() | nil, default: nil
    field :write_queue, :queue.queue(), default: :queue.new()
    field :queued_requests, MapSet.t(reference()), default: MapSet.new()
    field :cancelled_requests, MapSet.t(reference()), default: MapSet.new()
    field :outbound_monitor_index, %{reference() => reference()}, default: %{}
    field :signal_handler_monitor_index, %{reference() => reference()}, default: %{}
    field :signal_handler_ref_index, %{reference() => reference()}, default: %{}
    field :send_fun, function(), default: &:socket.send/4
    field :sendmsg_fun, function(), default: &:socket.sendmsg/4
    field :recvmsg_fun, function(), default: &:socket.recvmsg/5
    field :cancel_fun, function(), default: &:socket.cancel/2
    field :fd_claim_handoff_fun, function() | nil, default: nil
    # Deterministic transition seams used only by the FD lifecycle tests.
    field :fd_claim_delivery_fun, function() | nil, default: nil
    field :fd_claim_ack_fun, function() | nil, default: nil
    # Narrow deterministic-test seam. Production requests retain their public
    # deadline exactly; tests can hold the internal timer long enough to order
    # a caller-side alias timeout before a queued late reply.
    field :request_timeout_slack, non_neg_integer(), default: 0
  end

  @impl true
  def init(args) do
    %{family: family} = addr = Keyword.fetch!(args, :addr)
    write_timeout = Keyword.get(args, :write_timeout, @default_write_timeout)
    timeout = Keyword.get(args, :timeout, @default_read_timeout)
    read_timeout = Keyword.get(args, :read_timeout, @default_read_timeout)

    setup_timeout =
      Keyword.get(args, :address_list_setup_timeout, Keyword.get(args, :read_timeout, timeout))

    aggregate_setup_timeout? = Keyword.has_key?(args, :address_list_setup_timeout)
    expected_guid = Keyword.get(args, :expected_guid)
    precomputed_auth_id = Keyword.get(args, :precomputed_auth_id)
    allow_anonymous? = Keyword.get(args, :allow_anonymous, false)
    bus? = Keyword.get(args, :bus, true)
    name = Keyword.get(args, :name)
    connect_waiter = Keyword.get(args, :connect_waiter)
    auth_id_runner = Keyword.get(args, :auth_id_fun, &run_auth_id/1)
    auth_username_runner = Keyword.get(args, :auth_username_fun, &run_auth_username/1)

    cond do
      not (is_integer(write_timeout) and write_timeout > 0) ->
        {:stop, :invalid_write_timeout}

      not (is_integer(timeout) and timeout > 0) ->
        {:stop, :invalid_timeout}

      not (is_integer(read_timeout) and read_timeout > 0) ->
        {:stop, :invalid_read_timeout}

      not (is_integer(setup_timeout) and setup_timeout > 0) ->
        {:stop, :invalid_setup_timeout}

      not (is_nil(expected_guid) or valid_guid?(expected_guid)) ->
        {:stop, :invalid_expected_guid}

      not (is_nil(precomputed_auth_id) or is_binary(precomputed_auth_id)) ->
        {:stop, :invalid_precomputed_auth_id}

      not is_boolean(allow_anonymous?) ->
        {:stop, :invalid_allow_anonymous}

      not is_boolean(bus?) ->
        {:stop, :invalid_bus_option}

      not (is_nil(name) or is_atom(name)) ->
        {:stop, :invalid_name}

      not is_function(auth_id_runner, 1) ->
        {:stop, :invalid_auth_id_fun}

      not is_function(auth_username_runner, 1) ->
        {:stop, :invalid_auth_username_fun}

      true ->
        # DynamicSupervisor stops children with an exit signal. Trap it so the
        # GenServer loop can return :stop and therefore invoke terminate/2,
        # which closes raw SCM_RIGHTS descriptors retained in partial frames or
        # reply claims. The EXIT clauses below preserve normal link semantics.
        Process.flag(:trap_exit, true)

        case :socket.open(family, :stream, :default) do
          {:ok, sock} ->
            _ = configure_receive_buffer(sock)

            {:ok,
             %__MODULE__{
               sock: sock,
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
               auth_id_runner: auth_id_runner,
               auth_username_runner: auth_username_runner,
               unix_fd_transport?: unix_fd_transport_supported?(family)
             }, {:continue, {:setup, addr}}}

          {:error, reason} ->
            {:stop, normalize_socket_error(reason)}
        end
    end
  end

  @impl true
  def terminate(
        _reason,
        %__MODULE__{
          sock: sock,
          partial_frame_timer: timer_ref,
          inbound_unix_fds: inbound_unix_fds,
          fd_claims: fd_claims
        }
      ) do
    cancel_partial_frame_timer(timer_ref)
    _ = UnixFD.close_all(inbound_unix_fds)
    close_fd_claims(fd_claims)
    _ = :socket.close(sock)
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
        %__MODULE__{sock: s, active_write: %{wait: {:select, continuation, h}}} = state
      ) do
    advance_writes(%{
      state
      | active_write: %{state.active_write | wait: {:continue, continuation}}
    })
  end

  def handle_info(
        {:"$socket", s, :completion, {h, result}},
        %__MODULE__{sock: s, active_write: %{wait: {:completion, _continuation, h}}} = state
      ) do
    handle_completion_result(result, %{state | active_write: %{state.active_write | wait: nil}})
  end

  def handle_info(
        {:"$socket", s, :completion, {h, result}},
        %__MODULE__{sock: s, rref: {:completion, h}} = state
      ) do
    handle_read_completion(result, %{state | rref: nil})
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, rref: h} = state
      ) do
    stop_for_transport_error(reason, state)
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, rref: {:completion, h}} = state
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
        %__MODULE__{active_write: %{request_ref: request_ref} = write} = state
      ) do
    if write.partial? do
      stop_for_transport_error(:timeout, state)
    else
      # No bytes have entered the stream, so this frame can be safely abandoned.
      reply_if_live(write, {:error, :timeout}, state)
      advance_writes(drop_active(state, cancel?: true))
    end
  end

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, active_write: %{wait: {:select, _continuation, h}}} = state
      ),
      do: stop_for_transport_error(reason, state)

  def handle_info(
        {:"$socket", s, :abort, {h, reason}},
        %__MODULE__{sock: s, active_write: %{wait: {:completion, _continuation, h}}} = state
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
        case Map.pop(state.outbound_monitor_index, ref) do
          {nil, _outbound_index} ->
            case Map.fetch(state.fd_claim_monitor_index, ref) do
              {:ok, claim_ref} ->
                {:noreply, drop_fd_claim(state, claim_ref, close?: true, monitor_down?: true)}

              :error ->
                {:noreply, state}
            end

          {request_ref, outbound_monitor_index} ->
            state = %{state | outbound_monitor_index: outbound_monitor_index}
            cancel_outbound_request(state, request_ref)
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
         {:ok, bin} <- Message.encode(%{method | serial: state.serial}) do
      case :socket.send(state.sock, bin, [], state.write_timeout) do
        :ok ->
          {:noreply, %{state | hello_serial: state.serial, serial: next_serial(state.serial)},
           {:continue, :hello_reply_buffer}}

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
  # correlate. The connection is established as soon as BEGIN has been written,
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
        state.recvmsg_fun.(
          state.sock,
          inbound_receive_size(state),
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
        state.recvmsg_fun.(state.sock, 0, @max_unix_fd_control_size, [], :nowait)
        |> handle_receive_result(state)

      true ->
        handle_receive_result(:socket.recv(state.sock, 0, [], :nowait), state)
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

  def handle_receive_result(
        {:completion, {:completion_info, :recv, handle}},
        %__MODULE__{} = state
      ) do
    {:noreply, %{state | rref: {:completion, handle}}}
  end

  def handle_receive_result(
        {:completion, {:completion_info, :recvmsg, handle}},
        %__MODULE__{} = state
      ) do
    {:noreply, %{state | rref: {:completion, handle}}}
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
      case :socket.recv(state.sock, 0, [], timeout) do
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
    case state.recvmsg_fun.(
           state.sock,
           inbound_receive_size(state),
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

  defp handle_read_completion({:ok, data}, %__MODULE__{} = state) when is_binary(data) do
    append_inbound(data, state, :recv)
  end

  defp handle_read_completion({:ok, message}, %__MODULE__{} = state) when is_map(message) do
    append_recvmsg(message, state, :recv)
  end

  defp handle_read_completion({:error, reason}, %__MODULE__{} = state) do
    stop_for_transport_error(reason, state)
  end

  # Completion-based socket backends may use a result shape that differs from
  # the readiness backend. Treat an unknown result as a clean transport stop
  # rather than raising and allowing GenServer to log buffered peer data.
  defp handle_read_completion(_result, %__MODULE__{} = state) do
    stop_for_transport_error(:receive_failed, state)
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
          run_fd_claim_hook(state.fd_claim_delivery_fun)

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
        run_fd_claim_ack_hook(state.fd_claim_ack_fun, claim)

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
            case state.active_write do
              %{request_ref: ^request_ref, partial?: false} ->
                advance_writes(drop_active(state, cancel?: true))

              %{request_ref: ^request_ref} ->
                {:noreply,
                 %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}

              _ ->
                if MapSet.member?(state.queued_requests, request_ref) do
                  {:noreply,
                   %{
                     state
                     | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)
                   }}
                else
                  {:noreply, state}
                end
            end
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

  defp cancel_outbound_request(state, request_ref) do
    case state.active_write do
      %{request_ref: ^request_ref, partial?: false} ->
        advance_writes(drop_active(state, cancel?: true))

      %{request_ref: ^request_ref} ->
        {:noreply,
         %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}

      _ ->
        {:noreply,
         %{state | cancelled_requests: MapSet.put(state.cancelled_requests, request_ref)}}
    end
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
        append_inbound(retain_remainder(data, source), state, continuation)

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

      state.queued_replies >= @max_queued_replies ->
        refuse_saturated_reply(state)

      true ->
        {reply_opts, state} = method_call_reply(msg, state)
        queue_method_call_reply(reply_opts, msg, state)
    end
  end

  # A peer flooding calls into a stalled transport must not also flood the
  # log: warn when the cap is first hit, then stay quiet until the queue has
  # drained below it again.
  defp refuse_saturated_reply(%__MODULE__{reply_queue_saturated?: true} = state), do: state

  defp refuse_saturated_reply(%__MODULE__{} = state) do
    Logger.warning("D-Bus internal reply dropped: :reply_queue_full", reason: :reply_queue_full)
    %{state | reply_queue_saturated?: true}
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
        state =
          queue_write(%{state | queued_replies: state.queued_replies + 1}, %{
            kind: :reply,
            from: nil,
            msg: reply,
            deadline: System.monotonic_time(:millisecond) + state.write_timeout,
            request_ref: make_ref()
          })

        kick_writes(state)

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
         :ok <- connect_socket(sock, addr, connect_timeout),
         {:ok, guid, rest} <- authenticate(state, sock, auth_id, deadline, state.setup_timeout),
         :ok <- verify_expected_guid(guid, state.expected_guid),
         {:ok, unix_fd_negotiated?, rest} <-
           negotiate_unix_fd(state, sock, rest, deadline, state.setup_timeout),
         :ok <-
           handshake_send_with_deadline(sock, "BEGIN \r\n", state, deadline, state.setup_timeout) do
      initialized_connection(%{state | unix_fd_negotiated?: unix_fd_negotiated?}, guid, rest)
    else
      {:error, reason} -> stop_and_close(sock, reason)
    end
  end

  defp initialize(%__MODULE__{} = state, addr) do
    sock = state.sock

    with {:ok, auth_id} <- setup_auth_id(state, state.setup_timeout),
         :ok <- connect_socket(sock, addr, state.setup_timeout),
         deadline = read_deadline(state.setup_timeout),
         {:ok, guid, rest} <- authenticate(state, sock, auth_id, deadline, state.setup_timeout),
         :ok <- verify_expected_guid(guid, state.expected_guid),
         {:ok, unix_fd_negotiated?, rest} <-
           negotiate_unix_fd(state, sock, rest, deadline, state.setup_timeout),
         :ok <-
           handshake_send_with_deadline(sock, "BEGIN \r\n", state, deadline, state.setup_timeout) do
      initialized_connection(%{state | unix_fd_negotiated?: unix_fd_negotiated?}, guid, rest)
    else
      {:error, reason} -> stop_and_close(sock, reason)
    end
  end

  defp initialized_connection(state, guid, rest) do
    {:ok,
     %{
       state
       | guid: guid,
         inbound_segments: if(rest == <<>>, do: [], else: [{byte_size(rest), rest}]),
         inbound_size: byte_size(rest)
     }, {:continue, setup_continuation(state)}}
  end

  defp setup_continuation(%__MODULE__{bus?: false}), do: :established
  defp setup_continuation(%__MODULE__{}), do: :hello

  defp aggregate_setup_auth_id(%__MODULE__{precomputed_auth_id: auth_id}, _deadline)
       when is_binary(auth_id),
       do: {:ok, auth_id}

  defp aggregate_setup_auth_id(%__MODULE__{} = state, deadline) do
    with {:ok, auth_id_timeout} <- remaining_setup_timeout(deadline, state.setup_timeout) do
      get_auth_id(auth_id_timeout, state.auth_id_runner)
    end
  end

  defp setup_auth_id(%__MODULE__{precomputed_auth_id: auth_id}, _timeout) when is_binary(auth_id),
    do: {:ok, auth_id}

  defp setup_auth_id(%__MODULE__{} = state, timeout),
    do: get_auth_id(timeout, state.auth_id_runner)

  # EXTERNAL remains the first authentication mechanism. If it is rejected the
  # advertised list determines a bounded, deterministic retry: cookie first,
  # anonymous only when the caller explicitly enabled it. Each mechanism can be
  # attempted once; later REJECTED lists are parsed for protocol safety but do
  # not alter the original mechanism selection.
  defp authenticate(state, sock, auth_id, deadline, maximum) do
    with :ok <-
           handshake_send_with_deadline(
             sock,
             [0, "AUTH EXTERNAL ", auth_id, "\r\n"],
             state,
             deadline,
             maximum
           ),
         {:ok, line, rest} <- handshake_recv_with_deadline(sock, <<>>, deadline, maximum) do
      case parse_auth_response(line) do
        {:ok, guid} ->
          {:ok, guid, rest}

        {:rejected, mechanisms} ->
          authenticate_rejected(state, sock, auth_id, mechanisms, rest, deadline, maximum)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp authenticate_rejected(state, sock, auth_id, mechanisms, rest, deadline, maximum) do
    cond do
      "DBUS_COOKIE_SHA1" in mechanisms ->
        authenticate_cookie(state, sock, auth_id, mechanisms, rest, deadline, maximum)

      state.allow_anonymous? and "ANONYMOUS" in mechanisms ->
        authenticate_anonymous(state, sock, rest, deadline, maximum)

      true ->
        {:error, {:auth_rejected, mechanisms}}
    end
  end

  defp authenticate_cookie(state, sock, auth_id, mechanisms, rest, deadline, maximum) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum) do
      case get_auth_username(timeout, state.auth_username_runner) do
        {:ok, username} ->
          authenticate_cookie_with_username(
            state,
            sock,
            auth_id,
            mechanisms,
            username,
            rest,
            deadline,
            maximum
          )

        {:error, :auth_cookie_unavailable} ->
          cookie_unavailable_before_auth(state, sock, mechanisms, rest, deadline, maximum)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  # A local username is the initial response for DBUS_COOKIE_SHA1. If it cannot
  # be acquired, no cookie mechanism has started: send ANONYMOUS directly only
  # when the caller opted in and the server advertised it. Once AUTH has been
  # sent, no weaker fallback is permitted.
  defp cookie_unavailable_before_auth(
         %__MODULE__{allow_anonymous?: true} = state,
         sock,
         mechanisms,
         rest,
         deadline,
         maximum
       ) do
    if "ANONYMOUS" in mechanisms,
      do: authenticate_anonymous(state, sock, rest, deadline, maximum),
      else: {:error, :auth_cookie_unavailable}
  end

  defp cookie_unavailable_before_auth(_state, _sock, _mechanisms, _rest, _deadline, _maximum),
    do: {:error, :auth_cookie_unavailable}

  defp authenticate_cookie_with_username(
         state,
         sock,
         auth_id,
         mechanisms,
         username,
         rest,
         deadline,
         maximum
       ) do
    with :ok <-
           handshake_send_with_deadline(
             sock,
             ["AUTH DBUS_COOKIE_SHA1 ", Base.encode16(username, case: :lower), "\r\n"],
             state,
             deadline,
             maximum
           ),
         {:ok, line, rest} <- handshake_recv_with_deadline(sock, rest, deadline, maximum) do
      case line do
        "DATA " <> challenge ->
          authenticate_cookie_data(
            state,
            sock,
            auth_id,
            username,
            challenge,
            mechanisms,
            rest,
            deadline,
            maximum
          )

        "REJECTED" <> _rest ->
          # A mechanism rejection is terminal: do not silently lower the
          # authentication level after starting DBUS_COOKIE_SHA1.
          case parse_auth_response(line) do
            {:rejected, advertised} -> {:error, {:auth_rejected, advertised}}
            {:error, reason} -> {:error, reason}
          end

        _ ->
          {:error, :auth_failed}
      end
    else
      # Once DBUS_COOKIE_SHA1 AUTH is on the wire, even a local credential
      # failure is terminal. A peer must not be able to steer a client toward
      # ANONYMOUS by offering an unavailable context or cookie ID.
      {:error, :auth_cookie_unavailable} -> {:error, :auth_cookie_unavailable}
      {:error, reason} -> {:error, reason}
    end
  end

  defp authenticate_cookie_data(
         state,
         sock,
         auth_id,
         username,
         challenge,
         _mechanisms,
         rest,
         deadline,
         maximum
       ) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum),
         {:ok, uid} <- auth_id_uid(auth_id),
         {:ok, response} <- cookie_response(username, uid, challenge, timeout),
         :ok <-
           handshake_send_with_deadline(
             sock,
             ["DATA ", response, "\r\n"],
             state,
             deadline,
             maximum
           ),
         {:ok, line, rest} <- handshake_recv_with_deadline(sock, rest, deadline, maximum) do
      case parse_auth_response(line) do
        {:ok, guid} -> {:ok, guid, rest}
        # A response that reached the server must not be followed by a weaker
        # mechanism, even when anonymous was explicitly enabled.
        {:rejected, _mechanisms} -> {:error, :auth_failed}
        {:error, reason} -> {:error, reason}
      end
    else
      # A received challenge ties the following credential lookup to
      # DBUS_COOKIE_SHA1. Do not emit CANCEL or attempt ANONYMOUS after a
      # missing/ambiguous cookie, including a peer-chosen context or ID.
      {:error, :auth_cookie_unavailable} -> {:error, :auth_cookie_unavailable}
      {:error, reason} -> {:error, reason}
    end
  end

  defp authenticate_anonymous(state, sock, rest, deadline, maximum) do
    with :ok <-
           handshake_send_with_deadline(sock, "AUTH ANONYMOUS\r\n", state, deadline, maximum),
         {:ok, line, rest} <- handshake_recv_with_deadline(sock, rest, deadline, maximum) do
      case parse_auth_response(line) do
        {:ok, guid} -> {:ok, guid, rest}
        {:rejected, mechanisms} -> {:error, {:auth_rejected, mechanisms}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp parse_auth_response(<<"OK ", guid::binary-size(32)>>) do
    if valid_guid?(guid), do: {:ok, :binary.copy(guid)}, else: {:error, :auth_failed}
  end

  defp parse_auth_response("REJECTED" <> _rest = line) do
    case Auth.parse_rejected(line) do
      {:ok, mechanisms} -> {:rejected, mechanisms}
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_auth_response(_line), do: {:error, :auth_failed}

  defp verify_expected_guid(_guid, nil), do: :ok

  defp verify_expected_guid(guid, expected_guid) do
    if guid_equal?(guid, expected_guid), do: :ok, else: {:error, :guid_mismatch}
  end

  defp valid_guid?(guid) when is_binary(guid) and byte_size(guid) == 32, do: hex_guid?(guid)
  defp valid_guid?(_guid), do: false

  defp hex_guid?(guid), do: all_bytes?(guid, &hex_byte?/1)

  defp hex_byte?(byte) when byte in ?0..?9 or byte in ?a..?f or byte in ?A..?F, do: true
  defp hex_byte?(_byte), do: false

  defp guid_equal?(<<>>, <<>>), do: true

  defp guid_equal?(<<left, left_rest::binary>>, <<right, right_rest::binary>>) do
    ascii_lower(left) == ascii_lower(right) and guid_equal?(left_rest, right_rest)
  end

  defp guid_equal?(_left, _right), do: false

  defp ascii_lower(byte) when byte in ?A..?Z, do: byte + 32
  defp ascii_lower(byte), do: byte

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

  @doc false
  @spec get_auth_id(pos_integer(), (pos_integer() -> {:ok, binary()} | {:error, term()})) ::
          {:ok, binary()} | {:error, :auth_id_unavailable | :read_timeout}
  def get_auth_id(timeout, runner \\ &run_auth_id/1) when is_integer(timeout) and timeout > 0 do
    case safely_run_auth_id(runner, timeout) do
      {:ok, output} when is_binary(output) and byte_size(output) <= @max_auth_id_output ->
        case String.trim(output) do
          uid when uid != <<>> ->
            if uid_bytes?(uid),
              do: {:ok, :binary.encode_hex(uid)},
              else: {:error, :auth_id_unavailable}

          _ ->
            {:error, :auth_id_unavailable}
        end

      {:error, :timeout} ->
        {:error, :read_timeout}

      _ ->
        {:error, :auth_id_unavailable}
    end
  end

  @doc false
  @spec get_auth_username(pos_integer(), (pos_integer() -> {:ok, binary()} | {:error, term()})) ::
          {:ok, binary()} | {:error, :auth_cookie_unavailable | :read_timeout}
  def get_auth_username(timeout, runner \\ &run_auth_username/1)
      when is_integer(timeout) and timeout > 0 do
    case safely_run_auth_id(runner, timeout) do
      {:ok, output} when is_binary(output) and byte_size(output) <= @max_auth_id_output ->
        username = String.trim(output)

        if valid_auth_username?(username),
          do: {:ok, :binary.copy(username)},
          else: {:error, :auth_cookie_unavailable}

      {:error, :timeout} ->
        {:error, :read_timeout}

      _ ->
        {:error, :auth_cookie_unavailable}
    end
  end

  @doc false
  @spec run_auth_username(
          pos_integer(),
          (String.t() -> String.t() | nil),
          ({:spawn_executable, charlist()}, keyword() -> port())
        ) :: {:ok, binary()} | {:error, term()}
  def run_auth_username(
        timeout,
        executable_finder \\ &System.find_executable/1,
        port_opener \\ &Port.open/2
      )
      when is_integer(timeout) and timeout > 0 and is_function(executable_finder, 1) and
             is_function(port_opener, 2) do
    case safely_find_executable(executable_finder) do
      nil -> {:error, :enoent}
      executable -> safely_open_auth_username_port(executable, port_opener, timeout)
    end
  end

  defp safely_open_auth_username_port(executable, port_opener, timeout) do
    port =
      port_opener.({:spawn_executable, String.to_charlist(executable)}, [
        :binary,
        :exit_status,
        args: ["-un"]
      ])

    collect_auth_id_output(port, <<>>, read_deadline(timeout), timeout)
  rescue
    _exception -> {:error, :port_open_failed}
  catch
    _kind, _reason -> {:error, :port_open_failed}
  end

  defp valid_auth_username?(username) when byte_size(username) in 1..64,
    do: all_bytes?(username, &visible_ascii_byte?/1)

  defp valid_auth_username?(_username), do: false

  defp visible_ascii_byte?(byte), do: byte in 0x21..0x7E

  defp auth_id_uid(auth_id) when is_binary(auth_id) do
    with {:ok, uid_bytes} <- Base.decode16(auth_id, case: :mixed),
         {uid, <<>>} <- Integer.parse(uid_bytes),
         true <- uid >= 0 and uid <= 4_294_967_295 do
      {:ok, uid}
    else
      _ -> {:error, :auth_failed}
    end
  end

  # File metadata and reads are local but can still block on a hostile mount.
  # Keep the whole credential operation inside the same setup deadline without
  # retaining either the cookie or server challenge in Connection state.
  defp cookie_response(username, uid, challenge, timeout) do
    ref = make_ref()
    delivery_alias = :erlang.alias([:reply])

    pid =
      spawn_link(fn ->
        send(delivery_alias, {ref, safe_cookie_response(username, uid, challenge)})
      end)

    monitor_ref = Process.monitor(pid)

    await_cookie_response(pid, ref, delivery_alias, monitor_ref, timeout)
  end

  defp safe_cookie_response(username, uid, challenge) do
    Auth.cookie_response(username, uid, challenge)
  rescue
    _exception -> {:error, :auth_cookie_unavailable}
  catch
    _kind, _reason -> {:error, :auth_cookie_unavailable}
  end

  defp await_cookie_response(pid, ref, delivery_alias, monitor_ref, timeout) do
    receive do
      {^ref, result} ->
        result

      {:DOWN, ^monitor_ref, :process, ^pid, _reason} ->
        {:error, :auth_cookie_unavailable}
    after
      timeout ->
        Process.unlink(pid)
        :erlang.unalias(delivery_alias)
        Process.exit(pid, :kill)
        {:error, :read_timeout}
    end
  after
    # The one-shot alias rejects a late worker result atomically. Drain a
    # response queued before unaliasing so a derived digest cannot linger in
    # this GenServer's mailbox after the bounded credential operation ends.
    :erlang.unalias(delivery_alias)
    drain_cookie_response_delivery(ref)
    Process.demonitor(monitor_ref, [:flush])
  end

  defp drain_cookie_response_delivery(ref) do
    receive do
      {^ref, _result} -> :ok
    after
      0 -> :ok
    end
  end

  @doc false
  @spec run_auth_id(
          pos_integer(),
          (String.t() -> String.t() | nil),
          ({:spawn_executable, charlist()}, keyword() -> port())
        ) :: {:ok, binary()} | {:error, term()}
  def run_auth_id(
        timeout,
        executable_finder \\ &System.find_executable/1,
        port_opener \\ &Port.open/2
      )
      when is_integer(timeout) and timeout > 0 and is_function(executable_finder, 1) and
             is_function(port_opener, 2) do
    case safely_find_executable(executable_finder) do
      nil ->
        {:error, :enoent}

      executable ->
        safely_open_auth_id_port(executable, port_opener, timeout)
    end
  end

  defp safely_run_auth_id(runner, timeout) do
    runner.(timeout)
  rescue
    _exception -> {:error, :runner_failed}
  catch
    _kind, _reason -> {:error, :runner_failed}
  end

  defp safely_find_executable(executable_finder) do
    executable_finder.("id")
  rescue
    _exception -> nil
  catch
    _kind, _reason -> nil
  end

  defp safely_open_auth_id_port(executable, port_opener, timeout) do
    port =
      port_opener.({:spawn_executable, String.to_charlist(executable)}, [
        :binary,
        :exit_status,
        args: ["-u"]
      ])

    collect_auth_id_output(port, <<>>, read_deadline(timeout), timeout)
  rescue
    _exception -> {:error, :port_open_failed}
  catch
    _kind, _reason -> {:error, :port_open_failed}
  end

  defp collect_auth_id_output(port, output, deadline, maximum) do
    case remaining_timeout(deadline, maximum) do
      :expired ->
        safe_close_port(port)
        {:error, :timeout}

      {:ok, timeout} ->
        receive do
          {^port, {:data, data}}
          when is_binary(data) and byte_size(output) + byte_size(data) <= @max_auth_id_output ->
            collect_auth_id_output(port, output <> data, deadline, maximum)

          {^port, {:data, _data}} ->
            safe_close_port(port)
            {:error, :output_too_large}

          {^port, {:exit_status, 0}} ->
            {:ok, output}

          {^port, {:exit_status, _status}} ->
            {:error, :exit_status}

          {:EXIT, ^port, _reason} ->
            {:error, :port_exit}
        after
          timeout ->
            safe_close_port(port)
            {:error, :timeout}
        end
    end
  end

  defp safe_close_port(port) do
    Port.close(port)
  catch
    _kind, _reason -> :ok
  end

  defp uid_bytes?(uid), do: all_bytes?(uid, &digit_byte?/1)

  defp digit_byte?(byte), do: byte in ?0..?9

  # Walk the binary directly: no intermediate list, and the first byte that
  # fails the predicate ends the walk.
  defp all_bytes?(<<>>, _predicate), do: true

  defp all_bytes?(<<byte, rest::binary>>, predicate),
    do: predicate.(byte) and all_bytes?(rest, predicate)

  defp stop_and_close(sock, reason) do
    _ = :socket.close(sock)
    {:stop, normalize_socket_error(reason)}
  end

  defp connect_socket(sock, addr, timeout) do
    case :socket.connect(sock, addr, timeout) do
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
  def configure_receive_buffer(
        sock,
        setopt_fun \\ &:socket.setopt/3,
        warning_fun \\ fn message -> Logger.warning(message) end
      ) do
    # A zero-length receive returns the bytes currently available on every
    # supported OTP release. Keep the backing allocation independent of a
    # peer-declared D-Bus frame length. Some backends only accept the scalar
    # form, so failure to tune this hint must never make connections unavailable.
    case setopt_fun.(sock, {:otp, :rcvbuf}, {@max_read_attempts, @max_read_chunk}) do
      :ok ->
        :tuple

      {:error, _reason} ->
        case setopt_fun.(sock, {:otp, :rcvbuf}, @max_read_chunk) do
          :ok ->
            :scalar

          {:error, _reason} ->
            default_receive_buffer(warning_fun)

          _other ->
            default_receive_buffer(warning_fun)
        end

      _other ->
        default_receive_buffer(warning_fun)
    end
  end

  defp default_receive_buffer(warning_fun) do
    warning_fun.("D-Bus connection is using OTP's default receive buffer")
    :default
  end

  defp handshake_recv(sock, buffer, timeout) when is_binary(buffer) do
    receive_auth_line(sock, buffer, read_deadline(timeout), timeout)
  end

  defp handshake_recv_with_deadline(sock, buffer, deadline, maximum) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum) do
      handshake_recv(sock, buffer, timeout)
    end
  end

  defp handshake_send_with_deadline(sock, data, state, deadline, maximum) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum) do
      handshake_send(sock, data, min(timeout, state.write_timeout))
    end
  end

  # Unix-FD negotiation is an optional authentication extension. A peer's
  # ERROR leaves the ordinary D-Bus connection usable, but FD-bearing messages
  # will be rejected before any bytes are sent. We only issue it on local Unix
  # stream sockets where SCM_RIGHTS is available to OTP.
  defp negotiate_unix_fd(
         %__MODULE__{unix_fd_transport?: false},
         _sock,
         rest,
         _deadline,
         _maximum
       ),
       do: {:ok, false, rest}

  defp negotiate_unix_fd(%__MODULE__{} = state, sock, rest, deadline, maximum) do
    with :ok <-
           handshake_send_with_deadline(
             sock,
             "NEGOTIATE_UNIX_FD\r\n",
             state,
             deadline,
             maximum
           ),
         {:ok, line, rest} <- handshake_recv_with_deadline(sock, rest, deadline, maximum) do
      case line do
        "AGREE_UNIX_FD" -> {:ok, true, rest}
        "ERROR" <> _reason -> {:ok, false, rest}
        _ -> {:error, :auth_failed}
      end
    end
  end

  defp receive_auth_line(sock, buffer, deadline, timeout) do
    # Previous reads can contain multiple auth lines. Consume one already in
    # the bounded buffer before touching the socket: the peer may legitimately
    # have closed after coalescing its next response.
    case consume_auth_buffer(buffer) do
      {:ok, _line, _rest} = result ->
        result

      :incomplete ->
        receive_auth_socket_data(sock, buffer, deadline, timeout)

      {:error, :auth_failed} = error ->
        error
    end
  end

  defp receive_auth_socket_data(sock, buffer, deadline, timeout) do
    case remaining_timeout(deadline, timeout) do
      :expired ->
        {:error, :read_timeout}

      {:ok, receive_timeout} ->
        case :socket.recv(sock, 0, [], receive_timeout) do
          {:ok, data} ->
            consume_auth_data(sock, buffer, data, deadline, timeout)

          {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
            consume_auth_data(sock, buffer, data, deadline, timeout)

          {:error, :timeout} ->
            {:error, :read_timeout}

          {:error, {:timeout, _data}} ->
            {:error, :read_timeout}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp consume_auth_data(sock, buffer, data, deadline, timeout) do
    case consume_auth_buffer(buffer <> data) do
      {:ok, _line, _rest} = result -> result
      {:error, :auth_failed} = error -> error
      :incomplete -> receive_auth_line(sock, buffer <> data, deadline, timeout)
    end
  end

  defp consume_auth_buffer(buffer) do
    case :binary.match(buffer, "\r\n") do
      {line_size, 2} when line_size <= @max_auth_line_size ->
        line = binary_part(buffer, 0, line_size)
        rest_size = byte_size(buffer) - line_size - 2
        rest = binary_part(buffer, line_size + 2, rest_size)
        {:ok, line, rest}

      {_, 2} ->
        {:error, :auth_failed}

      :nomatch when byte_size(buffer) > @max_auth_line_size ->
        {:error, :auth_failed}

      :nomatch ->
        :incomplete
    end
  end

  defp handshake_send(sock, data, timeout) do
    case :socket.send(sock, data, [], timeout) do
      :ok -> :ok
      {:error, reason} -> {:error, normalize_socket_error(reason)}
      _other -> {:error, :send_failed}
    end
  end

  @doc false
  @spec normalize_socket_error(term()) :: term()
  def normalize_socket_error({:auth_rejected, _mechanisms} = error), do: error

  def normalize_socket_error({reason, partial} = error) when is_atom(reason) do
    if is_binary(partial) or iolist?(partial), do: reason, else: error
  end

  def normalize_socket_error(reason), do: reason

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

      state.inbound_size != 0 or state.inbound_unix_fds != [] ->
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

  defp inbound_receive_size(%__MODULE__{inbound_expected_size: nil, inbound_size: inbound_size}) do
    max(1, min(16 - inbound_size, @max_read_chunk))
  end

  defp inbound_receive_size(%__MODULE__{
         inbound_expected_size: expected,
         inbound_size: inbound_size
       }) do
    max(1, min(expected - inbound_size, @max_read_chunk))
  end

  defp append_inbound(<<>>, %__MODULE__{} = state, continuation),
    do: process_inbound(state, continuation)

  defp append_inbound(data, %__MODULE__{} = state, continuation) do
    segments = append_segment(data, state.inbound_segments)

    if length(segments) <= @max_inbound_segments do
      state = %{
        state
        | inbound_segments: segments,
          inbound_size: state.inbound_size + byte_size(data)
      }

      process_inbound(state, continuation)
    else
      # Segment metadata is part of the retained inbound budget. A peer that
      # defeats rope merging with pathological fragment sizes is rejected
      # before its BEAM-term overhead becomes unbounded.
      stop_for_protocol_error(:message_too_large, state)
    end
  end

  defp process_inbound(%__MODULE__{inbound_size: 0} = state, continuation),
    do: buffer_incomplete_message(state, continuation)

  defp process_inbound(%__MODULE__{inbound_expected_size: nil} = state, continuation) do
    case Message.expected_size(inbound_prefix(state, min(state.inbound_size, 16))) do
      {:ok, expected_size} ->
        process_inbound(%{state | inbound_expected_size: expected_size}, continuation)

      nil ->
        buffer_incomplete_message(state, continuation)

      {:error, reason} ->
        stop_for_protocol_error(reason, state)
    end
  end

  defp process_inbound(%__MODULE__{} = state, continuation) do
    if state.inbound_size >= state.inbound_expected_size do
      data = inbound_binary(state)

      parse_complete_message(
        data,
        clear_inbound_frame(%{state | inbound_flatten_count: state.inbound_flatten_count + 1}),
        continuation
      )
    else
      buffer_incomplete_message(state, continuation)
    end
  end

  @doc false
  @spec inbound_receive_buffer_size() :: pos_integer()
  def inbound_receive_buffer_size, do: @max_read_chunk

  defp inbound_prefix(%__MODULE__{} = state, size) do
    state.inbound_segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> take_prefix(size, [])
    |> IO.iodata_to_binary()
  end

  defp take_prefix(_segments, 0, acc), do: Enum.reverse(acc)
  defp take_prefix([], _size, acc), do: Enum.reverse(acc)

  defp take_prefix([segment | segments], size, acc) when byte_size(segment) <= size do
    take_prefix(segments, size - byte_size(segment), [segment | acc])
  end

  defp take_prefix([segment | _segments], size, acc) do
    Enum.reverse([binary_part(segment, 0, size) | acc])
  end

  defp inbound_binary(%__MODULE__{} = state) do
    state.inbound_segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> IO.iodata_to_binary()
  end

  # Segments are newest first. Merging a segment only with smaller or equal
  # predecessors keeps common small-fragment traffic logarithmic while
  # preserving byte order. The explicit segment limit protects pathological
  # decreasing fragment sizes without flattening an ever-growing buffer.
  defp append_segment(data, segments) do
    merge_segment(byte_size(data), data, segments)
  end

  defp merge_segment(size, data, [{previous_size, previous} | segments])
       when previous_size <= size do
    merge_segment(previous_size + size, previous <> data, segments)
  end

  defp merge_segment(size, data, segments), do: [{size, data} | segments]

  defp retain_remainder(remainder, source) do
    if byte_size(remainder) * 4 < byte_size(source) do
      :binary.copy(remainder)
    else
      remainder
    end
  end

  defp clear_inbound_frame(%__MODULE__{} = state),
    do: %{state | inbound_segments: [], inbound_size: 0, inbound_expected_size: nil}

  # A timer exists only while a nonempty frame is incomplete. Each retained
  # fragment replaces it, so a peer that is making progress remains connected
  # while a peer that stops or dribbles too slowly cannot pin retained data.
  defp buffer_incomplete_message(%__MODULE__{inbound_size: 0, rref: rref} = state, _continuation)
       when not is_nil(rref) do
    {:noreply, clear_partial_frame(state)}
  end

  defp buffer_incomplete_message(%__MODULE__{inbound_size: 0} = state, continuation) do
    {:noreply, clear_partial_frame(state), {:continue, continuation}}
  end

  defp buffer_incomplete_message(%__MODULE__{rref: rref} = state, _continuation)
       when not is_nil(rref) do
    timer_ref = restart_partial_frame_timer(state)
    {:noreply, %{state | partial_frame_timer: timer_ref}}
  end

  defp buffer_incomplete_message(%__MODULE__{} = state, continuation) do
    timer_ref = restart_partial_frame_timer(state)
    {:noreply, %{state | partial_frame_timer: timer_ref}, {:continue, continuation}}
  end

  defp clear_partial_frame(%__MODULE__{} = state) do
    %{
      state
      | inbound_segments: [],
        inbound_size: 0,
        inbound_expected_size: nil,
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

  defp iolist?(data) do
    _ = IO.iodata_to_binary(data)
    true
  rescue
    ArgumentError -> false
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

              run_fd_claim_handoff_hook(state.fd_claim_handoff_fun)
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

  defp run_fd_claim_handoff_hook(nil), do: :ok
  defp run_fd_claim_handoff_hook(fun) when is_function(fun, 0), do: fun.()

  defp run_fd_claim_hook(nil), do: :ok
  defp run_fd_claim_hook(fun) when is_function(fun, 0), do: fun.()

  defp run_fd_claim_ack_hook(nil, _claim), do: :ok
  defp run_fd_claim_ack_hook(fun, _claim) when is_function(fun, 0), do: fun.()
  defp run_fd_claim_ack_hook(fun, claim) when is_function(fun, 1), do: fun.(claim)

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

  defp encode_message(%Message{} = msg) do
    case Message.encode(msg) do
      {:ok, bin} ->
        {:ok, bin}

      {:error, reason}
      when reason in [
             :invalid_body,
             :invalid_header_fields,
             :invalid_message,
             :message_too_large
           ] ->
        Logger.warning("D-Bus message encoding failed: #{inspect(reason)}", reason: reason)
        {:error, :encode_failed}

      {:error, _reason} ->
        Logger.warning("D-Bus message encoding failed: :invalid_message",
          reason: :invalid_message
        )

        {:error, :encode_failed}
    end
  rescue
    exception ->
      Logger.warning("D-Bus message encoding failed: #{inspect(exception.__struct__)}",
        reason: exception.__struct__
      )

      {:error, :encode_failed}
  catch
    kind, _reason ->
      Logger.warning("D-Bus message encoding failed: #{inspect(kind)}", reason: kind)
      {:error, :encode_failed}
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

  defp remaining_timeout(deadline) when is_integer(deadline) do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, remaining}
      _ -> {:error, :timeout}
    end
  end

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

  @doc false
  defguardp is_select_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :select_info and
                   elem(info, 1) in [:send, :sendmsg] and is_reference(elem(info, 2))

  defguardp is_sendmsg_select_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :select_info and
                   elem(info, 1) == :sendmsg and is_reference(elem(info, 2))

  defguardp is_completion_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :completion_info and
                   elem(info, 1) in [:send, :sendmsg] and is_reference(elem(info, 2))

  defguardp is_sendmsg_completion_info(info)
            when is_tuple(info) and tuple_size(info) == 3 and elem(info, 0) == :completion_info and
                   elem(info, 1) == :sendmsg and is_reference(elem(info, 2))

  @spec classify_send_result(term(), non_neg_integer()) ::
          :ok
          | {:continue, iodata()}
          | {:select, tuple(), binary() | nil}
          | {:completion, term()}
          | {:error, term()}
  def classify_send_result(:ok, _payload_length), do: :ok

  def classify_send_result({:ok, rest}, _payload_length) when is_binary(rest),
    do: {:continue, rest}

  def classify_send_result({:select, {select_info, rest}}, _payload_length)
      when is_select_info(select_info) and is_binary(rest),
      do: {:select, select_info, rest}

  def classify_send_result({:select, select_info}, _payload_length)
      when is_select_info(select_info),
      do: {:select, select_info, nil}

  def classify_send_result({:completion, completion_info}, _payload_length)
      when is_completion_info(completion_info),
      do: {:completion, completion_info}

  def classify_send_result({:error, {:timeout, rest}}, payload_length) do
    if iolist?(rest) and IO.iodata_length(rest) == payload_length,
      do: {:error, :timeout},
      else: {:error, {:send_fatal, :timeout}}
  end

  def classify_send_result({:error, {reason, _rest}}, _payload_length) when is_atom(reason),
    do: {:error, {:send_fatal, reason}}

  def classify_send_result({:error, reason}, _payload_length) when is_atom(reason),
    do: {:error, {:send_fatal, reason}}

  def classify_send_result({:error, _reason}, _payload_length),
    do: {:error, {:send_fatal, :send_failed}}

  def classify_send_result(_result, _payload_length), do: {:error, {:send_fatal, :send_failed}}

  defp classify_sendmsg_result(result, payload_length) do
    case result do
      {:ok, rest} ->
        case send_rest_binary(rest) do
          {:ok, rest} -> {:continue, rest}
          _ -> {:error, {:send_fatal, :send_failed}}
        end

      {:select, {select_info, rest}} when is_sendmsg_select_info(select_info) ->
        case send_rest_binary(rest) do
          {:ok, rest} -> {:select, select_info, rest}
          _ -> {:error, {:send_fatal, :send_failed}}
        end

      {:select, select_info} when is_sendmsg_select_info(select_info) ->
        {:select, select_info, nil}

      {:completion, completion_info} when is_sendmsg_completion_info(completion_info) ->
        {:completion, completion_info}

      {:select, _unexpected} ->
        {:error, {:send_fatal, :send_failed}}

      {:completion, _unexpected} ->
        {:error, {:send_fatal, :send_failed}}

      {:error, reason} when reason in [:ebadf, :einval, :eperm, :emfile, :enfile] ->
        # A descriptor-local failure before this attempt accepted bytes is not
        # a stream failure. The queued caller receives a bounded error and the
        # connection can continue with independent calls.
        {:error, :unix_fd_send_failed}

      {:error, {reason, rest}} when reason in [:ebadf, :einval, :eperm, :emfile, :enfile] ->
        if iolist?(rest) and IO.iodata_length(rest) == payload_length,
          do: {:error, :unix_fd_send_failed},
          else: {:error, {:send_fatal, reason}}

      other ->
        classify_send_result(other, payload_length)
    end
  end

  defp send_rest_binary(rest) do
    {:ok, IO.iodata_to_binary(rest)}
  rescue
    ArgumentError -> :error
  end

  # Writes are one-frame-at-a-time.  OTP retains the unaccepted RestData in every
  # partial result; retaining it here is what preserves D-Bus stream framing.
  defp enqueue_write(state, operation), do: advance_writes(queue_write(state, operation))

  # Connection-originated frames (`kind: :reply`) have no caller: no `from` to
  # reply to, no monitor to release, and no cancellation. They share the FIFO
  # write queue so a reply can never overtake or starve caller traffic.
  defp queue_write(state, operation) do
    monitor_ref = monitor_operation(operation)
    operation = Map.put(operation, :monitor_ref, monitor_ref)

    %{
      state
      | write_queue: :queue.in(operation, state.write_queue),
        queued_requests: MapSet.put(state.queued_requests, operation.request_ref),
        outbound_monitor_index:
          index_outbound_monitor(state.outbound_monitor_index, monitor_ref, operation.request_ref)
    }
  end

  defp release_reply_slot(%__MODULE__{} = state, %{kind: :reply}),
    do: %{state | queued_replies: state.queued_replies - 1, reply_queue_saturated?: false}

  defp release_reply_slot(%__MODULE__{} = state, _operation), do: state

  defp monitor_operation(%{from: nil}), do: nil
  defp monitor_operation(%{from: from}), do: Process.monitor(elem(from, 0))

  defp index_outbound_monitor(index, nil, _request_ref), do: index

  defp index_outbound_monitor(index, monitor_ref, request_ref),
    do: Map.put(index, monitor_ref, request_ref)

  defp advance_writes(%__MODULE__{active_write: nil} = state) do
    case :queue.out(state.write_queue) do
      {:empty, _} ->
        {:noreply, state}

      {{:value, operation}, queue} ->
        state = %{
          release_reply_slot(state, operation)
          | write_queue: queue,
            queued_requests: MapSet.delete(state.queued_requests, operation.request_ref)
        }

        if cancelled_or_expired?(operation, state) do
          state = release_outbound_monitor(state, operation)

          advance_writes(%{
            state
            | cancelled_requests: MapSet.delete(state.cancelled_requests, operation.request_ref)
          })
        else
          case validate_outbound_fd_transport(operation.msg, state) do
            :ok ->
              case allocate_serial(state.serial, state.pending) do
                {:ok, serial} ->
                  case encode_message(%{operation.msg | serial: serial}) do
                    {:ok, bin} ->
                      bin = IO.iodata_to_binary(bin)

                      timer_ref =
                        Process.send_after(
                          self(),
                          {:write_timeout, operation.request_ref},
                          state.write_timeout
                        )

                      write =
                        Map.merge(operation, %{
                          serial: serial,
                          rest: bin,
                          frame_size: byte_size(bin),
                          wait: nil,
                          timer_ref: timer_ref,
                          partial?: false,
                          unix_fds: operation.msg.unix_fds,
                          uses_sendmsg?: operation.msg.unix_fds != [],
                          # `:socket.sendmsg/4` retains the original encoded
                          # control map in a select continuation. We keep this
                          # explicit so that only a no-progress select uses
                          # that continuation; once bytes have been accepted,
                          # the remaining stream bytes use plain send/4.
                          fd_control: if(operation.msg.unix_fds == [], do: :none, else: :initial)
                        })

                      advance_writes(%{state | active_write: write})

                    {:error, reason} ->
                      advance_writes(fail_operation(state, operation, reason))
                  end

                {:error, reason} ->
                  advance_writes(fail_operation(state, operation, reason))
              end

            {:error, reason} ->
              advance_writes(fail_operation(state, operation, reason))
          end
        end
    end
  end

  defp advance_writes(%__MODULE__{active_write: %{wait: {:select, _, _}}} = state),
    do: {:noreply, state}

  defp advance_writes(%__MODULE__{active_write: %{wait: {:completion, _, _}}} = state),
    do: {:noreply, state}

  defp advance_writes(%__MODULE__{active_write: write} = state) do
    if (expired?(write) or cancelled?(write, state)) and not write.partial? do
      advance_writes(drop_active(state, cancel?: true))
    else
      result = safe_socket_send(state, write)
      handle_write_result(result, %{state | active_write: %{write | wait: nil}})
    end
  end

  # A connection-originated reply has no caller to inform. Failing to encode,
  # serialize or transport one is a defect in this library rather than a caller
  # error, so it is logged and the frame is dropped; the connection continues.
  defp fail_operation(state, %{kind: :reply} = operation, reason) do
    Logger.warning("D-Bus internal reply dropped: #{inspect(reason)}", reason: reason)
    release_outbound_monitor(state, operation)
  end

  defp fail_operation(state, operation, reason) do
    state = release_outbound_monitor(state, operation)
    GenServer.reply(operation.from, {:error, reason})
    state
  end

  defp handle_write_result(result, %__MODULE__{active_write: write} = state) do
    case classify_write_result(result, write) do
      :ok ->
        complete_active_write(state)

      {:continue, rest} ->
        state = put_active_rest(state, rest)
        {:noreply, state, {:continue, :write}}

      {:select, continuation, rest} ->
        partial_with_rights? = fd_control_accepted?(write, rest)
        state = if rest, do: put_active_rest(state, rest), else: state
        {:select_info, _operation, handle} = continuation

        if partial_with_rights? do
          # OTP's Cont keeps the original encoded Msg (including ctrl); using
          # it after a byte was sent could emit SCM_RIGHTS again. Cancel the
          # pending select and let plain send/4 register its own continuation.
          cancel_socket_write(state, {:select, continuation, handle})
          {:noreply, state, {:continue, :write}}
        else
          # `:accepted` is sticky. A positive-progress sendmsg has already
          # transferred SCM_RIGHTS and its tail is now a plain send/4
          # operation. A later plain-send select must never turn it back into
          # a sendmsg continuation (whose OTP continuation still owns ctrl).
          state =
            if write.uses_sendmsg? and
                 Map.get(state.active_write, :fd_control) in [:initial, :select_continuation],
               do: %{
                 state
                 | active_write: %{state.active_write | fd_control: :select_continuation}
               },
               else: state

          {:noreply,
           %{state | active_write: %{state.active_write | wait: {:select, continuation, handle}}}}
        end

      {:completion, {:completion_info, _operation, notification_handle} = handle} ->
        {:noreply,
         %{
           state
           | active_write: %{
               state.active_write
               | wait: {:completion, handle, notification_handle}
             }
         }}

      {:error, {:send_fatal, reason}} ->
        stop_for_transport_error(reason, state)

      {:error, reason} ->
        if write.partial? do
          stop_for_transport_error(reason, state)
        else
          reply_if_live(write, {:error, reason}, state)
          advance_writes(drop_active(state, cancel?: true))
        end
    end
  end

  defp classify_write_result(
         result,
         %{uses_sendmsg?: true, fd_control: control, rest: rest}
       )
       when control in [:initial, :select_continuation],
       do: classify_sendmsg_result(result, byte_size(rest))

  defp classify_write_result(result, %{rest: rest}),
    do: classify_send_result(result, byte_size(rest))

  defp handle_completion_result(:ok, state), do: complete_active_write(state)

  defp handle_completion_result({:ok, written}, %__MODULE__{active_write: write} = state)
       when is_integer(written) and written > 0 and written < byte_size(write.rest) do
    <<_sent::binary-size(^written), rest::binary>> = write.rest
    state = put_active_rest(state, rest)
    {:noreply, state, {:continue, :write}}
  end

  defp handle_completion_result({:error, reason}, state),
    do: stop_for_transport_error(reason, state)

  defp handle_completion_result(_unexpected, state),
    do: stop_for_transport_error(:send_failed, state)

  defp put_active_rest(%__MODULE__{active_write: write} = state, rest) do
    partial? = write.partial? or byte_size(rest) < byte_size(write.rest)

    fd_control =
      if fd_control_accepted?(write, rest),
        do: :accepted,
        else: Map.get(write, :fd_control, :none)

    %{state | active_write: %{write | rest: rest, partial?: partial?, fd_control: fd_control}}
  end

  defp fd_control_accepted?(%{uses_sendmsg?: true, fd_control: control, rest: previous}, rest)
       when control in [:initial, :select_continuation] and is_binary(rest) do
    byte_size(rest) < byte_size(previous)
  end

  defp fd_control_accepted?(_write, _rest), do: false

  defp complete_active_write(%__MODULE__{active_write: write} = state) do
    live? = not cancelled_or_expired?(write, state)
    state = drop_active(state, retain_monitor?: live? and write.kind == :call)
    state = %{state | serial: next_serial(write.serial)}

    if live? do
      case write.kind do
        :reply ->
          advance_writes(state)

        :send ->
          GenServer.reply(write.from, :ok)
          advance_writes(state)

        :call ->
          case remaining_timeout(write.deadline) do
            {:ok, remaining} ->
              timer_ref =
                Process.send_after(
                  self(),
                  {:request_timeout, write.serial, write.request_ref},
                  remaining + state.request_timeout_slack
                )

              state = %{
                state
                | outbound_monitor_index:
                    Map.delete(state.outbound_monitor_index, write.monitor_ref)
              }

              pending =
                Map.put(
                  state.pending,
                  write.serial,
                  {write.from, timer_ref, write.request_ref, write.monitor_ref, write.deadline}
                )

              advance_writes(%{
                state
                | pending: pending,
                  request_index: Map.put(state.request_index, write.request_ref, write.serial),
                  monitor_index: Map.put(state.monitor_index, write.monitor_ref, write.serial)
              })

            {:error, :timeout} ->
              advance_writes(release_outbound_monitor(state, write))
          end
      end
    else
      advance_writes(state)
    end
  end

  defp drop_active(%__MODULE__{active_write: write} = state, opts) do
    _ = Process.cancel_timer(write.timer_ref)

    if Keyword.get(opts, :cancel?, false), do: cancel_socket_write(state, write.wait)

    state = %{
      state
      | active_write: nil,
        cancelled_requests: MapSet.delete(state.cancelled_requests, write.request_ref)
    }

    if Keyword.get(opts, :retain_monitor?, false),
      do: state,
      else: release_outbound_monitor(state, write)
  end

  defp release_outbound_monitor(state, %{monitor_ref: nil}), do: state

  defp release_outbound_monitor(state, operation) do
    Process.demonitor(operation.monitor_ref, [:flush])

    %{
      state
      | outbound_monitor_index: Map.delete(state.outbound_monitor_index, operation.monitor_ref)
    }
  end

  defp cancelled_or_expired?(operation, state) do
    cancelled?(operation, state) or expired?(operation)
  end

  defp cancelled?(operation, state),
    do: MapSet.member?(state.cancelled_requests, operation.request_ref)

  defp expired?(operation), do: match?({:error, :timeout}, remaining_timeout(operation.deadline))

  @doc false
  @spec socket_send_args(binary(), nil | {:continue, tuple()}) ::
          {binary(), [] | tuple(), :nowait}
  def socket_send_args(rest, {:continue, continuation}), do: {rest, continuation, :nowait}
  def socket_send_args(rest, _wait), do: {rest, [], :nowait}

  # `socket.erl` in OTP 26--28 stores the encoded original Msg in a sendmsg
  # select continuation (prim_socket:sendmsg/4's Cont is `{Msg, EMsg, EFlags}`).
  # Therefore an IOV-only continuation is correct only when no byte has been
  # accepted. After partial progress we cancel that continuation and send the
  # tail without ctrl, which guarantees SCM_RIGHTS is emitted once.
  defp socket_send(
         %__MODULE__{} = state,
         %{uses_sendmsg?: true, fd_control: :initial, wait: nil} = write
       ) do
    state.sendmsg_fun.(
      state.sock,
      %{
        iov: [write.rest],
        ctrl: [%{level: :socket, type: :rights, data: rights_data(write.unix_fds)}]
      },
      [],
      :nowait
    )
  end

  defp socket_send(
         %__MODULE__{} = state,
         %{uses_sendmsg?: true, fd_control: :select_continuation, wait: {:continue, continuation}} =
           write
       ) do
    state.sendmsg_fun.(state.sock, [write.rest], continuation, :nowait)
  end

  defp socket_send(%__MODULE__{} = state, write) do
    {rest, flags_or_cont, timeout} = socket_send_args(write.rest, write.wait)
    state.send_fun.(state.sock, rest, flags_or_cont, timeout)
  end

  # Socket wrappers are injectable for deterministic state-machine coverage.
  # Never let a malformed result or an injected exception crash the GenServer:
  # that would make OTP log the active frame and its control state.
  defp safe_socket_send(state, write) do
    socket_send(state, write)
  rescue
    _exception -> {:error, :send_failed}
  catch
    _, _ -> {:error, :send_failed}
  end

  defp rights_data(fds) do
    for fd <- fds, into: <<>>, do: <<fd::native-signed-32>>
  end

  defp cancel_socket_write(state, {:select, continuation, _handle}) do
    _ = state.cancel_fun.(state.sock, continuation)
    :ok
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  defp cancel_socket_write(state, {:completion, continuation, _handle}) do
    _ = state.cancel_fun.(state.sock, continuation)
    :ok
  rescue
    _ -> :ok
  catch
    _, _ -> :ok
  end

  defp cancel_socket_write(_state, _wait), do: :ok

  defp reply_if_live(%{from: nil}, _reply, _state), do: :ok

  defp reply_if_live(operation, reply, state) do
    if not cancelled_or_expired?(operation, state), do: GenServer.reply(operation.from, reply)
  end

  defp fail_pending(%__MODULE__{} = state) do
    case state.active_write do
      nil ->
        :ok

      write ->
        _ = Process.cancel_timer(write.timer_ref)
        abandon_outbound_operation(write)
    end

    :queue.to_list(state.write_queue)
    |> Enum.each(&abandon_outbound_operation/1)

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
        outbound_monitor_index: %{},
        active_write: nil,
        write_queue: :queue.new(),
        queued_requests: MapSet.new(),
        cancelled_requests: MapSet.new(),
        queued_replies: 0,
        reply_queue_saturated?: false
    }
  end

  # A queued connection-originated reply is simply discarded on teardown.
  defp abandon_outbound_operation(%{from: nil}), do: :ok

  defp abandon_outbound_operation(operation) do
    Process.demonitor(operation.monitor_ref, [:flush])
    GenServer.reply(operation.from, {:error, :disconnected})
  end

  defp remove_indexes(state, request_ref, monitor_ref) do
    %{
      state
      | request_index: Map.delete(state.request_index, request_ref),
        monitor_index: Map.delete(state.monitor_index, monitor_ref)
    }
  end

  defp allocate_serial(serial, pending), do: allocate_serial(serial, pending, @max_serial)

  @doc false
  @spec allocate_serial(non_neg_integer(), map(), pos_integer()) ::
          {:ok, pos_integer()} | {:error, :serial_exhausted}
  def allocate_serial(serial, pending, max_serial)
      when is_integer(serial) and is_map(pending) and is_integer(max_serial) and max_serial > 0 do
    allocate_serial(serial, pending, max_serial, max_serial)
  end

  defp allocate_serial(_serial, _pending, _max_serial, 0), do: {:error, :serial_exhausted}

  defp allocate_serial(serial, pending, max_serial, attempts) do
    if Map.has_key?(pending, serial) do
      allocate_serial(next_serial(serial, max_serial), pending, max_serial, attempts - 1)
    else
      {:ok, serial}
    end
  end

  defp next_serial(@max_serial), do: 1
  defp next_serial(serial), do: serial + 1
  defp next_serial(max_serial, max_serial), do: 1
  defp next_serial(serial, _max_serial), do: serial + 1
end
