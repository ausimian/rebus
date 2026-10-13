defmodule Rebus.MatchSubscription.Worker do
  @moduledoc false
  use GenServer
  use TypedStruct

  require Logger

  alias Rebus.Connection
  alias Rebus.MatchRule
  alias Rebus.MatchRule.Overlap
  alias Rebus.MatchSubscription.Operation
  alias Rebus.SafeCall

  @cleanup_timeout 1_000
  @max_queued_requests 64
  @max_initial_cleanups 16
  @recovery_delays [50, 100, 200, 400, 800, 1_000]

  defmodule Rule do
    @moduledoc false

    use TypedStruct

    # One match rule this worker is responsible for: the canonical rule, who
    # holds it, and where it has got to in the rule state machine. Everything
    # below `status` bounds the work still owed for it, either the operation in
    # flight and the requests waiting behind it, or the retries of a cleanup
    # whose effect on the bus is not yet known.

    typedstruct enforce: true do
      field :rule, MatchRule.t()
      # The live subscription references, and the handler references whose
      # removal is still owed. Spelled `%MapSet{}` rather than
      # `MapSet.t(reference())` because an empty set does not inhabit that
      # opaque type and every spec naming `t/0` would then be reported as
      # violating it.
      field :refs, %MapSet{}, default: MapSet.new()
      field :pending_handlers, %MapSet{}, default: MapSet.new()
      # Whether the bus may still hold this rule, and so whether cleanup owes
      # it a RemoveMatch.
      field :remote_may_exist?, boolean(), default: false
      # `:uncertain` is terminal: Rebus has given up resolving the rule's bus
      # state and does no further bus work for it on this connection.
      field :status, :installing | :active | :cleaning | :recovering | :uncertain
      # The token of the operation in flight for this rule, and the requests
      # waiting behind it.
      field :operation, reference() | nil, default: nil
      field :queue, [reference()], default: []
      # What the current cleanup is clearing, how many attempts it has taken,
      # and the timer holding the next one.
      field :recovery_kind, :rule | :handlers | nil, default: nil
      field :recovery_attempt, non_neg_integer(), default: 0
      field :retry_timer, reference() | nil, default: nil
    end
  end

  # A worker lives and dies with its connection, and is never restarted: the
  # rules and handlers it manages exist nowhere else, so a replacement would
  # start without them. See `handle_continue/2` for how the two are tied.
  def child_spec(conn) do
    %{
      id: {__MODULE__, conn},
      start: {__MODULE__, :start_link, [conn]},
      restart: :temporary,
      type: :worker
    }
  end

  def start_link(conn) do
    GenServer.start_link(__MODULE__, conn,
      name: {:via, Registry, {Rebus.MatchSubscription.Registry, conn}}
    )
  end

  @spec call(pid(), term(), integer(), non_neg_integer()) :: term()
  def call(worker, request, deadline, overhead) do
    case Operation.remaining_timeout(deadline) do
      {:ok, timeout} when timeout > 0 ->
        SafeCall.call(worker, request, timeout + overhead)

      _expired ->
        {:error, :timeout}
    end
  end

  @typedoc """
  A public call waiting for its reply: the caller to answer, what it asked for,
  and the deadline it must be answered by. A request owns the timer bounding
  that deadline and the monitor on its caller, so answering one releases both.
  `ref` is the subscription a `:remove` names, and `nil` for an `:add`.
  """
  @type request :: %{
          from: GenServer.from(),
          owner: pid(),
          kind: :add | :remove,
          key: String.t(),
          ref: reference() | nil,
          deadline: integer(),
          timer: reference(),
          monitor: reference()
        }

  typedstruct enforce: true do
    field :conn, pid()
    # The local signal handlers installed on this connection, by handler
    # reference, together with the rule each belongs to. A handler already
    # reported gone to the bus is kept as `:removed` until its rule is settled.
    field :subscriptions,
          %{reference() => %{owner: pid(), key: String.t(), handler: :active | :removed}},
          default: %{}

    field :rules, %{String.t() => Rule.t()}, default: %{}
    # Each subscription owner is monitored once: `owner_monitors` finds the
    # subscription a DOWN belongs to, `ref_monitors` the monitor to release
    # when that subscription goes.
    field :owner_monitors, %{reference() => reference()}, default: %{}
    field :ref_monitors, %{reference() => reference()}, default: %{}
    # The calls still waiting for a reply, and the index from the monitor on a
    # caller back to the call it is waiting on.
    field :requests, %{reference() => request()}, default: %{}
    field :request_monitors, %{reference() => reference()}, default: %{}
    # The tasks in flight, by the token their result arrives with, and the
    # index from the monitor on a task back to that token.
    field :operations,
          %{
            reference() => %{
              key: String.t(),
              type: :add_new | :add_existing | :remove | :recovery | :initial_cleanup,
              request_id: reference() | nil,
              monitor: reference() | nil,
              handler: reference() | nil
            }
          },
          default: %{}

    field :operation_monitors, %{reference() => reference()}, default: %{}
    # The keys of the rules in the bounded recovery set. Spelled `%MapSet{}`
    # rather than `MapSet.t(binary())` because an empty set does not inhabit
    # that opaque type and every spec naming `t/0` would then be reported as
    # violating it.
    field :recovering_rules, %MapSet{}, default: MapSet.new()
    # The keys of the rules left `:uncertain`. Together with
    # `recovering_rules` they are the rules whose bus state is unresolved,
    # which is what admission of a new rule is bounded by.
    field :uncertain_rules, %MapSet{}, default: MapSet.new()
    # The keys holding one of the bounded initial-cleanup slots, and the keys
    # waiting for one. `:queue.new/0` is called in `init/1` rather than left to
    # a field default so the queue keeps its opaque type instead of the literal
    # a compile-time default bakes in.
    field :initial_cleanup_keys, %MapSet{}, default: MapSet.new()
    field :initial_cleanup_queue, :queue.queue()
    # Whether this connection is a bus, `nil` until it has been asked once.
    field :bus?, boolean() | nil, default: nil
    # The well-known sender names whose owner this worker has asked the
    # connection to track, and the tracking task in flight for a name with the
    # monitor watching it. Tracking is derived from the installed rules.
    field :tracked_names, %{binary() => true}, default: %{}
    field :tracking_ops, %{binary() => reference() | nil}, default: %{}
    field :tracking_monitors, %{reference() => binary()}, default: %{}
    # The retry a failed tracking sequence owes a name: how many retries it has
    # taken and the timer holding the next one, which is `nil` while the retry
    # it started is in flight. An entry exists only between a failure and the
    # attempt that resolves it or the budget running out.
    field :tracking_retries, %{binary() => %{attempt: pos_integer(), timer: reference() | nil}},
      default: %{}

    # The supervisor operation tasks are started under. A field rather than a
    # literal at the call site so a test can point a worker at a supervisor
    # that refuses to start children and exercise the never-started path.
    field :task_supervisor, Supervisor.supervisor(),
      default: Rebus.MatchSubscription.TaskSupervisor
  end

  @impl true
  def init(conn) do
    {:ok, %__MODULE__{conn: conn, initial_cleanup_queue: :queue.new()}, {:continue, :link}}
  end

  # The worker and its connection are linked, so neither outlives the other.
  # This worker traps exits, so its connection stopping for any reason,
  # `:normal` included, arrives as an `:EXIT` it stops on in turn; linking a
  # connection that is already dead delivers one at once. The connection is
  # told which process this is, so it can stop with a reason that names it
  # when this worker exits: a worker failing is a bug in Rebus, and the rules
  # and handlers it was managing cannot be rebuilt without it.
  @impl true
  def handle_continue(:link, state) do
    Process.flag(:trap_exit, true)
    true = Process.link(state.conn)
    :ok = Connection.attach_match_worker(state.conn)
    {:noreply, state}
  end

  @impl true
  def handle_call({:add, owner, rule, deadline}, from, state) do
    key = MatchRule.to_string(rule)

    cond do
      not before_deadline?(deadline) ->
        {:reply, {:error, :timeout}, state}

      rule_uncertain?(state, key) ->
        {:reply, {:error, :match_rule_uncertain}, state}

      recovery_capacity_exhausted?(state, key) ->
        {:reply, {:error, :match_recovery_capacity_exhausted}, state}

      queue_full?(state, key) ->
        {:reply, {:error, :match_rule_cleanup_pending}, state}

      sender_routing_ambiguous?(state, key, rule) ->
        {:reply, {:error, :sender_routing_ambiguous}, state}

      true ->
        add_subscription(state, from, owner, rule, key, deadline)
    end
  end

  def handle_call({:remove, ref, deadline}, from, state) do
    case Map.fetch(state.subscriptions, ref) do
      {:ok, %{key: key}} ->
        cond do
          not before_deadline?(deadline) ->
            {:reply, {:error, :timeout}, state}

          queue_full?(state, key) ->
            {:reply, {:error, :match_rule_cleanup_pending}, state}

          true ->
            {request_id, state} =
              put_request(state, from, elem(from, 0), :remove, key, ref, deadline)

            state = enqueue_request(state, key, request_id)
            {:noreply, dispatch_rule(state, key)}
        end

      :error ->
        # A reference is idempotent and scoped to its original connection.
        {:reply, :ok, state}
    end
  end

  defp add_subscription(state, from, owner, rule, key, deadline) do
    case ensure_bus(state, deadline) do
      {:ok, state} ->
        {request_id, state} = put_request(state, from, owner, :add, key, nil, deadline)
        state = ensure_rule(state, key, rule)
        state = enqueue_request(state, key, request_id)
        {:noreply, dispatch_rule(state, key)}

      {:error, reason, state} ->
        {:reply, {:error, reason}, state}
    end
  end

  # AddMatch is a bus-driver method, so it cannot be served by a peer-to-peer
  # connection. The answer is fixed for the connection's life: ask once, then
  # cache it so an established bus connection pays no extra round trip.
  defp ensure_bus(%__MODULE__{bus?: true} = state, _deadline), do: {:ok, state}
  defp ensure_bus(%__MODULE__{bus?: false} = state, _deadline), do: {:error, :not_a_bus, state}

  defp ensure_bus(state, deadline) do
    case Operation.remaining_timeout(deadline) do
      {:ok, timeout} ->
        case Connection.bus?(state.conn, timeout) do
          true -> {:ok, %{state | bus?: true}}
          false -> {:error, :not_a_bus, %{state | bus?: false}}
          {:error, reason} -> {:error, reason, state}
        end

      {:error, reason} ->
        {:error, reason, state}
    end
  end

  # A disconnected bus drops all server-side match state, so there is nothing
  # left to clean up and no transport to send RemoveMatch through.
  @impl true
  def handle_info({:EXIT, conn, _reason}, %__MODULE__{conn: conn} = state),
    do: {:stop, :normal, state}

  # Any other link keeps its usual meaning: the registry this worker is
  # registered in, for one.
  def handle_info({:EXIT, _pid, :normal}, state), do: {:noreply, state}
  def handle_info({:EXIT, _pid, reason}, state), do: {:stop, reason, state}

  def handle_info({:request_timeout, request_id}, state) do
    case Map.fetch(state.requests, request_id) do
      {:ok, %{key: key}} ->
        state = reply_request(state, request_id, {:error, :timeout})
        {:noreply, maybe_progress_rule(state, key)}

      :error ->
        {:noreply, state}
    end
  end

  def handle_info({:retry_recovery, key}, state) do
    case Map.get(state.rules, key) do
      %Rule{status: :recovering} = rule ->
        state = put_rule(state, key, %{rule | retry_timer: nil})
        {:noreply, start_recovery_attempt(state, key)}

      _ ->
        {:noreply, state}
    end
  end

  def handle_info({:operation_result, token, result}, state) do
    case take_operation(state, token) do
      {nil, state} ->
        {:noreply, state}

      {%{key: key} = operation, state} ->
        state = complete_operation(state, key, operation, result)
        {:noreply, reconcile_tracking(state)}
    end
  end

  def handle_info({:tracking_result, name, result}, state) do
    log_tracking(name, result)

    state =
      state
      |> clear_tracking_op(name)
      |> settle_tracking_retry(name, result)
      |> reconcile_tracking()

    {:noreply, state}
  end

  def handle_info({:retry_tracking, name}, state) do
    {:noreply, retry_tracking(state, name)}
  end

  def handle_info({:DOWN, monitor_ref, :process, _pid, _reason}, state) do
    cond do
      ref = Map.get(state.owner_monitors, monitor_ref) ->
        # An uncertain rule starts no cleanup whose result would reconcile
        # tracking, so the last owner of one leaving must do it here.
        state = owner_down(state, monitor_ref, ref)
        {:noreply, reconcile_tracking(state)}

      request_id = Map.get(state.request_monitors, monitor_ref) ->
        key = request_key(state, request_id)
        state = reply_request(state, request_id, {:error, :disconnected})
        state = if(key, do: maybe_progress_rule(state, key), else: state)
        {:noreply, state}

      name = Map.get(state.tracking_monitors, monitor_ref) ->
        # Owner tracking is best-effort and deliberately outside the rule
        # recovery machinery: a lost task leaves the name tracked but unseeded,
        # which rejects directed signals exactly as before it was tracked,
        # until one of the bounded retries below seeds it.
        log_tracking(name, {:error, {:task, :lost}})

        state =
          state
          |> clear_tracking_op(name)
          |> settle_tracking_retry(name, {:error, {:task, :lost}})
          |> reconcile_tracking()

        {:noreply, state}

      token = Map.get(state.operation_monitors, monitor_ref) ->
        # A task that finishes normally sends its result first. Reaching this
        # branch means an operation died before reporting a safe outcome, so
        # its rule is left uncertain rather than guessing about a handler or
        # bus-rule state.
        operation_down(state, token)

      true ->
        {:noreply, state}
    end
  end

  defp operation_down(state, token) do
    case take_operation(state, token) do
      {nil, state} ->
        {:noreply, state}

      {%{key: key} = operation, state} ->
        state = operation_failed(state, key, operation)
        {:noreply, reconcile_tracking(state)}
    end
  end

  @impl true
  def terminate(_reason, state) do
    Enum.each(state.requests, fn {request_id, _request} ->
      _state = reply_request(state, request_id, {:error, :disconnected})
    end)

    :ok
  end

  defp dispatch_rule(state, key) do
    case Map.get(state.rules, key) do
      nil ->
        state

      %Rule{status: :recovering} ->
        state

      %Rule{status: :cleaning} ->
        state

      %Rule{operation: operation} when not is_nil(operation) ->
        state

      rule ->
        dispatch_next_request(state, key, rule)
    end
  end

  defp dispatch_next_request(state, key, rule) do
    {request_id, queue} = next_live_request(rule.queue, state.requests)
    state = put_rule(state, key, %{rule | queue: queue})

    case request_id && Map.get(state.requests, request_id) do
      nil ->
        maybe_idle_rule(state, key)

      %{kind: :add} = request when rule.status == :installing ->
        start_add(state, key, :add_new, request_id, request)

      %{kind: :add} = request when rule.status == :active ->
        start_add(state, key, :add_existing, request_id, request)

      %{kind: :add} when rule.status == :uncertain ->
        dispatch_rule(reply_request(state, request_id, {:error, :match_rule_uncertain}), key)

      # A reference on an uncertain rule can still be removed, so its holder
      # can stop its signals, but only locally: see `start_remove/5`.
      %{kind: :remove, ref: ref} = request when rule.status in [:active, :uncertain] ->
        dispatch_remove(state, key, request_id, request, ref)

      %{kind: :remove} ->
        dispatch_rule(reply_request(state, request_id, :ok), key)
    end
  end

  defp dispatch_remove(state, key, request_id, request, ref) do
    case Map.fetch(state.subscriptions, ref) do
      {:ok, subscription} -> start_remove(state, key, request_id, request, subscription)
      :error -> dispatch_rule(reply_request(state, request_id, :ok), key)
    end
  end

  defp start_add(state, key, type, request_id, request) do
    rule = Map.fetch!(state.rules, key).rule
    conn = state.conn
    owner = request.owner
    deadline = request.deadline
    handler_ref = make_ref()

    fun =
      case type do
        :add_new ->
          fn -> Operation.add_new(conn, owner, handler_ref, rule, deadline) end

        :add_existing ->
          fn -> Operation.add_existing(conn, owner, handler_ref, rule, deadline) end
      end

    start_operation(state, key, type, request_id, fun, handler_ref)
  end

  # The last reference of an uncertain rule is never final: Rebus does no more
  # bus work for that rule, so its removal deletes the local handler and sends
  # no RemoveMatch.
  defp start_remove(state, key, request_id, request, subscription) do
    rule = Map.fetch!(state.rules, key)
    final? = rule.status == :active and MapSet.size(rule.refs) == 1
    conn = state.conn
    canonical_rule = rule.rule
    ref = request.ref
    handler = subscription.handler
    deadline = request.deadline

    start_operation(state, key, :remove, request_id, fn ->
      Operation.remove(
        conn,
        canonical_rule,
        ref,
        handler,
        final?,
        deadline
      )
    end)
  end

  defp start_recovery_attempt(state, key) do
    case Map.get(state.rules, key) do
      %Rule{status: status, operation: nil, retry_timer: nil} = rule
      when status in [:cleaning, :recovering] ->
        conn = state.conn
        deadline = System.monotonic_time(:millisecond) + @cleanup_timeout

        recovery = %{
          pending_handlers: rule.pending_handlers,
          recovery_kind: rule.recovery_kind,
          remote_may_exist?: rule.remote_may_exist?,
          rule: rule.rule
        }

        operation_type = if status == :cleaning, do: :initial_cleanup, else: :recovery

        state =
          if status == :cleaning do
            %{state | initial_cleanup_keys: MapSet.put(state.initial_cleanup_keys, key)}
          else
            state
          end

        start_operation(state, key, operation_type, nil, fn ->
          Operation.recover(conn, recovery, deadline)
        end)

      _ ->
        state
    end
  end

  # `handler` is the local handler an add installs, recorded so a task lost
  # mid-flight still leaves a known handler to cancel.
  defp start_operation(state, key, type, request_id, fun, handler \\ nil) do
    token = make_ref()
    worker = self()
    operation = %{key: key, type: type, request_id: request_id, monitor: nil, handler: handler}

    started =
      Task.Supervisor.start_child(state.task_supervisor, fn ->
        send(worker, {:operation_result, token, fun.()})
      end)

    rule = Map.fetch!(state.rules, key)
    state = put_rule(state, key, %{rule | operation: token})

    case started do
      {:ok, pid} ->
        monitor_ref = Process.monitor(pid)

        %{
          state
          | operations: Map.put(state.operations, token, %{operation | monitor: monitor_ref}),
            operation_monitors: Map.put(state.operation_monitors, monitor_ref, token)
        }

      {:error, _reason} ->
        # No task ran, therefore nothing was sent to the bus and no local
        # handler was attached. Report that distinctly from a task that died
        # mid-flight, whose effect on the bus is genuinely unknown. The
        # operation is still recorded so the synthesised result is handled
        # like any other.
        send(self(), {:operation_result, token, :not_started})
        %{state | operations: Map.put(state.operations, token, operation)}
    end
  end

  defp take_operation(state, token) do
    case Map.pop(state.operations, token) do
      {nil, _operations} ->
        {nil, state}

      {%{key: key, monitor: monitor_ref} = operation, operations} ->
        if is_reference(monitor_ref), do: Process.demonitor(monitor_ref, [:flush])

        state = %{
          state
          | operations: operations,
            operation_monitors:
              if(is_reference(monitor_ref),
                do: Map.delete(state.operation_monitors, monitor_ref),
                else: state.operation_monitors
              )
        }

        state =
          case Map.get(state.rules, key) do
            nil -> state
            rule -> put_rule(state, key, %{rule | operation: nil})
          end

        {operation, state}
    end
  end

  defp complete_operation(state, key, operation, {:operation_failed, _reason}) do
    operation_failed(state, key, operation)
  end

  # A task that never started is the one unambiguous failure: it sent nothing
  # to the bus and attached no local handler, so the rule and its references
  # are exactly what they were before the operation. Unlike `operation_down/2`
  # there is no unresolved bus state to leave the rule uncertain over, and
  # unlike `{:operation_failed, _}` there is nothing to blame on the connection.
  defp complete_operation(state, key, %{type: :add_new, request_id: request_id}, :not_started) do
    log_not_started(:add_new)

    # The rule was created for this operation and installed nothing, so it is
    # torn down exactly as a definitive AddMatch failure that owes no
    # RemoveMatch is, redispatching anything queued behind it.
    state
    |> reply_request_if_live(request_id, {:error, :not_started})
    |> maybe_progress_rule(key)
  end

  defp complete_operation(state, key, %{type: type, request_id: request_id}, :not_started)
       when type in [:add_existing, :remove] do
    log_not_started(type)

    # The rule stays `:active` either way: no reference was added, and none was
    # removed, so the caller's own reference is still good for a retry.
    state
    |> reply_request_if_live(request_id, {:error, :not_started})
    |> dispatch_rule(key)
  end

  defp complete_operation(state, key, %{type: :recovery}, :not_started) do
    log_recovery(:recovery_not_started)

    # Nothing was cleared, so the rule stays `:recovering` with its owed
    # handler removals intact and simply waits for the next attempt.
    schedule_recovery_retry(state, key)
  end

  defp complete_operation(state, key, %{type: :initial_cleanup}, :not_started) do
    log_recovery(:initial_cleanup_not_started)

    # The bus was never asked, so the cleanup is still owed. Give the slot
    # back, graduate the rule to the bounded recovery set as an unproven
    # cleanup does, and let the next queued key take the slot.
    state
    |> release_initial_cleanup(key)
    |> enter_recovery(key, :rule)
    |> start_next_initial_cleanup()
  end

  defp complete_operation(state, key, %{type: :add_new, request_id: request_id}, result) do
    complete_add_new(state, key, request_id, result)
  end

  defp complete_operation(state, key, %{type: :add_existing, request_id: request_id}, result) do
    complete_add_existing(state, key, request_id, result)
  end

  defp complete_operation(state, key, %{type: :remove, request_id: request_id}, result) do
    complete_remove(state, key, request_id, result)
  end

  defp complete_operation(state, key, %{type: :recovery}, result) do
    complete_recovery(state, key, result)
  end

  defp complete_operation(state, key, %{type: :initial_cleanup}, result) do
    state
    |> release_initial_cleanup(key)
    |> complete_initial_cleanup(key, result)
    |> start_next_initial_cleanup()
  end

  defp complete_add_new(state, key, request_id, {:added, handler_ref}) do
    case accept_add_request(state, request_id) do
      {:ok, request, state} ->
        state = put_active_rule(state, key)
        state = put_subscription(state, request.owner, key, handler_ref)
        state = dispatch_rule(state, key)
        reply_request(state, request_id, {:ok, handler_ref})

      {:expired, state} ->
        state
        |> put_pending_handler(key, handler_ref)
        |> put_remote_may_exist(key, true)
        |> enter_recovery(key, :rule)
    end
  end

  defp complete_add_new(state, key, request_id, {:add_failed, error, handler_ref}) do
    state = reply_request_if_live(state, request_id, error)
    remote_may_exist? = not Operation.definitive_bus_error?(error)

    state =
      state
      |> maybe_put_pending_handler(key, handler_ref)
      |> put_remote_may_exist(key, remote_may_exist?)

    if handler_ref || remote_may_exist? do
      enter_recovery(state, key, :rule)
    else
      maybe_progress_rule(state, key)
    end
  end

  defp complete_add_existing(state, key, request_id, {:added_existing, handler_ref}) do
    case accept_add_request(state, request_id) do
      {:ok, request, state} ->
        state = put_subscription(state, request.owner, key, handler_ref)
        state = dispatch_rule(state, key)
        reply_request(state, request_id, {:ok, handler_ref})

      {:expired, state} ->
        state = put_pending_handler(state, key, handler_ref)

        if MapSet.size(Map.fetch!(state.rules, key).refs) == 0 do
          enter_recovery(state, key, :rule)
        else
          enter_recovery(state, key, :handlers)
        end
    end
  end

  defp complete_add_existing(state, key, request_id, {:add_existing_failed, error}) do
    state
    |> reply_request_if_live(request_id, error)
    |> dispatch_rule(key)
  end

  defp complete_remove(state, key, request_id, {:remove_failed, error, :active}) do
    state
    |> reply_request_if_live(request_id, error)
    |> dispatch_rule(key)
  end

  defp complete_remove(state, key, request_id, {:removed, ref, :nonfinal}) do
    state = drop_subscription(state, ref)
    state = dispatch_rule(state, key)
    reply_request_if_live(state, request_id, :ok)
  end

  defp complete_remove(state, key, request_id, {:removed, ref, :final}) do
    state = drop_subscription(state, ref)
    state = clear_rule_and_resume(state, key)
    reply_request_if_live(state, request_id, :ok)
  end

  defp complete_remove(state, key, request_id, {:remove_definitive_error, ref, error}) do
    state = mark_handler_removed(state, ref)
    state = reply_request_if_live(state, request_id, error)
    dispatch_rule(state, key)
  end

  defp complete_remove(state, key, request_id, {:remove_ambiguous, ref, error}) do
    state = mark_handler_removed(state, ref)
    state = reply_request_if_live(state, request_id, error)
    enter_recovery(state, key, :rule)
  end

  defp complete_recovery(state, key, :cleared) do
    log_recovery(:cleared)
    clear_rule_and_resume(state, key)
  end

  defp complete_recovery(state, key, :handlers_cleared) do
    log_recovery(:handlers_cleared)

    state
    |> put_pending_handlers(key, MapSet.new())
    |> leave_recovery(key, :active)
    |> maybe_progress_rule(key)
  end

  defp complete_recovery(state, key, {:retry, :handlers}) do
    log_recovery(:handler_retry)
    schedule_recovery_retry(state, key)
  end

  defp complete_recovery(state, key, {:retry, :remote}) do
    log_recovery(:remote_retry)
    state |> put_pending_handlers(key, MapSet.new()) |> schedule_recovery_retry(key)
  end

  defp complete_recovery(state, key, {:definitive_bus_error, _error}) do
    # This result is definitive (and is logged as such), but it cannot prove
    # that a previous RemoveMatch did not take effect. Keep the rule in the
    # bounded recovery set and retry rather than silently forgetting it or
    # permanently quarantining ordinary owner cleanup.
    log_recovery(:definitive_bus_error)
    schedule_recovery_retry(state, key)
  end

  defp complete_initial_cleanup(state, key, :cleared), do: complete_recovery(state, key, :cleared)

  defp complete_initial_cleanup(state, key, :handlers_cleared),
    do: complete_recovery(state, key, :handlers_cleared)

  defp complete_initial_cleanup(state, key, {:retry, :handlers}) do
    log_recovery(:initial_handler_retry)
    enter_recovery(state, key, :handlers)
  end

  defp complete_initial_cleanup(state, key, {:retry, :remote}) do
    log_recovery(:initial_remote_retry)
    enter_recovery(state, key, :rule)
  end

  defp complete_initial_cleanup(state, key, {:definitive_bus_error, _error}) do
    log_recovery(:initial_definitive_bus_error)
    enter_recovery(state, key, :rule)
  end

  # Every operation type fails the same way: the task died without reporting a
  # safe outcome, so nothing it may have done to the bus rule can be known, and
  # its rule is left uncertain. The handler an add may have installed is owed a
  # removal like any other pending handler, which `mark_uncertain/3` casts. The
  # request-bearing types answer their caller; the recovery tracks never carry
  # a request.
  defp operation_failed(state, key, operation) do
    state
    |> reply_request_if_present(operation.request_id, {:error, :match_rule_uncertain})
    |> maybe_put_pending_handler(key, Map.get(operation, :handler))
    |> mark_uncertain(key, :operation_lost)
  end

  defp owner_down(state, monitor_ref, ref) do
    state = %{
      state
      | owner_monitors: Map.delete(state.owner_monitors, monitor_ref),
        ref_monitors: Map.delete(state.ref_monitors, ref)
    }

    case Map.pop(state.subscriptions, ref) do
      {nil, _subscriptions} ->
        state

      {%{key: key}, subscriptions} ->
        rule = Map.fetch!(state.rules, key)
        refs = MapSet.delete(rule.refs, ref)

        pending_handlers =
          if MapSet.size(refs) == 0,
            do: MapSet.put(rule.pending_handlers, ref),
            else: rule.pending_handlers

        state = %{state | subscriptions: subscriptions}

        state =
          put_rule(state, key, %{
            rule
            | refs: refs,
              pending_handlers: pending_handlers
          })

        maybe_progress_rule(state, key)
    end
  end

  defp maybe_progress_rule(state, key) do
    state
    |> maybe_idle_rule(key)
    |> dispatch_rule(key)
  end

  defp maybe_idle_rule(state, key) do
    case Map.get(state.rules, key) do
      nil ->
        state

      %Rule{status: :recovering} ->
        state

      %Rule{operation: operation} when not is_nil(operation) ->
        state

      %Rule{status: :active} = rule ->
        if MapSet.size(rule.refs) == 0 and not has_live_add?(rule.queue, state.requests) do
          start_initial_cleanup(state, key)
        else
          state
        end

      %Rule{status: :installing, queue: []} ->
        delete_rule(state, key)

      _rule ->
        state
    end
  end

  # The first best-effort cleanup after the final owner exits is not yet an
  # ambiguity. It must therefore neither consume nor be rejected by the cap
  # reserved for rules whose server state is actually unresolved. Limit its
  # concurrent bus work separately and queue the rest without dropping state.
  defp start_initial_cleanup(state, key) do
    case Map.get(state.rules, key) do
      %Rule{status: :active, operation: nil} = rule ->
        rule = %{
          rule
          | status: :cleaning,
            recovery_kind: :rule,
            recovery_attempt: 0,
            retry_timer: nil
        }

        state
        |> put_rule(key, rule)
        |> admit_initial_cleanup(key)

      _ ->
        state
    end
  end

  # The one place the initial-cleanup concurrency cap is applied: a rule marked
  # `:cleaning` either takes a free slot now or waits in the queue that
  # `start_next_initial_cleanup/1` drains as slots come back.
  defp admit_initial_cleanup(state, key) do
    if MapSet.size(state.initial_cleanup_keys) < @max_initial_cleanups do
      start_recovery_attempt(state, key)
    else
      %{state | initial_cleanup_queue: :queue.in(key, state.initial_cleanup_queue)}
    end
  end

  defp release_initial_cleanup(state, key) do
    %{state | initial_cleanup_keys: MapSet.delete(state.initial_cleanup_keys, key)}
  end

  defp start_next_initial_cleanup(state) do
    if MapSet.size(state.initial_cleanup_keys) >= @max_initial_cleanups do
      state
    else
      case :queue.out(state.initial_cleanup_queue) do
        {{:value, key}, queue} ->
          resume_initial_cleanup(%{state | initial_cleanup_queue: queue}, key)

        {:empty, _queue} ->
          state
      end
    end
  end

  defp resume_initial_cleanup(state, key) do
    case Map.get(state.rules, key) do
      %Rule{status: :cleaning, operation: nil} ->
        state
        |> start_recovery_attempt(key)
        |> start_next_initial_cleanup()

      _ ->
        start_next_initial_cleanup(state)
    end
  end

  defp enter_recovery(state, key, kind) when kind in [:rule, :handlers] do
    case Map.get(state.rules, key) do
      nil ->
        state

      %Rule{status: status} when status in [:recovering, :uncertain] ->
        state

      %Rule{operation: operation} when not is_nil(operation) ->
        state

      rule ->
        if MapSet.size(state.recovering_rules) >= max_recovering_rules() do
          mark_uncertain(state, key, :capacity_exhausted)
        else
          rule = %{
            rule
            | status: :recovering,
              recovery_kind: kind,
              recovery_attempt: 0,
              retry_timer: nil
          }

          state = put_rule(state, key, rule)
          state = %{state | recovering_rules: MapSet.put(state.recovering_rules, key)}

          log_recovery(:started)
          start_recovery_attempt(state, key)
        end
    end
  end

  # The backoff saturates at one second, so retrying is otherwise unbounded: a
  # bus that stays ambiguous for this one rule would be retried forever,
  # visible only at debug level. A rule whose bus state cannot be proven is
  # never silently forgotten, so when the budget runs out it is left uncertain
  # and its subscribers and the connection's owner are told, exactly as the
  # capacity branch of `enter_recovery/3` does for a rule it has no room for.
  defp schedule_recovery_retry(state, key) do
    case Map.get(state.rules, key) do
      %Rule{status: :recovering, retry_timer: nil} = rule ->
        attempt = rule.recovery_attempt + 1

        if attempt > max_recovery_attempts() do
          mark_uncertain(state, key, :recovery_exhausted)
        else
          maybe_log_recovery_saturated(attempt)
          timer = Process.send_after(self(), {:retry_recovery, key}, retry_delay(attempt))
          put_rule(state, key, %{rule | recovery_attempt: attempt, retry_timer: timer})
        end

      _ ->
        state
    end
  end

  defp leave_recovery(state, key, status) do
    case Map.get(state.rules, key) do
      nil ->
        state

      rule ->
        if is_reference(rule.retry_timer),
          do: Process.cancel_timer(rule.retry_timer, async: true, info: false)

        state = %{
          state
          | recovering_rules: MapSet.delete(state.recovering_rules, key)
        }

        put_rule(state, key, %{
          rule
          | status: status,
            recovery_kind: nil,
            recovery_attempt: 0,
            retry_timer: nil
        })
    end
  end

  # Leaves a rule whose bus state Rebus can no longer resolve exactly where it
  # is, instead of closing the connection to make the bus forget it. The rule
  # stops all bus work: it is dropped from the recovery set, its retry timer
  # and any initial-cleanup slot it held are released, and it is never removed
  # from the bus. Its live references keep their local handlers, and the
  # requests queued behind it are answered now rather than left to time out.
  #
  # The handlers it still owed a removal are local, so they are not left
  # behind with it: each is cancelled with a cast, which needs no reply from a
  # connection whose calls may be what kept failing, and which is a no-op for
  # a handler that is already gone.
  #
  # The holder of every reference on the rule, and the connection's owner when
  # it has one, are sent `{:rebus_match_uncertain, conn, ref | nil, rule}` so
  # the application can decide what to do, closing the connection included.
  # A four-element tuple tagged with an atom cannot be mistaken for a
  # `{ref, %Rebus.Message{}}` delivery by any receive pattern written for one.
  defp mark_uncertain(state, key, event) do
    case Map.get(state.rules, key) do
      nil ->
        state

      # Already uncertain, so only a local removal can have failed. Nothing
      # is in flight now, so the requests queued behind it go next.
      %Rule{status: :uncertain} ->
        dispatch_rule(state, key)

      rule ->
        if is_reference(rule.retry_timer),
          do: Process.cancel_timer(rule.retry_timer, async: true, info: false)

        log_uncertain(event)
        Enum.each(rule.pending_handlers, &Connection.cancel_signal_handler(state.conn, &1))

        state = %{
          state
          | recovering_rules: MapSet.delete(state.recovering_rules, key),
            uncertain_rules: MapSet.put(state.uncertain_rules, key)
        }

        state
        |> put_rule(key, %{
          rule
          | status: :uncertain,
            pending_handlers: MapSet.new(),
            retry_timer: nil
        })
        |> notify_uncertain(rule)
        |> release_initial_cleanup(key)
        |> start_next_initial_cleanup()
        |> dispatch_rule(key)
    end
  end

  defp notify_uncertain(state, %Rule{rule: rule, refs: refs}) do
    Enum.each(refs, fn ref ->
      case Map.get(state.subscriptions, ref) do
        %{owner: owner} -> send(owner, {:rebus_match_uncertain, state.conn, ref, rule})
        nil -> :ok
      end
    end)

    :ok = Connection.notify_owner(state.conn, {:rebus_match_uncertain, state.conn, nil, rule})
    state
  end

  defp rule_uncertain?(state, key), do: MapSet.member?(state.uncertain_rules, key)

  # Admission is what bounds the uncertain rules now that closing the
  # connection no longer discards them. A rule can only become unresolved
  # after it exists, so refusing new rules while the unresolved ones fill the
  # cap bounds them by the rules that existed when it filled. Equivalent rules
  # already held are still served: they add no bus registration.
  defp recovery_capacity_exhausted?(state, key) do
    not Map.has_key?(state.rules, key) and
      MapSet.size(state.recovering_rules) + MapSet.size(state.uncertain_rules) >=
        max_recovering_rules()
  end

  defp clear_rule_and_resume(state, key) do
    case Map.get(state.rules, key) do
      nil ->
        state

      rule ->
        queue = rule.queue
        state = leave_recovery(state, key, :installing)

        state =
          Enum.reduce(rule.refs, state, fn ref, acc ->
            drop_subscription(acc, ref)
          end)

        state = delete_rule(state, key)

        {add_requests, state} = Enum.reduce(queue, {[], state}, &split_pending_request/2)

        case Enum.reverse(add_requests) do
          [] ->
            state

          queue ->
            rule = new_rule(rule.rule, :installing)
            state = put_rule(state, key, %{rule | queue: queue})
            dispatch_rule(state, key)
        end
    end
  end

  # Splits a rule's queued requests: pending adds are kept for the reinstalled
  # rule, while pending removes are already satisfied by the clearing.
  defp split_pending_request(request_id, {adds, state}) do
    case Map.get(state.requests, request_id) do
      %{kind: :add} -> {[request_id | adds], state}
      %{kind: :remove} -> {adds, reply_request(state, request_id, :ok)}
      nil -> {adds, state}
    end
  end

  # The set of names to track is derived from the rules this worker holds, so
  # no extra refcount is kept: a name is needed while at least one installed
  # rule names it as a well-known sender. Reconciling after every operation
  # therefore installs the tracking rule once for the first such rule and
  # removes it after the last one goes.
  defp reconcile_tracking(state) do
    needed = needed_tracked_names(state)

    state =
      Enum.reduce(needed, state, fn {name, true}, acc ->
        if Map.has_key?(acc.tracked_names, name), do: acc, else: start_name_tracking(acc, name)
      end)

    Enum.reduce(state.tracked_names, state, fn {name, true}, acc ->
      if Map.has_key?(needed, name), do: acc, else: stop_name_tracking(acc, name)
    end)
  end

  defp needed_tracked_names(state) do
    Enum.reduce(state.rules, %{}, fn {_key, %Rule{rule: rule} = entry}, names ->
      case delivering?(entry) && trackable_sender(rule) do
        name when is_binary(name) -> Map.put(names, name, true)
        _none -> names
      end
    end)
  end

  # An uncertain rule that still holds references keeps its name tracked:
  # Rebus has stopped managing its bus registration, not its local delivery.
  defp delivering?(%Rule{status: :active}), do: true
  defp delivering?(%Rule{status: :uncertain, refs: refs}), do: MapSet.size(refs) > 0
  defp delivering?(%Rule{}), do: false

  # Only a well-known sender needs an owner. A unique sender is compared
  # directly, and the bus driver sends its own directed signals under the
  # literal name `org.freedesktop.DBus`, which already matches.
  defp trackable_sender(%MatchRule{criteria: %{sender: <<":", _::binary>>}}), do: nil
  defp trackable_sender(%MatchRule{criteria: %{sender: "org.freedesktop.DBus"}}), do: nil
  defp trackable_sender(%MatchRule{criteria: %{sender: sender}}), do: sender
  defp trackable_sender(%MatchRule{}), do: nil

  # A name with a task in flight is left alone: that task's result reconciles
  # again, so an add and a remove for the same name never overlap on the bus.
  defp start_name_tracking(state, name) do
    if Map.has_key?(state.tracking_ops, name) do
      state
    else
      state = %{state | tracked_names: Map.put(state.tracked_names, name, true)}
      start_tracking_task(state, name, &Operation.track_owner(&1, name))
    end
  end

  # A name that stops being needed drops whatever retry it was owed: the
  # untrack below is the last word on it, and a retry that fired anyway would
  # reinstall the tracking rule the untrack has just removed.
  defp stop_name_tracking(state, name) do
    if Map.has_key?(state.tracking_ops, name) do
      state
    else
      state = %{state | tracked_names: Map.delete(state.tracked_names, name)}
      state = clear_tracking_retry(state, name)
      start_tracking_task(state, name, &Operation.untrack_owner(&1, name))
    end
  end

  # A tracking result does not say which sequence produced it. It does not have
  # to: `stop_name_tracking/2` drops the latch before starting the untrack, so
  # a result for a name still in `tracked_names` is a track's and one for a
  # name that has left it is an untrack's.
  #
  # An untrack is not retried. A tracking RemoveMatch the bus refused leaves an
  # internal rule behind, which only costs a duplicate copy of a
  # NameOwnerChanged the connection already handles, and a later re-track
  # installs its own rule regardless, since dbus-daemon accepts identical
  # AddMatch rules.
  defp settle_tracking_retry(state, name, result) do
    cond do
      not Map.has_key?(state.tracked_names, name) -> state
      result == :ok -> clear_tracking_retry(state, name)
      true -> schedule_tracking_retry(state, name)
    end
  end

  # The whole sequence is retried, not the step that failed: every step is
  # idempotent, and the worker cannot tell from a failure how far the bus got.
  # A budget spent without a success is the end of it, exactly as before this
  # was retried at all: the name stays latched and `:unknown` in the
  # connection, so directed signals for it stay rejected, and nothing here ever
  # closes the connection or enters rule recovery.
  #
  # A pending timer is not cancelled at `terminate/2`, any more than a recovery
  # one is: its message would only be delivered to a dead process.
  defp schedule_tracking_retry(state, name) do
    %{attempt: attempt} = entry = tracking_retry(state, name)
    next = attempt + 1

    if next > max_tracking_attempts() do
      log_tracking_exhausted(name, attempt)
      clear_tracking_retry(state, name)
    else
      timer = Process.send_after(self(), {:retry_tracking, name}, retry_delay(next))
      put_tracking_retry(state, name, %{entry | attempt: next, timer: timer})
    end
  end

  # The retry restarts the task directly rather than going through
  # `start_name_tracking/2`, whose latch check would skip a name the failed
  # attempt left marked as tracked.
  #
  # A timer is cancelled asynchronously, so a retry can still arrive for a name
  # that has since stopped being needed, or been picked up by a reconcile of
  # its own. Anything but a name still owed this retry drops the entry instead.
  defp retry_tracking(state, name) do
    state = clear_tracking_retry_timer(state, name)

    if retry_tracking?(state, name) do
      start_tracking_task(state, name, &Operation.track_owner(&1, name))
    else
      clear_tracking_retry(state, name)
    end
  end

  defp retry_tracking?(state, name) do
    Map.has_key?(state.tracking_retries, name) and
      Map.has_key?(state.tracked_names, name) and
      not Map.has_key?(state.tracking_ops, name) and
      Map.has_key?(needed_tracked_names(state), name)
  end

  defp tracking_retry(state, name) do
    Map.get(state.tracking_retries, name, %{attempt: 0, timer: nil})
  end

  defp put_tracking_retry(state, name, entry) do
    %{state | tracking_retries: Map.put(state.tracking_retries, name, entry)}
  end

  defp clear_tracking_retry_timer(state, name) do
    case Map.get(state.tracking_retries, name) do
      nil -> state
      entry -> put_tracking_retry(state, name, %{entry | timer: nil})
    end
  end

  defp clear_tracking_retry(state, name) do
    case Map.pop(state.tracking_retries, name) do
      {nil, _tracking_retries} ->
        state

      {%{timer: timer}, tracking_retries} ->
        if is_reference(timer), do: Process.cancel_timer(timer, async: true, info: false)
        %{state | tracking_retries: tracking_retries}
    end
  end

  # Tracking runs outside the rule state machine: its failures never close the
  # connection, never fail a caller's add_match/3, and never enter recovery.
  #
  # No deadline is passed: unlike a caller operation, a tracking sequence has
  # no single budget. `Rebus.MatchSubscription.Operation` bounds each of its
  # bus round trips separately.
  defp start_tracking_task(state, name, fun) do
    worker = self()
    conn = state.conn

    case Task.Supervisor.start_child(Rebus.MatchSubscription.TaskSupervisor, fn ->
           send(worker, {:tracking_result, name, fun.(conn)})
         end) do
      {:ok, pid} ->
        monitor_ref = Process.monitor(pid)

        %{
          state
          | tracking_ops: Map.put(state.tracking_ops, name, monitor_ref),
            tracking_monitors: Map.put(state.tracking_monitors, monitor_ref, name)
        }

      {:error, reason} ->
        send(self(), {:tracking_result, name, {:error, {:start, reason}}})
        %{state | tracking_ops: Map.put(state.tracking_ops, name, nil)}
    end
  end

  defp clear_tracking_op(state, name) do
    case Map.pop(state.tracking_ops, name) do
      {nil, tracking_ops} ->
        %{state | tracking_ops: tracking_ops}

      {monitor_ref, tracking_ops} ->
        Process.demonitor(monitor_ref, [:flush])

        %{
          state
          | tracking_ops: tracking_ops,
            tracking_monitors: Map.delete(state.tracking_monitors, monitor_ref)
        }
    end
  end

  defp put_request(state, from, owner, kind, key, ref, deadline) do
    request_id = make_ref()
    monitor_ref = Process.monitor(owner)
    timeout = max(deadline - System.monotonic_time(:millisecond), 0)
    timer = Process.send_after(self(), {:request_timeout, request_id}, timeout)

    request = %{
      from: from,
      owner: owner,
      kind: kind,
      key: key,
      ref: ref,
      deadline: deadline,
      timer: timer,
      monitor: monitor_ref
    }

    {request_id,
     %{
       state
       | requests: Map.put(state.requests, request_id, request),
         request_monitors: Map.put(state.request_monitors, monitor_ref, request_id)
     }}
  end

  # A caller only gets the operation's own reply while it is still waiting for
  # one: a dead owner is disconnected and a passed deadline is a timeout.
  defp expired_reason(request) do
    cond do
      not Process.alive?(request.owner) -> :disconnected
      not before_deadline?(request.deadline) -> :timeout
      true -> nil
    end
  end

  defp effective_reply(request, reply) do
    case expired_reason(request) do
      nil -> reply
      reason -> {:error, reason}
    end
  end

  defp reply_request_if_live(state, request_id, reply) do
    case Map.get(state.requests, request_id) do
      nil -> state
      request -> reply_request(state, request_id, effective_reply(request, reply))
    end
  end

  defp accept_add_request(state, request_id) do
    case Map.get(state.requests, request_id) do
      nil ->
        {:expired, state}

      request ->
        case expired_reason(request) do
          nil -> {:ok, request, state}
          reason -> {:expired, reply_request(state, request_id, {:error, reason})}
        end
    end
  end

  defp reply_request_if_present(state, nil, _reply), do: state

  defp reply_request_if_present(state, request_id, reply),
    do: reply_request(state, request_id, reply)

  defp reply_request(state, request_id, reply) do
    case Map.pop(state.requests, request_id) do
      {nil, _requests} ->
        state

      {%{from: from, timer: timer, monitor: monitor_ref}, requests} ->
        _ = Process.cancel_timer(timer, async: true, info: false)
        Process.demonitor(monitor_ref, [:flush])

        state = %{
          state
          | requests: requests,
            request_monitors: Map.delete(state.request_monitors, monitor_ref)
        }

        GenServer.reply(from, reply)
        state
    end
  end

  defp request_key(state, request_id) do
    case Map.get(state.requests, request_id) do
      %{key: key} -> key
      nil -> nil
    end
  end

  defp ensure_rule(state, key, rule) do
    if Map.has_key?(state.rules, key),
      do: state,
      else: put_rule(state, key, new_rule(rule, :installing))
  end

  defp new_rule(rule, status), do: %Rule{rule: rule, status: status}

  defp enqueue_request(state, key, request_id) do
    update_rule(state, key, fn rule -> %{rule | queue: rule.queue ++ [request_id]} end)
  end

  defp queue_full?(state, key) do
    case Map.get(state.rules, key) do
      nil -> false
      rule -> length(rule.queue) >= @max_queued_requests
    end
  end

  defp sender_routing_ambiguous?(state, key, candidate) do
    Enum.any?(state.rules, fn {existing_key, %Rule{rule: existing}} ->
      existing_key != key and Overlap.sender_routing_ambiguous?(candidate, existing)
    end)
  end

  defp next_live_request(queue, requests) do
    case Enum.split_while(queue, fn request_id -> not Map.has_key?(requests, request_id) end) do
      {_expired, [request_id | remaining]} -> {request_id, remaining}
      {_expired, []} -> {nil, []}
    end
  end

  defp has_live_add?(queue, requests) do
    Enum.any?(queue, fn request_id ->
      match?(%{kind: :add}, Map.get(requests, request_id))
    end)
  end

  defp put_active_rule(state, key) do
    update_rule(state, key, fn rule -> %{rule | status: :active, remote_may_exist?: true} end)
  end

  defp put_remote_may_exist(state, key, remote_may_exist?) do
    update_rule(state, key, fn rule -> %{rule | remote_may_exist?: remote_may_exist?} end)
  end

  defp put_pending_handler(state, key, handler_ref) do
    update_rule(state, key, fn rule ->
      %{rule | pending_handlers: MapSet.put(rule.pending_handlers, handler_ref)}
    end)
  end

  defp maybe_put_pending_handler(state, _key, nil), do: state

  defp maybe_put_pending_handler(state, key, handler_ref),
    do: put_pending_handler(state, key, handler_ref)

  defp put_pending_handlers(state, key, handler_refs) do
    update_rule(state, key, fn rule -> %{rule | pending_handlers: handler_refs} end)
  end

  defp put_subscription(state, owner, key, handler_ref) do
    monitor_ref = Process.monitor(owner)

    state =
      update_rule(state, key, fn rule -> %{rule | refs: MapSet.put(rule.refs, handler_ref)} end)

    %{
      state
      | subscriptions:
          Map.put(state.subscriptions, handler_ref, %{owner: owner, key: key, handler: :active}),
        owner_monitors: Map.put(state.owner_monitors, monitor_ref, handler_ref),
        ref_monitors: Map.put(state.ref_monitors, handler_ref, monitor_ref)
    }
  end

  defp mark_handler_removed(state, ref) do
    # Owner DOWN may have dropped the subscription while its final RemoveMatch
    # operation was still in flight.  Its late result still drives rule cleanup,
    # but must never dereference a now-missing local subscription.
    case Map.get(state.subscriptions, ref) do
      nil ->
        state

      %{handler: :active} ->
        put_in(state.subscriptions[ref].handler, :removed)

      %{handler: :removed} ->
        state
    end
  end

  defp drop_subscription(state, ref) do
    case Map.pop(state.subscriptions, ref) do
      {nil, _subscriptions} ->
        state

      {%{key: key}, subscriptions} ->
        {monitor_ref, ref_monitors} = Map.pop(state.ref_monitors, ref)
        if monitor_ref, do: Process.demonitor(monitor_ref, [:flush])

        state = %{
          state
          | subscriptions: subscriptions,
            owner_monitors: Map.delete(state.owner_monitors, monitor_ref),
            ref_monitors: ref_monitors
        }

        update_rule(state, key, fn rule -> %{rule | refs: MapSet.delete(rule.refs, ref)} end)
    end
  end

  defp put_rule(state, key, rule), do: %{state | rules: Map.put(state.rules, key, rule)}

  defp delete_rule(state, key), do: %{state | rules: Map.delete(state.rules, key)}

  defp update_rule(state, key, fun) do
    case Map.get(state.rules, key) do
      nil -> state
      rule -> put_rule(state, key, fun.(rule))
    end
  end

  defp max_recovering_rules do
    Application.get_env(:rebus, :match_recovery_max_rules, 64)
  end

  # Consecutive recovery attempts allowed for a single rule before it is left
  # uncertain. With `@recovery_delays` the default is roughly 26
  # seconds of retrying per recovery episode: long enough that a transient bus
  # hiccup never trips it, short enough that an operator learns of a stuck rule
  # within a minute. Attempts rather than wall-clock keeps it deterministic. A
  # successful recovery, and each new episode entered through
  # `enter_recovery/3`, resets the count to zero.
  defp max_recovery_attempts do
    Application.get_env(:rebus, :match_recovery_max_attempts, 30)
  end

  # Retries allowed for one failed owner-tracking sequence before the name is
  # left untracked for good. Shares `@recovery_delays` with rule recovery, so
  # the default is the same rough half-minute of retrying: long enough to ride
  # out a slow bus or a momentary AccessDenied, short enough that a name whose
  # tracking is genuinely refused stops asking.
  defp max_tracking_attempts do
    Application.get_env(:rebus, :match_tracking_max_attempts, 30)
  end

  defp retry_delay(attempt) do
    Enum.at(@recovery_delays, min(attempt - 1, length(@recovery_delays) - 1))
  end

  defp before_deadline?(deadline) when is_integer(deadline) do
    deadline > System.monotonic_time(:millisecond)
  end

  defp log_recovery(event) do
    Logger.debug("D-Bus match cleanup transition=#{event}")
  end

  # Individual retries stay at debug, but a rule that has exhausted the backoff
  # is stuck on the bus and an operator should see it well before the budget
  # runs out and the rule is left uncertain. The equality fires exactly once per
  # recovery episode, since a new episode restarts the count at zero. The rule
  # itself is not named: it can carry caller-supplied arguments.
  defp maybe_log_recovery_saturated(attempt) do
    if attempt == length(@recovery_delays) do
      Logger.warning(
        "D-Bus match recovery still retrying attempt=#{attempt} " <>
          "delay_ms=#{retry_delay(attempt)}"
      )
    end

    :ok
  end

  # A well-known bus name is not a secret, and naming it and the failed step is
  # what makes a silently degraded subscription diagnosable. Neither the unique
  # name behind it nor any message payload is logged.
  defp log_tracking(_name, :ok), do: :ok

  defp log_tracking(name, {:error, {step, _reason}}) do
    Logger.warning("D-Bus name owner tracking failed name=#{name} step=#{step}")
  end

  defp log_tracking(name, _result) do
    Logger.warning("D-Bus name owner tracking failed name=#{name} step=unknown")
  end

  # Each attempt logs its own warning; this says the retrying has stopped, at a
  # level that separates a name degraded for good from one still being retried.
  # It fires once per failure episode, since a success clears the count.
  defp log_tracking_exhausted(name, attempts) do
    Logger.error("D-Bus name owner tracking exhausted name=#{name} attempts=#{attempts}")
  end

  # The operation type is what makes a worker that cannot start tasks
  # diagnosable. The rule it was for is not named: a match rule can carry
  # caller-supplied arguments.
  defp log_not_started(type) do
    Logger.warning("D-Bus match operation not started type=#{type}")
  end

  # Fires once per rule, since an uncertain rule never leaves that state. As
  # elsewhere, the rule is not named: it can carry caller-supplied arguments.
  defp log_uncertain(event) do
    Logger.warning("D-Bus match rule left uncertain transition=#{event}")
  end
end
