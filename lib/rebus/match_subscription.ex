defmodule Rebus.MatchSubscription do
  @moduledoc false
  use DynamicSupervisor

  alias Rebus.MatchRule
  alias Rebus.MatchSubscription.Worker

  # The caller's timeout is one budget for the whole operation: installing or
  # removing the local handler, and the AddMatch or RemoveMatch reply. The
  # worker is given a slightly longer bound so its own deadline reply wins the
  # race against the caller giving up.
  @call_overhead 100
  @state_table Rebus.MatchSubscription.State

  def start_link(_args) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    if :ets.whereis(@state_table) == :undefined do
      _ =
        :ets.new(@state_table, [
          :named_table,
          :set,
          :public,
          read_concurrency: true,
          write_concurrency: true
        ])
    end

    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @doc false
  def load_state(conn) when is_pid(conn) do
    case :ets.lookup(@state_table, {:meta, conn}) do
      [{{:meta, ^conn}, %{uncertain?: uncertain?}}] ->
        {:ok,
         %{
           uncertain?: uncertain?,
           rules: persisted_rows(conn, :rule),
           subscriptions: persisted_rows(conn, :subscription)
         }}

      [] ->
        :error
    end
  end

  @doc false
  def persisted?(conn) when is_pid(conn), do: :ets.member(@state_table, {:meta, conn})

  @doc false
  def persist_state(conn, uncertain?, changes, rules, subscriptions)
      when is_pid(conn) and is_boolean(uncertain?) and is_map(changes) and is_map(rules) and
             is_map(subscriptions) do
    persist_rows(conn, :rule, changes.dirty_rules, changes.removed_rules, rules)

    persist_rows(
      conn,
      :subscription,
      changes.dirty_subscriptions,
      changes.removed_subscriptions,
      subscriptions
    )

    true = :ets.insert(@state_table, {{:meta, conn}, %{uncertain?: uncertain?}})
    :ok
  end

  @doc false
  def delete_state(conn) when is_pid(conn) do
    true = :ets.delete(@state_table, {:meta, conn})
    true = :ets.match_delete(@state_table, {{:rule, conn, :_}, :_})
    true = :ets.match_delete(@state_table, {{:subscription, conn, :_}, :_})
    :ok
  end

  defp persisted_rows(conn, kind) do
    :ets.match_object(@state_table, {{kind, conn, :_}, :_})
    |> Map.new(fn {{^kind, ^conn, key}, value} -> {key, value} end)
  end

  defp persist_rows(conn, kind, dirty, removed, values) do
    Enum.each(removed, fn key ->
      true = :ets.delete(@state_table, {kind, conn, key})
    end)

    Enum.each(dirty, fn key ->
      case Map.fetch(values, key) do
        {:ok, value} -> true = :ets.insert(@state_table, {{kind, conn, key}, value})
        :error -> true = :ets.delete(@state_table, {kind, conn, key})
      end
    end)
  end

  @spec add(pid(), MatchRule.t(), non_neg_integer()) ::
          {:ok, reference()} | {:error, term()}
  def add(conn, %MatchRule{} = rule, timeout)
      when is_pid(conn) and is_integer(timeout) and timeout >= 0 do
    if node(conn) == node() do
      deadline = System.monotonic_time(:millisecond) + timeout

      call_add(conn, rule, deadline)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  defp call_add(conn, rule, deadline) do
    with {:ok, worker} <- worker_for(conn) do
      Worker.call(worker, {:add, self(), rule, deadline}, deadline, @call_overhead)
    end
  catch
    :exit, _reason -> {:error, :disconnected}
  end

  @spec remove(pid(), reference(), non_neg_integer()) :: :ok | {:error, term()}
  def remove(conn, ref, timeout)
      when is_pid(conn) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    if node(conn) == node() do
      deadline = System.monotonic_time(:millisecond) + timeout

      call_remove(conn, ref, deadline)
    else
      {:error, :remote_connection_unsupported}
    end
  end

  defp call_remove(conn, ref, deadline) do
    case Registry.lookup(Rebus.MatchSubscription.Registry, conn) do
      [{worker, _value}] ->
        Worker.call(worker, {:remove, ref, deadline}, deadline, @call_overhead)

      [] ->
        if persisted?(conn) do
          with {:ok, worker} <- worker_for(conn) do
            Worker.call(worker, {:remove, ref, deadline}, deadline, @call_overhead)
          end
        else
          # References are idempotent and scoped to their original connection.
          :ok
        end
    end
  catch
    :exit, _reason -> {:error, :disconnected}
  end

  defp worker_for(conn) do
    case Registry.lookup(Rebus.MatchSubscription.Registry, conn) do
      [{worker, _value}] ->
        {:ok, worker}

      [] ->
        case DynamicSupervisor.start_child(__MODULE__, {Worker, conn}) do
          {:ok, worker} -> {:ok, worker}
          {:error, {:already_started, worker}} -> {:ok, worker}
          {:error, _reason} -> {:error, :disconnected}
        end
    end
  end
end

defmodule Rebus.MatchSubscription.Worker do
  @moduledoc false
  use GenServer

  require Logger

  alias Rebus.Connection
  alias Rebus.MatchRule
  alias Rebus.MatchSubscription
  alias Rebus.Message
  alias Rebus.UnixFD

  @cleanup_timeout 1_000
  @match_rule_not_found "org.freedesktop.DBus.Error.MatchRuleNotFound"
  @max_queued_requests 64
  @max_initial_cleanups 16
  @recovery_delays [50, 100, 200, 400, 800, 1_000]

  def child_spec(conn) do
    %{
      id: {__MODULE__, conn},
      start: {__MODULE__, :start_link, [conn]},
      restart: :transient,
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
    case remaining_timeout(deadline) do
      {:ok, timeout} when timeout > 0 ->
        safe_worker_call(worker, request, timeout + overhead)

      _expired ->
        {:error, :timeout}
    end
  end

  defp safe_worker_call(worker, request, timeout) do
    GenServer.call(worker, request, timeout)
  catch
    :exit, {:timeout, _call} -> {:error, :timeout}
    :exit, _reason -> {:error, :disconnected}
  end

  @impl true
  def init(conn) do
    state = %{
      conn: conn,
      connection_monitor: Process.monitor(conn),
      subscriptions: %{},
      rules: %{},
      owner_monitors: %{},
      ref_monitors: %{},
      requests: %{},
      request_monitors: %{},
      request_owner_index: %{},
      operations: %{},
      operation_monitors: %{},
      recovering_rules: MapSet.new(),
      initial_cleanup_keys: MapSet.new(),
      initial_cleanup_queue: :queue.new(),
      resetting?: false,
      reset_attempt: 0,
      reset_timer: nil,
      reset_token: nil,
      reset_task_monitor: nil,
      state_lost?: false,
      bus?: nil,
      persistence: empty_persistence_changes()
    }

    case MatchSubscription.load_state(conn) do
      {:ok, %{uncertain?: true}} ->
        send(self(), :reset_state_lost)
        {:ok, %{state | state_lost?: true}}

      {:ok, persisted} ->
        state = restore_state(state, persisted)
        Enum.each(state.recovering_rules, &send(self(), {:resume_recovery, &1}))
        {:ok, state}

      :error ->
        {:ok, state}
    end
  end

  @impl true
  def handle_call({:add, _owner, _rule, _deadline}, _from, %{state_lost?: true} = state) do
    {:reply, {:error, :match_subscription_state_lost}, state}
  end

  def handle_call({:add, owner, rule, deadline}, from, state) do
    key = MatchRule.to_string(rule)

    cond do
      not before_deadline?(deadline) ->
        {:reply, {:error, :timeout}, state}

      queue_full?(state, key) ->
        {:reply, {:error, :match_rule_cleanup_pending}, state}

      sender_routing_ambiguous?(state, key, rule) ->
        {:reply, {:error, :sender_routing_ambiguous}, state}

      true ->
        add_subscription(state, from, owner, rule, key, deadline)
    end
  end

  def handle_call({:remove, _ref, _deadline}, _from, %{state_lost?: true} = state) do
    {:reply, {:error, :match_subscription_state_lost}, state}
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
  defp ensure_bus(%{bus?: true} = state, _deadline), do: {:ok, state}
  defp ensure_bus(%{bus?: false} = state, _deadline), do: {:error, :not_a_bus, state}

  defp ensure_bus(state, deadline) do
    case remaining_timeout(deadline) do
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

  @impl true
  def handle_info(:reset_state_lost, state) do
    # An operation was in-flight when this worker died.  Neither the local
    # handler nor the bus rule can be reconstructed safely, so reject new
    # public operations while a bounded connection reset clears both.
    {:noreply, state |> request_connection_reset(nil) |> persist_state()}
  end

  def handle_info({:resume_recovery, key}, state) do
    {:noreply, start_recovery_attempt(state, key)}
  end

  def handle_info({:connection_reset_result, token, :ok}, %{reset_token: token} = state) do
    state = clear_reset_task(state)
    log_reset(:close_requested)
    # Supervisor termination is asynchronous relative to this monitor. Keep
    # both the reset latch and the stable snapshot until connection DOWN proves
    # that the bus and local handlers have actually exited.
    state = %{state | state_lost?: true}
    {:noreply, persist_state(state)}
  end

  def handle_info(
        {:connection_reset_result, token, {:error, _reason}},
        %{reset_token: token} = state
      ) do
    state = clear_reset_task(state)
    log_reset(:close_not_started)
    # Do not silently leave every rule behind a reset latch when this is a
    # direct/non-supervised PID. The unresolved state is explicit to callers.
    state = %{state | resetting?: false, state_lost?: true}
    {:noreply, persist_state(state)}
  end

  def handle_info({:connection_reset_result, _token, _result}, state), do: {:noreply, state}

  def handle_info({:request_timeout, request_id}, state) do
    case Map.fetch(state.requests, request_id) do
      {:ok, %{key: key}} ->
        state = reply_request(state, request_id, {:error, :timeout})
        {:noreply, state |> maybe_progress_rule(key) |> persist_state()}

      :error ->
        {:noreply, state}
    end
  end

  def handle_info({:retry_recovery, key}, state) do
    case Map.get(state.rules, key) do
      %{status: :recovering} = rule ->
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
        {:noreply, state |> complete_operation(key, operation, result) |> persist_state()}
    end
  end

  def handle_info({:DOWN, monitor_ref, :process, _pid, _reason}, state) do
    cond do
      monitor_ref == state.reset_task_monitor ->
        # The reset task died before reporting whether it closed the
        # connection. Do not leave a permanent reset latch or guess that the
        # registration disappeared; expose explicit state loss instead.
        log_reset(:task_lost)

        state =
          state
          |> clear_reset_task()
          |> Map.put(:resetting?, false)
          |> Map.put(:state_lost?, true)

        {:noreply, persist_state(state)}

      ref = Map.get(state.owner_monitors, monitor_ref) ->
        {:noreply, state |> owner_down(monitor_ref, ref) |> persist_state()}

      request_id = Map.get(state.request_monitors, monitor_ref) ->
        key = request_key(state, request_id)
        state = reply_request(state, request_id, {:error, :disconnected})
        state = if(key, do: maybe_progress_rule(state, key), else: state)
        {:noreply, persist_state(state)}

      token = Map.get(state.operation_monitors, monitor_ref) ->
        # A task that finishes normally sends its result first. Reaching this
        # branch means an operation died before reporting a safe outcome, so
        # reset the connection rather than guessing about a handler or
        # bus-rule state.
        case take_operation(state, token) do
          {nil, state} ->
            {:noreply, state}

          {%{key: key, request_id: request_id}, state} ->
            state = reply_request_if_present(state, request_id, {:error, :disconnected})
            log_recovery(:operation_lost)
            {:noreply, state |> request_connection_reset(key) |> persist_state()}
        end

      monitor_ref == state.connection_monitor ->
        # A disconnected bus drops all server-side match state. Do not try to
        # send RemoveMatch through a dead transport.
        :ok = MatchSubscription.delete_state(state.conn)
        {:stop, :normal, state}

      true ->
        {:noreply, state}
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

      %{status: :recovering} ->
        state

      %{status: :cleaning} ->
        state

      %{operation: operation} when not is_nil(operation) ->
        state

      rule ->
        {request_id, queue} = next_live_request(rule.queue, state.requests)
        state = put_rule(state, key, %{rule | queue: queue})

        case request_id && Map.get(state.requests, request_id) do
          nil ->
            maybe_idle_rule(state, key)

          %{kind: :add} = request when rule.status == :installing ->
            start_add_new(state, key, request_id, request)

          %{kind: :add} = request when rule.status == :active ->
            start_add_existing(state, key, request_id, request)

          %{kind: :remove, ref: ref} = request when rule.status == :active ->
            case Map.fetch(state.subscriptions, ref) do
              {:ok, subscription} -> start_remove(state, key, request_id, request, subscription)
              :error -> dispatch_rule(reply_request(state, request_id, :ok), key)
            end

          %{kind: :remove} ->
            dispatch_rule(reply_request(state, request_id, :ok), key)
        end
    end
  end

  defp start_add_new(state, key, request_id, request) do
    rule = Map.fetch!(state.rules, key).rule
    conn = state.conn
    owner = request.owner
    deadline = request.deadline

    start_operation(state, key, :add_new, request_id, fn ->
      add_new_operation(conn, owner, rule, deadline)
    end)
  end

  defp start_add_existing(state, key, request_id, request) do
    rule = Map.fetch!(state.rules, key).rule
    conn = state.conn
    owner = request.owner
    deadline = request.deadline

    start_operation(state, key, :add_existing, request_id, fn ->
      add_existing_operation(conn, owner, rule, deadline)
    end)
  end

  defp start_remove(state, key, request_id, request, subscription) do
    rule = Map.fetch!(state.rules, key)
    final? = MapSet.size(rule.refs) == 1
    conn = state.conn
    canonical_rule = rule.rule
    ref = request.ref
    handler = subscription.handler
    deadline = request.deadline

    start_operation(state, key, :remove, request_id, fn ->
      remove_operation(
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
      %{status: status, operation: nil, retry_timer: nil} = rule
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
          recover_operation(conn, recovery, deadline)
        end)

      _ ->
        state
    end
  end

  defp start_operation(state, key, type, request_id, fun) do
    token = make_ref()
    worker = self()

    case Task.Supervisor.start_child(Rebus.MatchSubscription.TaskSupervisor, fn ->
           send(worker, {:operation_result, token, fun.()})
         end) do
      {:ok, pid} ->
        monitor_ref = Process.monitor(pid)
        rule = Map.fetch!(state.rules, key)
        state = put_rule(state, key, %{rule | operation: token})

        %{
          state
          | operations:
              Map.put(state.operations, token, %{
                key: key,
                type: type,
                request_id: request_id,
                monitor: monitor_ref
              }),
            operation_monitors: Map.put(state.operation_monitors, monitor_ref, token)
        }
        |> persist_state()

      {:error, _reason} ->
        rule = Map.fetch!(state.rules, key)
        state = put_rule(state, key, %{rule | operation: token})

        state = %{
          state
          | operations:
              Map.put(state.operations, token, %{
                key: key,
                type: type,
                request_id: request_id,
                monitor: nil
              })
        }

        send(self(), {:operation_result, token, {:operation_failed, :disconnected}})
        persist_state(state)
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
        reply_request_after_persist(state, request_id, {:ok, handler_ref})

      {:expired, state} ->
        state
        |> put_pending_handler(key, handler_ref)
        |> put_remote_may_exist(key, true)
        |> enter_recovery(key, :rule)
    end
  end

  defp complete_add_new(state, key, request_id, {:add_failed, error, handler_ref}) do
    state = reply_request_if_live(state, request_id, error)
    remote_may_exist? = not definitive_bus_error?(error)

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

  defp complete_add_new(state, key, request_id, {:operation_failed, _reason}) do
    state = reply_request_if_present(state, request_id, {:error, :disconnected})
    request_connection_reset(state, key)
  end

  defp complete_add_existing(state, key, request_id, {:added_existing, handler_ref}) do
    case accept_add_request(state, request_id) do
      {:ok, request, state} ->
        state = put_subscription(state, request.owner, key, handler_ref)
        state = dispatch_rule(state, key)
        reply_request_after_persist(state, request_id, {:ok, handler_ref})

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

  defp complete_add_existing(state, key, request_id, {:operation_failed, _reason}) do
    state = reply_request_if_present(state, request_id, {:error, :disconnected})
    request_connection_reset(state, key)
  end

  defp complete_remove(state, key, request_id, {:remove_failed, error, :active}) do
    state
    |> reply_request_if_live(request_id, error)
    |> dispatch_rule(key)
  end

  defp complete_remove(state, key, request_id, {:removed, ref, :nonfinal}) do
    state = drop_subscription(state, ref)
    state = dispatch_rule(state, key)
    reply_request_if_live_after_persist(state, request_id, :ok)
  end

  defp complete_remove(state, key, request_id, {:removed, ref, :final}) do
    state = drop_subscription(state, ref)
    state = clear_rule_and_resume(state, key)
    reply_request_if_live_after_persist(state, request_id, :ok)
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

  defp complete_remove(state, key, request_id, {:operation_failed, _reason}) do
    state = reply_request_if_present(state, request_id, {:error, :disconnected})
    request_connection_reset(state, key)
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

  defp complete_recovery(state, key, {:operation_failed, _reason}) do
    log_recovery(:operation_lost)
    request_connection_reset(state, key)
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

  defp complete_initial_cleanup(state, key, {:operation_failed, _reason}) do
    log_recovery(:operation_lost)
    request_connection_reset(state, key)
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
        state = mark_subscription_removed(state, ref)

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

      %{status: :recovering} ->
        state

      %{operation: operation} when not is_nil(operation) ->
        state

      %{status: :active} = rule ->
        if MapSet.size(rule.refs) == 0 and not has_live_add?(rule.queue, state.requests) do
          start_initial_cleanup(state, key)
        else
          state
        end

      %{status: :installing, queue: []} ->
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
      %{status: :active, operation: nil} = rule ->
        rule = %{
          rule
          | status: :cleaning,
            recovery_kind: :rule,
            recovery_attempt: 0,
            retry_timer: nil
        }

        state = put_rule(state, key, rule)

        if MapSet.size(state.initial_cleanup_keys) < @max_initial_cleanups do
          start_recovery_attempt(state, key)
        else
          %{state | initial_cleanup_queue: :queue.in(key, state.initial_cleanup_queue)}
        end

      _ ->
        state
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
          state = %{state | initial_cleanup_queue: queue}

          case Map.get(state.rules, key) do
            %{status: :cleaning, operation: nil} ->
              state
              |> start_recovery_attempt(key)
              |> start_next_initial_cleanup()

            _ ->
              start_next_initial_cleanup(state)
          end

        {:empty, _queue} ->
          state
      end
    end
  end

  defp enter_recovery(state, key, kind) when kind in [:rule, :handlers] do
    case Map.get(state.rules, key) do
      nil ->
        state

      %{status: :recovering} ->
        state

      %{operation: operation} when not is_nil(operation) ->
        state

      rule ->
        if MapSet.size(state.recovering_rules) >= max_recovering_rules() do
          log_recovery(:capacity_exhausted)
          request_connection_reset(state, key)
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

  defp schedule_recovery_retry(state, key) do
    case Map.get(state.rules, key) do
      %{status: :recovering, retry_timer: nil} = rule ->
        attempt = rule.recovery_attempt + 1
        timer = Process.send_after(self(), {:retry_recovery, key}, retry_delay(attempt))
        put_rule(state, key, %{rule | recovery_attempt: attempt, retry_timer: timer})

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

        {add_requests, state} =
          Enum.reduce(queue, {[], state}, fn request_id, {adds, acc} ->
            case Map.get(acc.requests, request_id) do
              %{kind: :add} -> {[request_id | adds], acc}
              %{kind: :remove} -> {adds, reply_request(acc, request_id, :ok)}
              nil -> {adds, acc}
            end
          end)

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

  defp request_connection_reset(state, _key) do
    # Never include a rule or signal payload in observability. A connection
    # reset is bounded and makes the bus discard all match state for it. Its
    # result is observed so a failed task or non-supervised PID cannot leave a
    # permanent `resetting?` latch.
    if state.resetting? do
      state
    else
      Logger.warning("D-Bus match cleanup closing connection")
      token = make_ref()
      worker = self()
      conn = state.conn

      case Task.Supervisor.start_child(Rebus.MatchSubscription.TaskSupervisor, fn ->
             result = Rebus.close(conn)

             send(worker, {:connection_reset_result, token, result})
           end) do
        {:ok, pid} ->
          %{
            state
            | resetting?: true,
              state_lost?: true,
              reset_token: token,
              reset_task_monitor: Process.monitor(pid)
          }

        {:error, _reason} ->
          # No task ran, therefore no reset outcome is known. Keep existing
          # connection authority intact and make the lost state explicit.
          log_reset(:task_start_failed)
          %{state | state_lost?: true}
      end
    end
  end

  defp clear_reset_task(state) do
    if is_reference(state.reset_task_monitor),
      do: Process.demonitor(state.reset_task_monitor, [:flush])

    %{state | reset_task_monitor: nil, reset_token: nil}
  end

  defp add_new_operation(conn, owner, rule, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline),
         {:ok, handler_ref} <- install_local_handler(conn, owner, rule, timeout) do
      case invoke_bus_method(conn, "AddMatch", rule, deadline) do
        :ok -> {:added, handler_ref}
        {:error, _reason} = error -> {:add_failed, error, handler_ref}
      end
    else
      {:error, _reason} = error -> {:add_failed, error, nil}
    end
  end

  defp add_existing_operation(conn, owner, rule, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline),
         {:ok, handler_ref} <- install_local_handler(conn, owner, rule, timeout) do
      {:added_existing, handler_ref}
    else
      {:error, _reason} = error -> {:add_existing_failed, error}
    end
  end

  defp remove_operation(conn, rule, ref, handler, final?, deadline) do
    case ensure_handler_removed(conn, ref, handler, deadline) do
      :ok ->
        if final? do
          case invoke_bus_method(conn, "RemoveMatch", rule, deadline) do
            :ok ->
              {:removed, ref, :final}

            error ->
              cond do
                match_rule_not_found?(error) -> {:removed, ref, :final}
                definitive_bus_error?(error) -> {:remove_definitive_error, ref, error}
                true -> {:remove_ambiguous, ref, error}
              end
          end
        else
          {:removed, ref, :nonfinal}
        end

      {:error, _reason} = error ->
        {:remove_failed, error, :active}
    end
  end

  defp recover_operation(conn, rule, deadline) do
    case remove_pending_handlers(conn, rule.pending_handlers, deadline) do
      :ok ->
        case rule.recovery_kind do
          :handlers ->
            :handlers_cleared

          :rule ->
            case remove_remote_rule(conn, rule, deadline) do
              :ok -> :cleared
              {:definitive, error} -> {:definitive_bus_error, error}
              :ambiguous -> {:retry, :remote}
            end
        end

      {:error, _reason} ->
        {:retry, :handlers}
    end
  end

  defp remove_pending_handlers(conn, handler_refs, deadline) do
    Enum.reduce_while(handler_refs, :ok, fn handler_ref, :ok ->
      case delete_local_handler(conn, handler_ref, deadline) do
        :ok -> {:cont, :ok}
        {:error, _reason} = error -> {:halt, error}
      end
    end)
  end

  defp remove_remote_rule(_conn, %{remote_may_exist?: false}, _deadline), do: :ok

  defp remove_remote_rule(conn, %{rule: rule}, deadline) do
    case invoke_bus_method(conn, "RemoveMatch", rule, deadline) do
      :ok ->
        :ok

      error ->
        cond do
          match_rule_not_found?(error) -> :ok
          definitive_bus_error?(error) -> {:definitive, error}
          true -> :ambiguous
        end
    end
  end

  defp ensure_handler_removed(_conn, _ref, :removed, _deadline), do: :ok

  defp ensure_handler_removed(conn, ref, :active, deadline),
    do: delete_local_handler(conn, ref, deadline)

  defp install_local_handler(conn, owner, rule, timeout) do
    handler_ref = make_ref()

    case Connection.add_signal_handler(conn, owner, handler_ref, rule, timeout) do
      {:ok, ^handler_ref} = ok -> ok
      {:error, _reason} = error -> error
    end
  end

  defp delete_local_handler(conn, handler_ref, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline) do
      Connection.delete_signal_handler(conn, handler_ref, timeout)
    end
  end

  defp invoke_bus_method(conn, member, rule, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline) do
      # A D-Bus error reply now arrives as {:error, %Message{}}. Both reply
      # shapes must reach bus_reply_result/1 so any received descriptors are
      # closed before the reason is classified.
      case Rebus.call(conn, bus_message(member, rule), timeout) do
        {:ok, %Message{} = reply} -> bus_reply_result(reply)
        {:error, %Message{} = reply} -> bus_reply_result(reply)
        {:error, _reason} = error -> error
      end
    end
  end

  defp bus_message(member, rule) do
    Message.new!(:method_call,
      path: "/org/freedesktop/DBus",
      interface: "org.freedesktop.DBus",
      destination: "org.freedesktop.DBus",
      member: member,
      signature: "s",
      body: [MatchRule.to_string(rule)]
    )
  end

  # These internal method calls never expose a D-Bus reply to application
  # code. Retain neither its body nor any received descriptors; in particular,
  # a malicious AddMatch/RemoveMatch reply must not leak Unix FD ownership into
  # the worker process.
  defp bus_reply_result(%Message{} = reply) do
    _ = UnixFD.close_all(reply.unix_fds)

    case reply do
      %Message{type: :method_return} ->
        :ok

      %Message{type: :error, header_fields: %{error_name: error_name}}
      when is_binary(error_name) ->
        {:error, {:bus_error, :binary.copy(error_name)}}

      _ ->
        {:error, :invalid_bus_reply}
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
         request_monitors: Map.put(state.request_monitors, monitor_ref, request_id),
         request_owner_index: Map.put(state.request_owner_index, request_id, monitor_ref)
     }}
  end

  defp reply_request_if_live(state, request_id, reply) do
    case Map.get(state.requests, request_id) do
      nil ->
        state

      request ->
        if before_deadline?(request.deadline) and Process.alive?(request.owner) do
          reply_request(state, request_id, reply)
        else
          reply_request(
            state,
            request_id,
            {:error, if(Process.alive?(request.owner), do: :timeout, else: :disconnected)}
          )
        end
    end
  end

  defp reply_request_if_live_after_persist(state, request_id, reply) do
    case Map.get(state.requests, request_id) do
      nil ->
        state

      request ->
        if before_deadline?(request.deadline) and Process.alive?(request.owner) do
          reply_request_after_persist(state, request_id, reply)
        else
          reply_request_after_persist(
            state,
            request_id,
            {:error, if(Process.alive?(request.owner), do: :timeout, else: :disconnected)}
          )
        end
    end
  end

  defp accept_add_request(state, request_id) do
    case Map.get(state.requests, request_id) do
      nil ->
        {:expired, state}

      request ->
        if before_deadline?(request.deadline) and Process.alive?(request.owner) do
          {:ok, request, state}
        else
          state =
            reply_request(
              state,
              request_id,
              {:error, if(Process.alive?(request.owner), do: :timeout, else: :disconnected)}
            )

          {:expired, state}
        end
    end
  end

  defp reply_request_if_present(state, nil, _reply), do: state

  defp reply_request_if_present(state, request_id, reply),
    do: reply_request(state, request_id, reply)

  defp reply_request_after_persist(state, request_id, reply) do
    case Map.pop(state.requests, request_id) do
      {nil, _requests} ->
        state

      {%{from: from, timer: timer, monitor: monitor_ref}, requests} ->
        _ = Process.cancel_timer(timer, async: true, info: false)
        Process.demonitor(monitor_ref, [:flush])

        state = %{
          state
          | requests: requests,
            request_monitors: Map.delete(state.request_monitors, monitor_ref),
            request_owner_index: Map.delete(state.request_owner_index, request_id)
        }

        # A completed operation may be observed immediately by its caller,
        # including by stopping this worker. Persist its final state before
        # replying so restart recovery never mistakes an acknowledged stable
        # subscription for an uncertain in-flight operation.
        state = persist_state(state)
        GenServer.reply(from, reply)
        state
    end
  end

  defp reply_request(state, request_id, reply) do
    case Map.pop(state.requests, request_id) do
      {nil, _requests} ->
        state

      {%{from: from, timer: timer, monitor: monitor_ref}, requests} ->
        _ = Process.cancel_timer(timer, async: true, info: false)
        Process.demonitor(monitor_ref, [:flush])
        GenServer.reply(from, reply)

        %{
          state
          | requests: requests,
            request_monitors: Map.delete(state.request_monitors, monitor_ref),
            request_owner_index: Map.delete(state.request_owner_index, request_id)
        }
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

  defp new_rule(rule, status) do
    %{
      rule: rule,
      refs: MapSet.new(),
      pending_handlers: MapSet.new(),
      remote_may_exist?: false,
      status: status,
      operation: nil,
      queue: [],
      recovery_kind: nil,
      recovery_attempt: 0,
      retry_timer: nil
    }
  end

  defp enqueue_request(state, key, request_id) do
    update_rule(state, key, fn rule -> %{rule | queue: rule.queue ++ [request_id]} end)
  end

  defp queue_full?(state, key) do
    case Map.get(state.rules, key) do
      nil -> false
      rule -> length(rule.queue) >= @max_queued_requests
    end
  end

  # D-Bus does not tag an incoming signal with the AddMatch rule that admitted
  # it. A well-known `sender=` is deliberately bus-owned (the forwarded header
  # normally contains the owner's unique name), so it cannot safely be checked
  # locally when another overlapping rule could also have admitted the signal.
  # Refuse that ambiguous combination rather than cross-delivering a signal to
  # a sender-pinned handler. Equal well-known senders are safe: both server
  # rules enforce the same sender ownership predicate.
  defp sender_routing_ambiguous?(state, key, candidate) do
    Enum.any?(state.rules, fn {existing_key, %{rule: existing}} ->
      existing_key != key and
        (well_known_sender?(candidate) or well_known_sender?(existing)) and
        not same_well_known_sender?(candidate, existing) and
        rules_may_overlap?(candidate, existing)
    end)
  end

  defp well_known_sender?(%MatchRule{criteria: %{sender: sender}}),
    do: not String.starts_with?(sender, ":")

  defp well_known_sender?(%MatchRule{}), do: false

  defp same_well_known_sender?(
         %MatchRule{criteria: %{sender: sender}},
         %MatchRule{criteria: %{sender: sender}}
       ),
       do: not String.starts_with?(sender, ":")

  defp same_well_known_sender?(_left, _right), do: false

  # This predicate may only prove rules disjoint; any criterion combination it
  # cannot prove disjoint is treated as overlapping. That conservatism keeps
  # the sender guarantee independent of D-Bus name-owner timing.
  defp rules_may_overlap?(%MatchRule{criteria: left}, %MatchRule{criteria: right}) do
    exact_criteria_compatible?(left, right) and
      paths_compatible?(left, right) and
      exact_arguments_compatible?(left, right) and
      arg0_namespaces_compatible?(left, right)
  end

  defp exact_criteria_compatible?(left, right) do
    Enum.all?([:interface, :member, :destination], fn key ->
      not (Map.has_key?(left, key) and Map.has_key?(right, key) and left[key] != right[key])
    end)
  end

  defp paths_compatible?(left, right) do
    paths_overlap?(rule_path(left), rule_path(right))
  end

  defp rule_path(%{path: path}), do: {:exact, path}
  defp rule_path(%{path_namespace: path}), do: {:namespace, path}
  defp rule_path(_criteria), do: :any

  defp paths_overlap?(:any, _right), do: true
  defp paths_overlap?(_left, :any), do: true
  defp paths_overlap?({:exact, left}, {:exact, right}), do: left == right

  defp paths_overlap?({:exact, path}, {:namespace, namespace}),
    do: path_in_namespace?(path, namespace)

  defp paths_overlap?({:namespace, namespace}, {:exact, path}),
    do: path_in_namespace?(path, namespace)

  defp paths_overlap?({:namespace, left}, {:namespace, right}) do
    path_in_namespace?(left, right) or path_in_namespace?(right, left)
  end

  defp exact_arguments_compatible?(left, right) do
    left_args = Map.get(left, :args, %{})
    right_args = Map.get(right, :args, %{})

    Enum.all?(left_args, fn {index, value} ->
      case Map.fetch(right_args, index) do
        {:ok, other_value} -> value == other_value
        :error -> true
      end
    end)
  end

  defp arg0_namespaces_compatible?(left, right) do
    case {Map.get(left, :arg0namespace), Map.get(right, :arg0namespace)} do
      {nil, _right} ->
        true

      {_left, nil} ->
        true

      {left_namespace, right_namespace} ->
        left_namespace == right_namespace or
          String.starts_with?(left_namespace, right_namespace <> ".") or
          String.starts_with?(right_namespace, left_namespace <> ".")
    end
  end

  defp path_in_namespace?(path, namespace) do
    path == namespace or
      (namespace == "/" and String.starts_with?(path, "/")) or
      String.starts_with?(path, namespace <> "/")
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

    state = %{
      state
      | subscriptions:
          Map.put(state.subscriptions, handler_ref, %{owner: owner, key: key, handler: :active}),
        owner_monitors: Map.put(state.owner_monitors, monitor_ref, handler_ref),
        ref_monitors: Map.put(state.ref_monitors, handler_ref, monitor_ref)
    }

    mark_subscription_dirty(state, handler_ref)
  end

  defp mark_handler_removed(state, ref) do
    # Owner DOWN may have dropped the subscription while its final RemoveMatch
    # operation was still in flight.  Its late result still drives rule cleanup,
    # but must never dereference a now-missing local subscription.
    case Map.get(state.subscriptions, ref) do
      nil ->
        state

      %{handler: :active} ->
        state = put_in(state.subscriptions[ref].handler, :removed)
        mark_subscription_dirty(state, ref)

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

        state
        |> mark_subscription_removed(ref)
        |> update_rule(key, fn rule -> %{rule | refs: MapSet.delete(rule.refs, ref)} end)
    end
  end

  # A worker can be restarted by its DynamicSupervisor while the connection is
  # still alive. Persist only stable ownership/rule facts; queued callers and
  # in-flight operations are deliberately marked uncertain because their local
  # handler and bus effects cannot be reconciled from a snapshot. The rows are
  # incremental: an operation touches only its changed rule/reference rather
  # than copying every subscription on a busy connection.
  defp persist_state(state) do
    uncertain? =
      state.state_lost? or map_size(state.operations) > 0 or map_size(state.requests) > 0

    if not uncertain? and map_size(state.rules) == 0 and map_size(state.subscriptions) == 0 do
      :ok = MatchSubscription.delete_state(state.conn)
    else
      MatchSubscription.persist_state(
        state.conn,
        uncertain?,
        state.persistence,
        state.rules,
        state.subscriptions
      )
    end

    %{state | persistence: empty_persistence_changes()}
  end

  defp restore_state(state, %{rules: rules, subscriptions: subscriptions})
       when is_map(rules) and is_map(subscriptions) do
    rules =
      Map.new(rules, fn {key, rule} ->
        {key,
         %{
           rule: rule.rule,
           refs: rule.refs,
           pending_handlers: rule.pending_handlers,
           remote_may_exist?: rule.remote_may_exist?,
           status: rule.status,
           operation: nil,
           queue: [],
           recovery_kind: rule.recovery_kind,
           recovery_attempt: rule.recovery_attempt,
           retry_timer: nil
         }}
      end)

    {owner_monitors, ref_monitors} =
      Enum.reduce(subscriptions, {%{}, %{}}, fn {ref, %{owner: owner}}, {owners, refs} ->
        monitor_ref = Process.monitor(owner)
        {Map.put(owners, monitor_ref, ref), Map.put(refs, ref, monitor_ref)}
      end)

    recovering_rules =
      rules
      |> Enum.reduce(MapSet.new(), fn
        {key, %{status: :recovering}}, keys -> MapSet.put(keys, key)
        {_key, _rule}, keys -> keys
      end)

    %{
      state
      | rules: rules,
        subscriptions: subscriptions,
        owner_monitors: owner_monitors,
        ref_monitors: ref_monitors,
        recovering_rules: recovering_rules
    }
  end

  defp restore_state(state, _persisted), do: state

  defp empty_persistence_changes do
    %{
      dirty_rules: MapSet.new(),
      removed_rules: MapSet.new(),
      dirty_subscriptions: MapSet.new(),
      removed_subscriptions: MapSet.new()
    }
  end

  defp put_rule(state, key, rule) do
    changes = state.persistence

    changes = %{
      changes
      | dirty_rules: MapSet.put(changes.dirty_rules, key),
        removed_rules: MapSet.delete(changes.removed_rules, key)
    }

    %{state | rules: Map.put(state.rules, key, rule), persistence: changes}
  end

  defp delete_rule(state, key) do
    changes = state.persistence

    changes = %{
      changes
      | dirty_rules: MapSet.delete(changes.dirty_rules, key),
        removed_rules: MapSet.put(changes.removed_rules, key)
    }

    %{state | rules: Map.delete(state.rules, key), persistence: changes}
  end

  defp mark_subscription_dirty(state, ref) do
    changes = state.persistence

    changes = %{
      changes
      | dirty_subscriptions: MapSet.put(changes.dirty_subscriptions, ref),
        removed_subscriptions: MapSet.delete(changes.removed_subscriptions, ref)
    }

    %{state | persistence: changes}
  end

  defp mark_subscription_removed(state, ref) do
    changes = state.persistence

    changes = %{
      changes
      | dirty_subscriptions: MapSet.delete(changes.dirty_subscriptions, ref),
        removed_subscriptions: MapSet.put(changes.removed_subscriptions, ref)
    }

    %{state | persistence: changes}
  end

  defp update_rule(state, key, fun) do
    case Map.get(state.rules, key) do
      nil -> state
      rule -> put_rule(state, key, fun.(rule))
    end
  end

  defp max_recovering_rules do
    Application.get_env(:rebus, :match_recovery_max_rules, 64)
  end

  defp retry_delay(attempt) do
    Enum.at(@recovery_delays, min(attempt - 1, length(@recovery_delays) - 1))
  end

  defp definitive_bus_error?({:error, {:bus_error, error_name}}) when is_binary(error_name),
    do: true

  defp definitive_bus_error?({:error, {:reply_dropped, {:error, error_name}}})
       when is_binary(error_name),
       do: true

  defp definitive_bus_error?(_error), do: false

  defp match_rule_not_found?({:error, {:bus_error, @match_rule_not_found}}), do: true

  defp match_rule_not_found?({:error, {:reply_dropped, {:error, @match_rule_not_found}}}),
    do: true

  defp match_rule_not_found?(_error), do: false

  defp before_deadline?(deadline) when is_integer(deadline) do
    deadline > System.monotonic_time(:millisecond)
  end

  defp remaining_timeout(deadline) when is_integer(deadline) do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, remaining}
      _expired -> {:error, :timeout}
    end
  end

  defp log_recovery(event) do
    Logger.debug("D-Bus match cleanup transition=#{event}")
  end

  defp log_reset(event) do
    Logger.warning("D-Bus match reset transition=#{event}")
  end
end
