defmodule Rebus.MatchSubscription do
  @moduledoc false
  use DynamicSupervisor

  alias Rebus.MatchRule
  alias Rebus.MatchSubscription.Store
  alias Rebus.MatchSubscription.Worker

  # The caller's timeout is one budget for the whole operation: installing or
  # removing the local handler, and the AddMatch or RemoveMatch reply. The
  # worker is given a slightly longer bound so its own deadline reply wins the
  # race against the caller giving up.
  @call_overhead 100
  @state_table Store.table()

  def start_link(_args) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    # The state table is owned by `Rebus.MatchSubscription.Store`, started
    # ahead of this supervisor.
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

    meta = {{:meta, conn}, %{uncertain?: uncertain?}}

    # The first write for a connection asks the table owner to monitor it, so
    # its rows are reaped even if no worker is alive to see the connection go.
    # The cast keeps the owner off this write path. Nothing may write the meta
    # row after the cast: the connection may already be dead, in which case the
    # owner reaps the row it can see and a later write would strand a new one.
    if :ets.insert_new(@state_table, meta) do
      GenServer.cast(Store, {:watch, conn})
    else
      true = :ets.insert(@state_table, meta)
    end

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
        call_remove_without_worker(conn, ref, deadline)
    end
  catch
    :exit, _reason -> {:error, :disconnected}
  end

  defp call_remove_without_worker(conn, ref, deadline) do
    if persisted?(conn) do
      with {:ok, worker} <- worker_for(conn) do
        Worker.call(worker, {:remove, ref, deadline}, deadline, @call_overhead)
      end
    else
      # References are idempotent and scoped to their original connection.
      :ok
    end
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
