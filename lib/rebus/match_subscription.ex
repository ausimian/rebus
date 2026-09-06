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

  def start_link(_args) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @impl true
  def init(:ok) do
    # The state table is owned by `Rebus.MatchSubscription.Store`, started
    # ahead of this supervisor.
    DynamicSupervisor.init(strategy: :one_for_one)
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
    if Store.persisted?(conn) do
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
