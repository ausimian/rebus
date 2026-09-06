defmodule Rebus.MatchSubscription.Store do
  @moduledoc false

  # The match-subscription state table outlives the worker supervisor and the
  # workers themselves: it holds the rules the bus has actually been asked to
  # install, so losing it would strand those rules on the bus. An ETS table
  # dies with its owner, so the owner is this process, started ahead of the
  # registry, the task supervisor and the worker supervisor under a
  # `rest_for_one` parent. Anything that restarts those leaves the table
  # untouched, while a crash here restarts all of them, which is right: the
  # rows really are gone.
  #
  # This module is both halves of persistence: the change set recording what a
  # worker has touched since its last write, and the reads and writes that
  # apply it. Callers use the table directly through the functions below
  # rather than through this process; it keeps the table alive and reaps the
  # rows of connections that die while no worker is around to notice.
  # `persist_state/5` casts `{:watch, conn}` the first time it writes a
  # connection's meta row, and the monitor set up here covers the window after
  # a worker-supervisor restart when nothing else is watching that connection.

  use GenServer

  @state_table Rebus.MatchSubscription.State

  # A worker's rules and subscriptions are persisted incrementally, so it
  # records which keys it has touched since its last write. A key belongs to
  # exactly one of its kind's two sets.
  @type changes :: %{
          dirty_rules: MapSet.t(binary()),
          removed_rules: MapSet.t(binary()),
          dirty_subscriptions: MapSet.t(reference()),
          removed_subscriptions: MapSet.t(reference())
        }

  def table, do: @state_table

  def start_link(init_arg) do
    GenServer.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    _ =
      :ets.new(@state_table, [
        :named_table,
        :set,
        :public,
        read_concurrency: true,
        write_concurrency: true
      ])

    # The state is the set of monitored connections, `%{conn => ref}`. Monitors
    # are lost with the table if this process crashes, which is consistent:
    # there are no rows left to reap.
    {:ok, %{}}
  end

  @impl true
  def handle_cast({:watch, conn}, conns) when is_pid(conn) do
    # A connection whose rules all go away has its state deleted by its own
    # worker while it is still alive, so a later `insert_new` succeeds for a
    # connection that is already monitored. Monitor each one once.
    if Map.has_key?(conns, conn) do
      {:noreply, conns}
    else
      {:noreply, Map.put(conns, conn, Process.monitor(conn))}
    end
  end

  def handle_cast(_message, conns), do: {:noreply, conns}

  @impl true
  def handle_info({:DOWN, _ref, :process, conn, _reason}, conns) do
    case Map.pop(conns, conn) do
      {nil, _conns} ->
        {:noreply, conns}

      {_ref, conns} ->
        # The connection's own worker deletes these rows too when it sees the
        # same `:DOWN`. Deleting twice is harmless: `:ets.delete/2` and
        # `:ets.match_delete/2` are idempotent.
        :ok = delete_state(conn)
        {:noreply, conns}
    end
  end

  def handle_info(_message, conns), do: {:noreply, conns}

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
      GenServer.cast(__MODULE__, {:watch, conn})
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

  # No `@spec`: `MapSet.t/0` is opaque, so declaring one here would make this
  # constructor's success typing an opaqueness violation.
  @doc false
  def no_changes do
    %{
      dirty_rules: MapSet.new(),
      removed_rules: MapSet.new(),
      dirty_subscriptions: MapSet.new(),
      removed_subscriptions: MapSet.new()
    }
  end

  @doc false
  @spec rule_changed(changes(), binary()) :: changes()
  def rule_changed(changes, key), do: track_change(changes, :rules, key, :dirty)

  @doc false
  @spec rule_removed(changes(), binary()) :: changes()
  def rule_removed(changes, key), do: track_change(changes, :rules, key, :removed)

  @doc false
  @spec subscription_changed(changes(), reference()) :: changes()
  def subscription_changed(changes, ref), do: track_change(changes, :subscriptions, ref, :dirty)

  @doc false
  @spec subscription_removed(changes(), reference()) :: changes()
  def subscription_removed(changes, ref), do: track_change(changes, :subscriptions, ref, :removed)

  # A key belongs to exactly one of its kind's two change sets, so recording a
  # change moves it into one and out of the other.
  defp track_change(changes, kind, key, change) do
    {dirty, removed} =
      case kind do
        :rules -> {:dirty_rules, :removed_rules}
        :subscriptions -> {:dirty_subscriptions, :removed_subscriptions}
      end

    {into, out_of} = if change == :dirty, do: {dirty, removed}, else: {removed, dirty}

    changes
    |> Map.update!(into, &MapSet.put(&1, key))
    |> Map.update!(out_of, &MapSet.delete(&1, key))
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
end
