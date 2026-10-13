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
  # apply it. The table is `:protected`, so only this process writes it, and
  # every read and write below is a call into this process. Each call is one
  # logical operation, applied whole before the next one starts, so writers on
  # different connections cannot interleave inside one another's writes.
  #
  # Reads go through here too, not just writes. A worker killed while its
  # write call is queued still has that write applied, after it has died. If
  # its replacement read the table directly it could get there first and
  # restore an older snapshot than the one its predecessor finished. Reading
  # through the mailbox puts the replacement's read behind the write.
  #
  # Every call is bounded by `@call_timeout` and lets the exit through rather
  # than turning it into an error. A worker cannot do anything useful with a
  # write it could not make: its state no longer matches the table, so it
  # crashes and its replacement reloads. If this process is down, the
  # `rest_for_one` parent is already restarting every worker anyway. None of
  # the work done here waits on a worker or a connection, so a call cannot
  # deadlock: this process only touches the table and its own monitors.
  #
  # The process also reaps the rows of connections that die while no worker is
  # around to notice. The first write to create a connection's meta row
  # monitors that connection, which covers the window after a worker-supervisor
  # restart when nothing else is watching it.

  use GenServer

  @state_table Rebus.MatchSubscription.State

  # Each call is a handful of ETS operations, so this bound is only reached if
  # this process is wedged. Writes happen once per subscription change, not
  # per message, so funnelling every connection's writes through one process
  # costs nothing a busy bus would notice.
  @call_timeout 5_000

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
    _ = :ets.new(@state_table, [:named_table, :set, :protected])

    # The state is the set of monitored connections, `%{conn => ref}`. Monitors
    # are lost with the table if this process crashes, which is consistent:
    # there are no rows left to reap.
    {:ok, %{}}
  end

  @doc false
  def load_state(conn) when is_pid(conn), do: call({:load_state, conn})

  @doc false
  def persisted?(conn) when is_pid(conn), do: call({:persisted?, conn})

  # A write is three phases, each callable on its own so a test can stop
  # between them: the meta row is claimed as uncertain, the rows move, and only
  # then does the meta row take the caller's flag. `persist_state/5` runs all
  # three in one call, so a worker that dies part-way through it cannot tear
  # the write. Calling the phases separately still can, and leaves
  # `uncertain?: true` over a partial row set, so `load_state/1` sends the
  # replacement down the reset path rather than restoring half a snapshot.
  #
  # The rows are built here, in the caller, so a malformed change set fails in
  # the worker that passed it rather than taking down the table's owner.
  @doc false
  def persist_state(conn, uncertain?, changes, rules, subscriptions)
      when is_pid(conn) and is_boolean(uncertain?) and is_map(changes) and is_map(rules) and
             is_map(subscriptions) do
    call({:persist_state, conn, uncertain?, row_writes(conn, changes, rules, subscriptions)})
  end

  @doc false
  def mark_writing(conn) when is_pid(conn), do: call({:mark_writing, conn})

  @doc false
  def write_rows(conn, changes, rules, subscriptions)
      when is_pid(conn) and is_map(changes) and is_map(rules) and is_map(subscriptions) do
    call({:write_rows, row_writes(conn, changes, rules, subscriptions)})
  end

  @doc false
  def write_meta(conn, uncertain?) when is_pid(conn) and is_boolean(uncertain?) do
    call({:write_meta, conn, uncertain?})
  end

  # Deletes every row of a connection's rules and subscriptions, keeping the
  # meta row: a worker that discards an uncertain snapshot still wants the
  # connection watched and still wants the next read to see `uncertain?: true`.
  @doc false
  def discard_rows(conn) when is_pid(conn), do: call({:discard_rows, conn})

  @doc false
  def delete_state(conn) when is_pid(conn), do: call({:delete_state, conn})

  defp call(request), do: GenServer.call(__MODULE__, request, @call_timeout)

  @impl true
  def handle_call({:load_state, conn}, _from, conns) do
    {:reply, read_state(conn), conns}
  end

  def handle_call({:persisted?, conn}, _from, conns) do
    {:reply, :ets.member(@state_table, {:meta, conn}), conns}
  end

  def handle_call({:persist_state, conn, uncertain?, writes}, _from, conns) do
    conns = claim_meta(conns, conn)
    :ok = apply_writes(writes)
    _ = replace_meta(conn, uncertain?)
    {:reply, :ok, conns}
  end

  def handle_call({:mark_writing, conn}, _from, conns) do
    {:reply, :ok, claim_meta(conns, conn)}
  end

  def handle_call({:write_rows, writes}, _from, conns) do
    {:reply, apply_writes(writes), conns}
  end

  def handle_call({:write_meta, conn, uncertain?}, _from, conns) do
    _ = replace_meta(conn, uncertain?)
    {:reply, :ok, conns}
  end

  def handle_call({:discard_rows, conn}, _from, conns) do
    {:reply, discard(conn), conns}
  end

  def handle_call({:delete_state, conn}, _from, conns) do
    {:reply, delete(conn), conns}
  end

  @impl true
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
        :ok = delete(conn)
        {:noreply, conns}
    end
  end

  def handle_info(_message, conns), do: {:noreply, conns}

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

  defp read_state(conn) do
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

  # Only the first write for a connection may create its meta row, and that
  # write also monitors the connection, so its rows are reaped even if no
  # worker is alive to see it go. Every later meta write goes through
  # `replace_meta/2`, which updates the row in place and does nothing if it is
  # gone, so no write can resurrect a row reaped for a dead connection.
  #
  # `insert_new` can still create one for a connection that is already dead,
  # but that path is exactly the one that monitors it, and monitoring a dead
  # pid delivers `:DOWN` at once, so the row is reaped as soon as this call
  # returns. Rows written by a later `write_rows/4` call after such a reap
  # outlive it. A worker takes its own monitor on the connection in `init/1`,
  # before its first write, so its `:DOWN` deletes them; a caller of these
  # functions outside a worker has no such backstop.
  defp claim_meta(conns, conn) do
    if :ets.insert_new(@state_table, {{:meta, conn}, %{uncertain?: true}}) do
      watch(conns, conn)
    else
      _ = replace_meta(conn, true)
      conns
    end
  end

  # A connection whose rules all go away has its state deleted by its own
  # worker while it is still alive, so a later `insert_new` succeeds for a
  # connection that is already monitored. Monitor each one once.
  defp watch(conns, conn) do
    if Map.has_key?(conns, conn) do
      conns
    else
      Map.put(conns, conn, Process.monitor(conn))
    end
  end

  # Updates an existing meta row in place, atomically, returning 0 when there
  # is none: a reaped connection stays reaped.
  defp replace_meta(conn, uncertain?) do
    :ets.select_replace(@state_table, [
      {{{:meta, conn}, :_}, [], [{:const, {{:meta, conn}, %{uncertain?: uncertain?}}}]}
    ])
  end

  defp discard(conn) do
    true = :ets.match_delete(@state_table, {{:rule, conn, :_}, :_})
    true = :ets.match_delete(@state_table, {{:subscription, conn, :_}, :_})
    :ok
  end

  defp delete(conn) do
    true = :ets.delete(@state_table, {:meta, conn})
    discard(conn)
  end

  defp apply_writes({deletes, inserts}) do
    Enum.each(deletes, fn key -> true = :ets.delete(@state_table, key) end)
    true = :ets.insert(@state_table, inserts)
    :ok
  end

  defp persisted_rows(conn, kind) do
    :ets.match_object(@state_table, {{kind, conn, :_}, :_})
    |> Map.new(fn {{^kind, ^conn, key}, value} -> {key, value} end)
  end

  # The table operations a change set comes to: the keys to delete and the
  # rows to insert. A dirty key whose record has gone is deleted as well.
  defp row_writes(conn, changes, rules, subscriptions) do
    {rule_deletes, rule_inserts} =
      row_writes(conn, :rule, changes.dirty_rules, changes.removed_rules, rules)

    {subscription_deletes, subscription_inserts} =
      row_writes(
        conn,
        :subscription,
        changes.dirty_subscriptions,
        changes.removed_subscriptions,
        subscriptions
      )

    {rule_deletes ++ subscription_deletes, rule_inserts ++ subscription_inserts}
  end

  defp row_writes(conn, kind, dirty, removed, values) do
    deletes = Enum.map(removed, &{kind, conn, &1})

    Enum.reduce(dirty, {deletes, []}, fn key, {deletes, inserts} ->
      case Map.fetch(values, key) do
        {:ok, value} -> {deletes, [{{kind, conn, key}, row(value)} | inserts]}
        :error -> {[{kind, conn, key} | deletes], inserts}
      end
    end)
  end

  # A row is a plain map of the record's fields. A worker that holds a record
  # as a struct writes it without its struct tag, so the rows a version leaves
  # behind stay readable by the record definitions of another.
  defp row(%_struct{} = value), do: Map.from_struct(value)
  defp row(value) when is_map(value), do: value
end
