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
  # `Rebus.MatchSubscription` reads and writes the table directly; this
  # process keeps it alive and reaps the rows of connections that die while
  # no worker is around to notice. `persist_state/5` casts `{:watch, conn}`
  # the first time it writes a connection's meta row, and the monitor set up
  # here covers the window after a worker-supervisor restart when nothing else
  # is watching that connection.

  use GenServer

  alias Rebus.MatchSubscription

  @state_table Rebus.MatchSubscription.State

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
        :ok = MatchSubscription.delete_state(conn)
        {:noreply, conns}
    end
  end

  def handle_info(_message, conns), do: {:noreply, conns}
end
