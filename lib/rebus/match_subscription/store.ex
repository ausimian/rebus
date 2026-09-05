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
  # process exists only to keep it alive.

  use GenServer

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

    {:ok, %{}}
  end
end
