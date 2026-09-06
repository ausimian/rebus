defmodule Rebus.TestRestartBudget do
  @moduledoc false

  # Kills that stay local to the process they target.
  #
  # Several suites kill children of `Rebus.MatchSubscription.Supervisor` or of
  # the `Rebus.MatchSubscription` dynamic supervisor beneath it, and each of
  # those carries the default budget of three restarts in five seconds. Those
  # suites are synchronous and share one application, so a kill in one test
  # still counts against the next - including the next in another module, since
  # the budget period outlives most tests. A kill made once `supervisor` has no
  # budget left does not restart the target locally: it takes `supervisor`
  # down, `Rebus.Supervisor` restarts the whole subtree, and
  # `Rebus.MatchSubscription.Store` and `Rebus.MatchSubscription.Registry` come
  # back under new pids - replacing the state table some tests assert on, and
  # breaking the assertions others make that a process started before the
  # target was left alone. Waiting for room first keeps each kill local. The
  # common case has room and does not wait.

  import ExUnit.Assertions, only: [assert: 2]

  # Upper bound on waiting for restart-budget room, in attempts of 10 ms each:
  # one budget period plus slack. Past that the wait cannot help, and the kill
  # would take the supervisor down, so this fails here instead of leaving a
  # later assertion to explain it.
  @restart_room_attempts 600

  @doc """
  Kills `pid` once `supervisor` has room in its restart budget.
  """
  @spec kill_supervised(pid(), atom()) :: true
  def kill_supervised(pid, supervisor) when is_pid(pid) do
    assert wait_until(
             fn -> not restart_budget_full?(supervisor) end,
             @restart_room_attempts
           ),
           "restart budget of #{inspect(supervisor)} did not free up"

    Process.exit(pid, :kill)
  end

  defp restart_budget_full?(name) do
    case Process.whereis(name) do
      nil -> false
      pid -> pid |> :sys.get_state() |> budget_full?()
    end
  end

  # `DynamicSupervisor` keeps its budget in a struct, so the fields are named.
  # The OTP `supervisor` record is positional: intensity, period and the list
  # of restart timestamps sit at positions 5, 6 and 7 as verified on OTP 27 and
  # 28, and the matrix also runs OTP 29. Any shape not recognised here is
  # false, so the kill proceeds at once and degrades to the pre-fix behaviour
  # rather than failing.
  defp budget_full?(%{max_restarts: max, max_seconds: period, restarts: restarts})
       when is_integer(max) and is_integer(period) and is_list(restarts),
       do: recent_restarts(restarts, period) >= max

  defp budget_full?(state)
       when is_tuple(state) and tuple_size(state) > 7 and elem(state, 0) == :state do
    intensity = elem(state, 5)
    period = elem(state, 6)
    restarts = elem(state, 7)

    is_integer(intensity) and is_integer(period) and is_list(restarts) and
      recent_restarts(restarts, period) >= intensity
  end

  defp budget_full?(_state), do: false

  # The supervisors record restart times with `:erlang.monotonic_time(1)` and
  # only prune the list when they add to it, so age the entries out here.
  defp recent_restarts(restarts, period) do
    now = System.monotonic_time(:second)
    Enum.count(restarts, &(is_integer(&1) and now <= &1 + period))
  end

  defp wait_until(predicate, attempts) when attempts > 0 do
    if predicate.() do
      true
    else
      Process.sleep(10)
      wait_until(predicate, attempts - 1)
    end
  end

  defp wait_until(_predicate, 0), do: false
end
