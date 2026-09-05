defmodule Rebus.MatchSubscription.Supervisor do
  @moduledoc false
  use Supervisor

  def start_link(init_arg) do
    Supervisor.start_link(__MODULE__, init_arg, name: __MODULE__)
  end

  @impl true
  def init(_init_arg) do
    # Known limitation: `Registry` is itself a supervisor whose partitions are
    # registered under names derived from the registry's own name, so a brutal
    # `:kill` of the registry supervisor leaves those partition names
    # momentarily registered and an immediate restart of the plain child spec
    # can fail with `:already_started` and exhaust this supervisor's restart
    # intensity. That behaviour is pre-existing, cannot be provoked by anything
    # short of an external `Process.exit(pid, :kill)` against the registry
    # itself, and is deliberately not worked around here.
    children = [
      {Registry, keys: :unique, name: Rebus.MatchSubscription.Registry},
      {Task.Supervisor, name: Rebus.MatchSubscription.TaskSupervisor},
      {Rebus.MatchSubscription, []}
    ]

    # `rest_for_one` keeps the registry and the workers registered in it
    # consistent: if the registry restarts, the worker supervisor restarts too,
    # so no worker can survive unregistered and be duplicated by a later
    # `worker_for/1`. Connections are supervised elsewhere and are unaffected.
    Supervisor.init(children, strategy: :rest_for_one)
  end
end
