defmodule Rebus.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      {DynamicSupervisor, strategy: :one_for_one, name: Rebus.ConnectionSupervisor},
      {Registry, keys: :unique, name: Rebus.MatchSubscription.Registry},
      {Task.Supervisor, name: Rebus.MatchSubscription.TaskSupervisor},
      {Rebus.MatchSubscription, []}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Rebus.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
