defmodule Rebus.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    # Each match-subscription worker is linked to its connection, and a worker
    # that stops while its connection is up stops the connection with an
    # error. Starting the workers' supervisor first means it stops last, so on
    # shutdown the connections go first and their workers follow quietly.
    children = [
      {Rebus.MatchSubscription.Supervisor, []},
      {DynamicSupervisor, strategy: :one_for_one, name: Rebus.ConnectionSupervisor}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Rebus.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
