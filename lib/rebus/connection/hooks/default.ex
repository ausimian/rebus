defmodule Rebus.Connection.Hooks.Default do
  @moduledoc false

  # Production hooks: a request's deadline is exactly its public one.

  @behaviour Rebus.Connection.Hooks

  @impl Rebus.Connection.Hooks
  def request_timeout_slack, do: 0
end
