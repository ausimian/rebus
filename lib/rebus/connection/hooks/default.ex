defmodule Rebus.Connection.Hooks.Default do
  @moduledoc false

  # Production hooks: nothing happens at a transition, and a request's deadline
  # is exactly its public one.

  @behaviour Rebus.Connection.Hooks

  @impl Rebus.Connection.Hooks
  def fd_claim_handoff, do: :ok

  @impl Rebus.Connection.Hooks
  def fd_claim_delivery, do: :ok

  @impl Rebus.Connection.Hooks
  def fd_claim_ack(_claim), do: :ok

  @impl Rebus.Connection.Hooks
  def request_timeout_slack, do: 0
end
