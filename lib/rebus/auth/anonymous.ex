defmodule Rebus.Auth.Anonymous do
  @moduledoc false

  # ANONYMOUS asks the server to accept a client without credentials. It is
  # offered only when the caller explicitly enabled it, and is the last
  # mechanism tried: whatever the server answers is final.

  @behaviour Rebus.Auth.Mechanism

  @impl true
  def name, do: "ANONYMOUS"

  @impl true
  def initial_response(_context), do: {:ok, nil, :waiting_for_ok}

  @impl true
  def handle_data(_data, _state, _context), do: {:error, :auth_failed}

  @impl true
  def handle_verdict(:ok, _state), do: :ok
  def handle_verdict({:rejected, mechanisms}, _state), do: {:error, {:auth_rejected, mechanisms}}
  def handle_verdict(:error, _state), do: {:error, :auth_failed}
end
