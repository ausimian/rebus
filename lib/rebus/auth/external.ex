defmodule Rebus.Auth.External do
  @moduledoc false

  # EXTERNAL authenticates with the credentials the kernel attaches to the
  # socket, so the only thing the client says is its authorization identity:
  # the hex-encoded uid, sent as the initial response.
  #
  # This is the client side of the specification's state machine for
  # EXTERNAL, entered in WaitingForData because AUTH carried an initial
  # response. The states only move forward, so a peer gets at most one DATA
  # answer and one CANCEL before it must send OK or REJECTED.

  @behaviour Rebus.Auth.Mechanism

  @impl true
  def name, do: "EXTERNAL"

  @impl true
  def initial_response(%{auth_id: auth_id}), do: {:ok, auth_id, :waiting_for_data}

  # EXTERNAL defines only an empty server challenge, to which the client
  # answers with its authorization identity. Restate the identity already sent
  # as the initial response and wait for the verdict. A non-empty challenge has
  # no meaning for EXTERNAL and is refused like any other unexpected line, and
  # any DATA once the identity has been restated cancels the mechanism.
  @impl true
  def handle_data(<<>>, :waiting_for_data, %{auth_id: auth_id}),
    do: {:reply, auth_id, :waiting_for_ok}

  def handle_data(_data, :waiting_for_data, _context), do: {:error, :auth_failed}
  def handle_data(_data, :waiting_for_ok, _context), do: :cancel

  # ERROR in either waiting state cancels the mechanism, and a rejection lets
  # the handshake move on to a mechanism the server advertised.
  @impl true
  def handle_verdict(:ok, _state), do: :ok
  def handle_verdict({:rejected, _mechanisms}, _state), do: :fallback
  def handle_verdict(:error, _state), do: :cancel
end
