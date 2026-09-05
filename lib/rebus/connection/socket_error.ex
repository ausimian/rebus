defmodule Rebus.Connection.SocketError do
  @moduledoc false

  # A socket operation that fails part-way through reports the reason together
  # with the data it did not accept. That payload belongs to the caller's own
  # buffers, never to an error propagated out of the connection, so it is
  # dropped here. Shared by the handshake and the write queue, neither of which
  # may depend on the other.

  @doc false
  @spec normalize(term()) :: term()
  def normalize({:auth_rejected, _mechanisms} = error), do: error

  def normalize({reason, partial} = error) when is_atom(reason) do
    if is_binary(partial) or iolist?(partial), do: reason, else: error
  end

  def normalize(reason), do: reason

  @doc false
  @spec iolist?(term()) :: boolean()
  def iolist?(data) do
    _ = IO.iodata_to_binary(data)
    true
  rescue
    ArgumentError -> false
  end
end
