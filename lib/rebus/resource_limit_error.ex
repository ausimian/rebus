defmodule Rebus.ResourceLimitError do
  @moduledoc """
  Raised when encoding or decoding exceeds a local Rebus resource limit.

  These limits protect the VM from wire-valid D-Bus data that is too large
  or too deeply nested to handle. Malformed data is reported separately, and
  the protocol's own wire-size limits raise `Rebus.ProtocolLimitError`.
  """

  defexception limit: :unknown, message: "D-Bus local resource limit exceeded"

  @typedoc "The local resource cap that was exhausted."
  @type limit :: :structural | :scalar | :nesting | :unknown

  @type t :: %__MODULE__{limit: limit(), message: String.t()}
end
