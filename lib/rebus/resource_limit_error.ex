defmodule Rebus.ResourceLimitError do
  @moduledoc """
  Raised when decoding or encoding exceeds a local Rebus resource limit.

  These limits protect the VM while handling otherwise wire-valid D-Bus data.
  They are distinct from malformed D-Bus grammar and from the protocol's own
  wire-size limits, which raise `Rebus.ProtocolLimitError`.
  """

  defexception limit: :unknown, message: "D-Bus local resource limit exceeded"

  @typedoc "The local resource cap that was exhausted."
  @type limit :: :structural | :scalar | :nesting | :unknown

  @type t :: %__MODULE__{limit: limit(), message: String.t()}
end
