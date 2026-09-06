defmodule Rebus.ProtocolLimitError do
  @moduledoc """
  Raised when encoding or decoding exceeds a size limit set by the D-Bus
  specification - today, the array limit reported by
  `Rebus.Message.max_array_size/0`.

  These limits are part of the wire protocol, so data that trips one can
  neither be sent to nor accepted from a conforming peer, however much memory
  is available. They are distinct from `Rebus.ResourceLimitError`, which
  guards local VM resources while handling otherwise wire-valid D-Bus data.
  """

  defexception limit: :unknown, message: "D-Bus protocol size limit exceeded"

  @typedoc "The protocol wire-size limit that was exceeded."
  @type limit :: :array | :unknown

  @type t :: %__MODULE__{limit: limit(), message: String.t()}
end
