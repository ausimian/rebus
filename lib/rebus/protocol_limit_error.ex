defmodule Rebus.ProtocolLimitError do
  @moduledoc """
  Raised when encoding or decoding exceeds a wire-size limit set by the D-Bus
  specification. At present that is the array limit,
  `Rebus.Message.max_array_size/0`.

  These limits are part of the protocol, so data that trips one can neither
  be sent to nor accepted from a conforming peer, however much memory is
  available. `Rebus.ResourceLimitError` is the local counterpart, raised for
  wire-valid data that would exhaust the VM.
  """

  defexception limit: :unknown, message: "D-Bus protocol size limit exceeded"

  @typedoc "The protocol wire-size limit that was exceeded."
  @type limit :: :array | :unknown

  @type t :: %__MODULE__{limit: limit(), message: String.t()}
end
