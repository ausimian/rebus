defmodule Rebus.Auth do
  @moduledoc false

  # Parsing shared by every authentication mechanism. The mechanisms
  # themselves implement `Rebus.Auth.Mechanism`, and
  # `Rebus.Connection.Handshake` drives them.

  @max_mechanisms 64
  @max_mechanism_size 64

  # Implementations differ in how they space out the mechanism list: a trailing
  # space or a run of two spaces is common enough that failing the whole line
  # over one would lose mechanisms the peer really does offer. The raw segment
  # count is bounded first, so a flood is rejected before any segment is
  # inspected; past that, unusable segments - empty ones and names outside the
  # D-Bus character set - are dropped and the usable ones kept. A line whose
  # segments are all unusable carries nothing to act on and fails, while a bare
  # "REJECTED" advertises nothing by construction and is not a failure. Errors
  # stay payload-free: no peer-sent name reaches an error or a log.
  @doc false
  @spec parse_rejected(binary()) :: {:ok, [binary()]} | {:error, :auth_failed}
  def parse_rejected("REJECTED"), do: {:ok, []}

  def parse_rejected("REJECTED " <> advertised) do
    segments = :binary.split(advertised, " ", [:global])

    if length(segments) in 1..@max_mechanisms do
      case Enum.filter(segments, &valid_mechanism?/1) do
        [] -> {:error, :auth_failed}
        mechanisms -> {:ok, Enum.map(mechanisms, &:binary.copy/1)}
      end
    else
      {:error, :auth_failed}
    end
  end

  def parse_rejected(_line), do: {:error, :auth_failed}

  defp valid_mechanism?(mechanism)
       when byte_size(mechanism) in 1..@max_mechanism_size,
       do: all_bytes?(mechanism, &valid_mechanism_byte?/1)

  defp valid_mechanism?(_mechanism), do: false

  defp valid_mechanism_byte?(byte)
       when byte in ?A..?Z or byte in ?0..?9 or byte in [?_, ?-],
       do: true

  defp valid_mechanism_byte?(_byte), do: false

  # Walk the binary directly: no intermediate list, and the first byte that
  # fails the predicate ends the walk.
  defp all_bytes?(<<>>, _predicate), do: true

  defp all_bytes?(<<byte, rest::binary>>, predicate),
    do: predicate.(byte) and all_bytes?(rest, predicate)
end
