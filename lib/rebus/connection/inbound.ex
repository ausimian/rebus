defmodule Rebus.Connection.Inbound do
  @moduledoc false

  # The byte buffer a D-Bus connection fills from its socket, and the frame
  # boundary logic that turns those bytes into whole messages. It owns no
  # process state: no timers, no logging and no socket. Received fragments are
  # appended to a rope of segments, the fixed header is validated as soon as
  # sixteen bytes are retained, and the buffer is materialised exactly once per
  # complete frame.

  use TypedStruct

  alias Rebus.Message

  @max_inbound_segments 64

  typedstruct enforce: true do
    # Newest segment first; each entry carries its own byte size so merging
    # never has to re-measure a binary.
    field :segments, [{pos_integer(), binary()}], default: []
    field :size, non_neg_integer(), default: 0
    field :expected_size, pos_integer() | nil, default: nil
    # Counts whole-frame materialisations, so a test can assert that coalesced
    # frames are flattened once per receive rather than once per message.
    field :flatten_count, non_neg_integer(), default: 0
  end

  @spec new() :: t()
  def new, do: %__MODULE__{}

  # The handshake may read message bytes along with its final response. They
  # are ordinary inbound traffic and seed an otherwise empty buffer.
  @spec new(binary()) :: t()
  def new(<<>>), do: %__MODULE__{}

  def new(data) when is_binary(data),
    do: %__MODULE__{segments: [{byte_size(data), data}], size: byte_size(data)}

  @doc false
  @spec pending?(t()) :: boolean()
  def pending?(%__MODULE__{size: size}), do: size != 0

  # Retain the buffered bytes of the current frame, if any, discarding the
  # frame boundary. The materialisation count is a lifetime counter and is
  # deliberately preserved.
  @spec clear(t()) :: t()
  def clear(%__MODULE__{} = inbound),
    do: %{inbound | segments: [], size: 0, expected_size: nil}

  @spec append(t(), binary()) :: {:ok, t()} | {:error, :message_too_large}
  def append(%__MODULE__{} = inbound, <<>>), do: {:ok, inbound}

  def append(%__MODULE__{} = inbound, data) when is_binary(data) do
    segments = append_segment(data, inbound.segments)

    if length(segments) <= @max_inbound_segments do
      {:ok, %{inbound | segments: segments, size: inbound.size + byte_size(data)}}
    else
      # Segment metadata is part of the retained inbound budget. A peer that
      # defeats rope merging with pathological fragment sizes is rejected
      # before its BEAM-term overhead becomes unbounded.
      {:error, :message_too_large}
    end
  end

  # How many bytes to ask the socket for: enough to complete the fixed header
  # while the frame length is unknown, and no more than the rest of the frame
  # once it is. Never zero, so a receive always makes progress.
  @spec receive_size(t(), pos_integer()) :: pos_integer()
  def receive_size(%__MODULE__{expected_size: nil, size: size}, max_chunk),
    do: max(1, min(16 - size, max_chunk))

  def receive_size(%__MODULE__{expected_size: expected, size: size}, max_chunk),
    do: max(1, min(expected - size, max_chunk))

  # Fixed-header validation happens as soon as sixteen bytes are retained,
  # without making allocation depend on a peer-declared frame length. The
  # buffer is flattened only once the whole frame has arrived, and the frame
  # bytes are returned with the buffer cleared for the next one.
  @spec next(t()) :: {:frame, binary(), t()} | {:incomplete, t()} | {:error, atom()}
  def next(%__MODULE__{size: 0} = inbound), do: {:incomplete, inbound}

  def next(%__MODULE__{expected_size: nil} = inbound) do
    case Message.expected_size(prefix(inbound, min(inbound.size, 16))) do
      {:ok, expected_size} -> next(%{inbound | expected_size: expected_size})
      nil -> {:incomplete, inbound}
      {:error, reason} -> {:error, reason}
    end
  end

  def next(%__MODULE__{} = inbound) do
    if inbound.size >= inbound.expected_size do
      {:frame, flatten(inbound), %{clear(inbound) | flatten_count: inbound.flatten_count + 1}}
    else
      {:incomplete, inbound}
    end
  end

  # A remainder is a sub-binary of the frame it was parsed from, so retaining
  # it retains the whole materialised receive buffer. Copy it only when it is
  # small relative to that source; otherwise the copy costs more than it saves.
  @spec retain_remainder(binary(), binary()) :: binary()
  def retain_remainder(remainder, source) do
    if byte_size(remainder) * 4 < byte_size(source) do
      :binary.copy(remainder)
    else
      remainder
    end
  end

  defp prefix(%__MODULE__{} = inbound, size) do
    inbound.segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> take_prefix(size, [])
    |> IO.iodata_to_binary()
  end

  defp take_prefix(_segments, 0, acc), do: Enum.reverse(acc)
  defp take_prefix([], _size, acc), do: Enum.reverse(acc)

  defp take_prefix([segment | segments], size, acc) when byte_size(segment) <= size do
    take_prefix(segments, size - byte_size(segment), [segment | acc])
  end

  defp take_prefix([segment | _segments], size, acc) do
    Enum.reverse([binary_part(segment, 0, size) | acc])
  end

  defp flatten(%__MODULE__{} = inbound) do
    inbound.segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> IO.iodata_to_binary()
  end

  # Segments are newest first. Merging a segment only with smaller or equal
  # predecessors keeps common small-fragment traffic logarithmic while
  # preserving byte order. The explicit segment limit protects pathological
  # decreasing fragment sizes without flattening an ever-growing buffer.
  defp append_segment(data, segments) do
    merge_segment(byte_size(data), data, segments)
  end

  defp merge_segment(size, data, [{previous_size, previous} | segments])
       when previous_size <= size do
    merge_segment(previous_size + size, previous <> data, segments)
  end

  defp merge_segment(size, data, segments), do: [{size, data} | segments]
end
