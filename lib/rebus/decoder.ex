defmodule Rebus.Decoder do
  @moduledoc """
  Decodes D-Bus wire data into Elixir values.

  Multiple top-level values, arrays, and structs are represented as lists.
  Dictionary entries are `{key, value}` tuples, so dictionaries decode as
  lists of pairs. Variants are `{signature, value}` tuples. See the
  [D-Bus type system](https://dbus.freedesktop.org/doc/dbus-specification.html#type-system)
  for the signature grammar and wire layout.
  D-Bus infinities decode as `:infinity`/`:negative_infinity`; all NaNs decode
  as `:nan`, canonically losing their wire sign and payload.
  """

  alias Rebus.ProtocolLimitError
  alias Rebus.ResourceLimitError
  alias Rebus.Signature
  alias Rebus.WireValue

  import Bitwise, only: [&&&: 2]

  @default_element_budget 100_000
  @default_scalar_budget 1_000_000

  @type endianness :: :little | :big
  @typedoc false
  @type decoding_state :: %{
          endianness: endianness(),
          position: non_neg_integer(),
          data: binary(),
          array_depth: non_neg_integer(),
          struct_depth: non_neg_integer(),
          total_depth: non_neg_integer(),
          element_budget: non_neg_integer(),
          scalar_budget: non_neg_integer()
        }

  @doc """
  Decodes wire data according to `signature`.

  `endianness` is `:little` by default. The result is a list with one value
  for each top-level type in the signature.

  ## Raises

  Raises `ArgumentError` for an invalid signature, for a boolean whose wire
  value is neither 0 nor 1, and for non-zero alignment padding,
  `Rebus.ResourceLimitError` when the signature or data exceeds a local
  nesting, structural, or scalar-array limit, and `Rebus.ProtocolLimitError`
  when a declared array length exceeds `Rebus.Message.max_array_size/0`.

  ## Examples

      iex> Rebus.Decoder.decode("i", <<42, 0, 0, 0>>)
      [42]

      iex> Rebus.Decoder.decode("(si)", <<5, 0, 0, 0, "hello", 0, 0, 0, 42, 0, 0, 0>>)
      [["hello", 42]]

  """
  @spec decode(binary(), binary(), endianness()) :: [any()]
  def decode(signature, data, endianness \\ :little) do
    {values, _position} = decode_with_position(signature, data, endianness)
    values
  end

  @doc false
  @spec decode(binary(), binary(), endianness(), keyword()) :: [any()]
  def decode(signature, data, endianness, opts) do
    {values, _position} = decode_with_position(signature, data, endianness, opts)
    values
  end

  @doc """
  Decodes data and returns the number of bytes consumed.

  This is useful for callers that need to verify that a signature accounts for
  every byte in a bounded frame.

  D-Bus infinities are represented by atoms and NaNs by canonical `:nan`.
  Raises `Rebus.ResourceLimitError` when a local structural or scalar-array
  limit is exceeded, and `Rebus.ProtocolLimitError` when a declared array
  length exceeds `Rebus.Message.max_array_size/0`.

  ## Example

      iex> Rebus.Decoder.decode_with_position("y", <<7, 99>>)
      {[7], 1}
  """
  @spec decode_with_position(binary(), binary(), endianness()) :: {[any()], non_neg_integer()}
  def decode_with_position(signature, data, endianness \\ :little) do
    decode_with_position(signature, data, endianness, [])
  end

  @doc false
  @spec decode_with_position(binary(), binary(), endianness(), keyword()) ::
          {[any()], non_neg_integer()}
  def decode_with_position(signature, data, endianness, opts) when is_list(opts) do
    state = new_state(endianness, 0, data, opts)
    types = Signature.parse!(signature)
    Signature.validate_nesting!(types, state)

    {values, final_state} = decode_types(types, state)
    {values, final_state.position}
  end

  @doc false
  @spec decode_at_position(binary(), binary(), endianness(), non_neg_integer()) :: list()
  def decode_at_position(signature, data, endianness, starting_position) do
    # Create state with the starting position for proper alignment calculations
    state = new_state(endianness, starting_position, data, [])

    types = Signature.parse!(signature)
    Signature.validate_nesting!(types, state)

    types
    |> decode_types(state)
    # Return just the values, not the final state
    |> elem(0)
  end

  # Decode parsed types from binary data
  defp decode_types([], state), do: {[], state}

  defp decode_types([type | types], state) do
    {value, new_state} = state |> consume_element!() |> then(&decode_single(type, &1))
    {rest_values, final_state} = decode_types(types, new_state)
    {[value | rest_values], final_state}
  end

  # Decode individual values based on their type
  defp decode_single({:byte, _}, state), do: read_integer(state, 1, :unsigned)

  defp decode_single({:boolean, _}, state) do
    # The specification only permits 0 and 1 on the wire; anything else is a
    # malformed message, as libdbus also treats it.
    case decode_uint32(state) do
      {0, new_state} -> {false, new_state}
      {1, new_state} -> {true, new_state}
      {_other, _new_state} -> raise ArgumentError, "D-Bus boolean must be 0 or 1"
    end
  end

  defp decode_single({:int16, _}, state), do: read_integer(state, 2, :signed)

  defp decode_single({:uint16, _}, state), do: read_integer(state, 2, :unsigned)

  defp decode_single({:int32, _}, state), do: read_integer(state, 4, :signed)

  defp decode_single({:uint32, _}, state), do: decode_uint32(state)

  defp decode_single({:int64, _}, state), do: read_integer(state, 8, :signed)

  defp decode_single({:uint64, _}, state), do: read_integer(state, 8, :unsigned)

  defp decode_single({:double, _}, state) do
    {bits, new_state} = read_integer(state, 8, :unsigned)
    {decode_double(bits, state.endianness), new_state}
  end

  defp decode_single({:string, _}, state) do
    {value, new_state} = decode_string_like(state, 4)
    WireValue.validate!(:string, value)
    {value, new_state}
  end

  defp decode_single({:object_path, _}, state) do
    {value, new_state} = decode_string_like(state, 4)
    WireValue.validate!(:object_path, value)
    {value, new_state}
  end

  defp decode_single({:signature, _}, state) do
    {value, new_state} = decode_string_like(state, 1)
    WireValue.validate!(:signature, value)
    {value, new_state}
  end

  defp decode_single({:struct, field_types}, state) do
    # Structs are aligned to 8-byte boundary
    nested_state = Signature.enter_container!(state, :struct)
    aligned_state = align_to(nested_state, 8)
    {values, final_state} = decode_types(field_types, aligned_state)
    # Return struct as list
    {values, Signature.leave_container(final_state, state)}
  end

  defp decode_single({:array, element_type}, state) do
    # Read array length
    {array_length, length_state} = decode_uint32(state)

    # The declared length is checked before the data is bounded, so an array
    # declared over the limit is reported as a protocol size limit however few
    # body bytes are actually present: no conforming peer can send that array
    # at all, which makes the frame too large rather than merely malformed.
    if array_length > Rebus.Message.max_array_size() do
      raise ProtocolLimitError, limit: :array, message: "D-Bus array size limit exceeded"
    end

    # An array consumes its alignment padding plus the declared element bytes.
    padding = padding_for(length_state.position, Signature.alignment(element_type))
    total_array_size = padding + array_length

    # Extract exactly the data for this array
    <<array_binary::binary-size(^total_array_size), remaining_data::binary>> = length_state.data

    scalar_state = charge_scalar_array!(length_state, element_type, array_length)

    # Create a temporary state to decode just this array
    temp_state = %{scalar_state | data: array_binary}
    nested_state = Signature.enter_container!(temp_state, :array)
    aligned_state = skip_padding(nested_state, padding)

    # Track where array data ends within this isolated binary
    array_end_position = aligned_state.position + array_length

    # Whether elements are charged against the element budget is invariant over
    # the array, so decide it once here rather than per element.
    budgeted? = budgeted_array_element?(element_type)

    # Decode elements until we reach the end
    {elements, final_temp_state} =
      decode_array_elements(element_type, aligned_state, array_end_position, budgeted?, [])

    # Return with the remaining data and updated position
    final_state = %{
      length_state
      | data: remaining_data,
        position: length_state.position + total_array_size,
        element_budget: final_temp_state.element_budget,
        scalar_budget: final_temp_state.scalar_budget
    }

    {elements, final_state}
  end

  defp decode_single({:variant, _}, state) do
    # Read signature first
    {signature, signature_state} = decode_single({:signature, nil}, state)

    # Parse signature and decode value
    [parsed_type] = Signature.parse!(signature)
    nested_state = Signature.enter_container!(signature_state, :variant)
    Signature.validate_nesting!([parsed_type], nested_state)

    {value, final_state} =
      nested_state |> consume_element!() |> then(&decode_single(parsed_type, &1))

    {{signature, value}, Signature.leave_container(final_state, state)}
  end

  defp decode_single({:unix_fd, _}, state) do
    decode_uint32(state)
  end

  defp decode_single({:dict_entry, key_type, value_type}, state) do
    # Dictionary entries are like structs with key and value
    nested_state = Signature.enter_container!(state, :dict_entry)
    aligned_state = align_to(nested_state, 8)
    {key, key_state} = aligned_state |> consume_element!() |> then(&decode_single(key_type, &1))

    {value, final_state} = key_state |> consume_element!() |> then(&decode_single(value_type, &1))
    {{key, value}, Signature.leave_container(final_state, state)}
  end

  # Helper functions

  defp new_state(endianness, position, data, opts) do
    opts =
      Keyword.validate!(opts,
        element_budget: @default_element_budget,
        scalar_budget: @default_scalar_budget
      )

    Map.merge(
      %{
        endianness: endianness,
        position: position,
        data: data,
        element_budget: budget!(opts, :element_budget),
        scalar_budget: budget!(opts, :scalar_budget)
      },
      Signature.new_nesting_state()
    )
  end

  defp budget!(opts, key) do
    case Keyword.fetch!(opts, key) do
      budget when is_integer(budget) and budget > 0 -> budget
      _other -> raise ArgumentError, "#{key} must be a positive integer"
    end
  end

  defp consume_element!(%{element_budget: budget} = state) when budget > 0,
    do: %{state | element_budget: budget - 1}

  defp consume_element!(_state), do: raise(ResourceLimitError, limit: :structural)

  defp charge_scalar_array!(state, element_type, array_length) do
    case Signature.fixed_width(element_type) do
      nil -> state
      width -> consume_scalars!(state, div(array_length, width))
    end
  end

  defp consume_scalars!(%{scalar_budget: budget} = state, count) when budget >= count,
    do: %{state | scalar_budget: budget - count}

  defp consume_scalars!(_state, _count), do: raise(ResourceLimitError, limit: :scalar)

  defp decode_uint32(state), do: read_integer(state, 4, :unsigned)

  # Fixed-width integers are read at their natural alignment, then matched
  # directly out of the aligned binary.
  defp read_integer(state, size, signedness) do
    {bytes, new_state} = read_aligned_bytes(state, size, size)
    {integer_value(size, signedness, state.endianness, bytes), new_state}
  end

  defp integer_value(1, :unsigned, _endianness, <<value::8>>), do: value
  defp integer_value(2, :signed, :little, <<value::little-signed-16>>), do: value
  defp integer_value(2, :signed, :big, <<value::big-signed-16>>), do: value
  defp integer_value(2, :unsigned, :little, <<value::little-unsigned-16>>), do: value
  defp integer_value(2, :unsigned, :big, <<value::big-unsigned-16>>), do: value
  defp integer_value(4, :signed, :little, <<value::little-signed-32>>), do: value
  defp integer_value(4, :signed, :big, <<value::big-signed-32>>), do: value
  defp integer_value(4, :unsigned, :little, <<value::little-unsigned-32>>), do: value
  defp integer_value(4, :unsigned, :big, <<value::big-unsigned-32>>), do: value
  defp integer_value(8, :signed, :little, <<value::little-signed-64>>), do: value
  defp integer_value(8, :signed, :big, <<value::big-signed-64>>), do: value
  defp integer_value(8, :unsigned, :little, <<value::little-unsigned-64>>), do: value
  defp integer_value(8, :unsigned, :big, <<value::big-unsigned-64>>), do: value

  defp decode_string_like(state, 1) do
    {value, length_state} = read_bytes(state, 1)
    <<length::8>> = value
    read_string_body(length_state, length)
  end

  defp decode_string_like(state, 4) do
    {length, length_state} = decode_uint32(state)
    read_string_body(length_state, length)
  end

  defp read_string_body(state, length) do
    {string_data, string_state} = read_bytes(state, length)

    # Skip null terminator
    {null, final_state} = read_bytes(string_state, 1)
    <<0>> = null

    {string_data, final_state}
  end

  defp read_aligned_bytes(state, size, alignment) do
    aligned_state = align_to(state, alignment)
    read_bytes(aligned_state, size)
  end

  defp read_bytes(state, size) do
    <<value::binary-size(^size), rest::binary>> = state.data
    new_state = %{state | position: state.position + size, data: rest}
    {value, new_state}
  end

  defp align_to(state, alignment) do
    skip_padding(state, padding_for(state.position, alignment))
  end

  defp skip_padding(state, 0), do: state

  defp skip_padding(state, padding_size) do
    # Skip padding bytes in the data
    padded_data = binary_part(state.data, padding_size, byte_size(state.data) - padding_size)
    check_zero_padding!(state.data, padding_size)
    %{state | position: state.position + padding_size, data: padded_data}
  end

  # The specification requires alignment padding to be NUL bytes.
  defp check_zero_padding!(_data, 0), do: :ok

  defp check_zero_padding!(data, padding_size) do
    padding_bits = padding_size * 8

    case data do
      <<0::size(^padding_bits), _rest::binary>> -> :ok
      _ -> raise ArgumentError, "D-Bus alignment padding must be zero"
    end
  end

  defp padding_for(position, alignment) do
    case rem(position, alignment) do
      0 -> 0
      remainder -> alignment - remainder
    end
  end

  # Array-specific helper functions

  defp decode_array_elements(_element_type, state, end_position, _budgeted?, acc)
       when state.position >= end_position do
    {Enum.reverse(acc), state}
  end

  defp decode_array_elements(element_type, state, end_position, budgeted?, acc) do
    element_state = if budgeted?, do: consume_element!(state), else: state

    # Struct and dict-entry elements are aligned to 8 by decode_single/2 itself.
    {value, new_state} = decode_single(element_type, element_state)

    if new_state.position <= state.position do
      raise ArgumentError, "D-Bus array element did not consume input"
    end

    decode_array_elements(element_type, new_state, end_position, budgeted?, [value | acc])
  end

  # Only variable-width elements are charged against the element budget.
  defp budgeted_array_element?(element_type), do: Signature.fixed_width(element_type) == nil

  defp decode_double(0x7FF0_0000_0000_0000, _endianness), do: :infinity
  defp decode_double(0xFFF0_0000_0000_0000, _endianness), do: :negative_infinity

  defp decode_double(bits, _endianness)
       when (bits &&& 0x7FF0_0000_0000_0000) == 0x7FF0_0000_0000_0000,
       do: :nan

  defp decode_double(bits, :little) do
    <<value::little-float-64>> = <<bits::little-64>>
    value
  end

  defp decode_double(bits, :big) do
    <<value::big-float-64>> = <<bits::big-64>>
    value
  end
end
