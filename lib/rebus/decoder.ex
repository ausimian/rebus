defmodule Rebus.Decoder do
  @moduledoc """
  D-Bus message decoder that unmarshals data according to D-Bus wire format.

  Implements the D-Bus unmarshaling format with proper alignment and byte ordering.
  All structs and arrays are represented as Elixir lists for consistency.
  D-Bus infinities decode as `:infinity`/`:negative_infinity`; all NaNs decode
  as `:nan`, canonically losing their wire sign and payload.
  """

  alias Rebus.ResourceLimitError
  alias Rebus.Signature
  alias Rebus.WireValue

  import Bitwise, only: [&&&: 2]

  @default_element_budget 100_000
  @default_scalar_budget 1_000_000

  @type endianness :: :little | :big
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
  Decodes binary data based on the provided D-Bus signature.

  This function takes a D-Bus type signature string and binary data,
  then unmarshals it from the D-Bus wire format back into Elixir data structures.
  Both structs and arrays are represented as Elixir lists.

  ## Parameters

    * `signature` - A D-Bus type signature string (e.g., "i", "s", "a(is)", etc.)
    * `data` - Binary data in D-Bus wire format
    * `endianness` - Byte order for decoding (`:little` or `:big`). Defaults to `:little`

  ## Returns

  Returns the decoded Elixir data structure. Multiple values are returned as a list.

  ## Raises

  Raises `ArgumentError` for an invalid signature. Raises
  `Rebus.ResourceLimitError` when local resource limits are exceeded: 32 array
  levels, 32 struct levels, 64 total container levels, 100,000 structural
  terms, or 1,000,000 fixed-width scalar-array elements. `Rebus.Message`
  applies the structural and scalar limits independently to its header and body
  decodes.

  ## Examples

      # Decode a simple integer
      iex> Rebus.Decoder.decode("i", <<42, 0, 0, 0>>)
      [42]

      # Decode a string
      iex> Rebus.Decoder.decode("s", <<5, 0, 0, 0, "hello", 0>>)
      ["hello"]

      # Decode an array of integers
      iex> Rebus.Decoder.decode("ai", <<12, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0>>)
      [[1, 2, 3]]

      # Decode a struct (returned as list)
      iex> Rebus.Decoder.decode("(si)", <<5, 0, 0, 0, "hello", 0, 0, 0, 42, 0, 0, 0>>)
      [["hello", 42]]

  """
  @spec decode(binary(), binary(), endianness()) :: [any()]
  def decode(signature, data, endianness \\ :little) do
    {values, _position} = decode_with_position(signature, data, endianness)
    values
  end

  @doc false
  @spec decode(binary(), binary(), endianness(), pos_integer()) :: [any()]
  def decode(signature, data, endianness, element_budget) do
    {values, _position} =
      decode_with_position(signature, data, endianness, element_budget, @default_scalar_budget)

    values
  end

  @doc false
  @spec decode(binary(), binary(), endianness(), pos_integer(), pos_integer()) :: [any()]
  def decode(signature, data, endianness, element_budget, scalar_budget) do
    {values, _position} =
      decode_with_position(signature, data, endianness, element_budget, scalar_budget)

    values
  end

  @doc """
  Decodes data and returns the number of bytes consumed.

  This is useful for callers that need to verify that a signature accounts for
  every byte in a bounded frame.

  D-Bus infinities are represented by atoms and NaNs by canonical `:nan`.
  Raises `Rebus.ResourceLimitError` when the 100,000 structural-term or
  1,000,000 fixed-width-scalar budget is exceeded.
  """
  @spec decode_with_position(binary(), binary(), endianness()) :: {[any()], non_neg_integer()}
  def decode_with_position(signature, data, endianness \\ :little) do
    decode_with_position(signature, data, endianness, @default_element_budget)
  end

  @doc false
  @spec decode_with_position(binary(), binary(), endianness(), pos_integer()) ::
          {[any()], non_neg_integer()}
  def decode_with_position(signature, data, endianness, element_budget)
      when is_integer(element_budget) and element_budget > 0 do
    decode_with_position(signature, data, endianness, element_budget, @default_scalar_budget)
  end

  @doc false
  @spec decode_with_position(binary(), binary(), endianness(), pos_integer(), pos_integer()) ::
          {[any()], non_neg_integer()}
  def decode_with_position(signature, data, endianness, element_budget, scalar_budget)
      when is_integer(element_budget) and element_budget > 0 and is_integer(scalar_budget) and
             scalar_budget > 0 do
    state = new_state(endianness, 0, data, element_budget, scalar_budget)
    types = Signature.parse!(signature)
    Signature.validate_nesting!(types, state)

    {values, final_state} = decode_types(types, state)
    {values, final_state.position}
  end

  @doc """
  Decode data with a specific starting position for alignment calculations.

  This is useful when the data being decoded was encoded at a specific position
  in a larger message, and alignment must be calculated relative to that position.
  """
  @spec decode_at_position(binary(), binary(), endianness(), non_neg_integer()) :: list()
  def decode_at_position(signature, data, endianness, starting_position) do
    # Create state with the starting position for proper alignment calculations
    state =
      new_state(
        endianness,
        starting_position,
        data,
        @default_element_budget,
        @default_scalar_budget
      )

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
  defp decode_single({:byte, _}, state) do
    {value, new_state} = read_aligned_bytes(state, 1, 1)
    <<byte_value::8>> = value
    {byte_value, new_state}
  end

  defp decode_single({:boolean, _}, state) do
    {value, new_state} = decode_uint32(state)
    {value != 0, new_state}
  end

  defp decode_single({:int16, _}, state) do
    {value, new_state} = read_aligned_bytes(state, 2, 2)

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-signed-16>> = value
          result

        :big ->
          <<result::big-signed-16>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_single({:uint16, _}, state) do
    {value, new_state} = read_aligned_bytes(state, 2, 2)

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-16>> = value
          result

        :big ->
          <<result::big-16>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_single({:int32, _}, state) do
    decode_int32(state)
  end

  defp decode_single({:uint32, _}, state) do
    decode_uint32(state)
  end

  defp decode_single({:int64, _}, state) do
    {value, new_state} = read_aligned_bytes(state, 8, 8)

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-signed-64>> = value
          result

        :big ->
          <<result::big-signed-64>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_single({:uint64, _}, state) do
    {value, new_state} = read_aligned_bytes(state, 8, 8)

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-64>> = value
          result

        :big ->
          <<result::big-64>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_single({:double, _}, state) do
    {value, new_state} = read_aligned_bytes(state, 8, 8)

    bits =
      case state.endianness do
        :little ->
          <<result::little-64>> = value
          result

        :big ->
          <<result::big-64>> = value
          result
      end

    decoded_value = decode_double(bits, state.endianness)

    {decoded_value, new_state}
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

    if array_length > Rebus.Message.max_array_size() do
      raise ArgumentError, "D-Bus array size limit exceeded"
    end

    # Calculate how much data this array should consume in total
    # This includes alignment padding + the actual array data
    alignment_padding =
      case get_alignment(element_type) do
        alignment ->
          current_pos = length_state.position
          aligned_pos = align_position(current_pos, alignment)
          aligned_pos - current_pos
      end

    total_array_size = alignment_padding + array_length

    # Extract exactly the data for this array
    <<array_binary::binary-size(^total_array_size), remaining_data::binary>> = length_state.data

    scalar_state = charge_scalar_array!(length_state, element_type, array_length)

    # Create a temporary state to decode just this array
    temp_state = %{scalar_state | data: array_binary}
    nested_state = Signature.enter_container!(temp_state, :array)

    # Align to element type boundary
    element_alignment = get_alignment(element_type)
    aligned_state = align_to(nested_state, element_alignment)

    # Track where array data ends within this isolated binary
    array_end_position = aligned_state.position + array_length

    # Decode elements until we reach the end
    {elements, final_temp_state} =
      decode_array_elements(element_type, aligned_state, array_end_position, [])

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

  defp new_state(endianness, position, data, element_budget, scalar_budget) do
    Map.merge(
      %{
        endianness: endianness,
        position: position,
        data: data,
        element_budget: element_budget,
        scalar_budget: scalar_budget
      },
      Signature.new_nesting_state()
    )
  end

  defp consume_element!(%{element_budget: budget} = state) when budget > 0,
    do: %{state | element_budget: budget - 1}

  defp consume_element!(_state), do: raise(ResourceLimitError, limit: :structural)

  defp charge_scalar_array!(state, element_type, array_length) do
    case scalar_width(element_type) do
      nil -> state
      width -> consume_scalars!(state, div(array_length, width))
    end
  end

  defp consume_scalars!(%{scalar_budget: budget} = state, count) when budget >= count,
    do: %{state | scalar_budget: budget - count}

  defp consume_scalars!(_state, _count), do: raise(ResourceLimitError, limit: :scalar)

  defp scalar_width({:byte, _}), do: 1
  defp scalar_width({:boolean, _}), do: 4
  defp scalar_width({type, _}) when type in [:int16, :uint16], do: 2
  defp scalar_width({type, _}) when type in [:int32, :uint32, :unix_fd], do: 4
  defp scalar_width({type, _}) when type in [:int64, :uint64, :double], do: 8
  defp scalar_width(_), do: nil

  defp decode_int32(state) do
    {value, new_state} = read_aligned_bytes(state, 4, 4)

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-signed-32>> = value
          result

        :big ->
          <<result::big-signed-32>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_uint32(state) do
    {value, new_state} = read_aligned_bytes(state, 4, 4)

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-32>> = value
          result

        :big ->
          <<result::big-32>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_string_like(state, length_size) do
    # Read length
    {length, length_state} =
      case length_size do
        1 ->
          {value, new_state} = read_bytes(state, 1)
          <<len::8>> = value
          {len, new_state}

        4 ->
          decode_uint32(state)
      end

    # Read string data
    {string_data, string_state} = read_bytes(length_state, length)

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
    current_pos = state.position
    aligned_pos = align_position(current_pos, alignment)
    padding_size = aligned_pos - current_pos

    # Skip padding bytes in the data
    padded_data = binary_part(state.data, padding_size, byte_size(state.data) - padding_size)
    %{state | position: aligned_pos, data: padded_data}
  end

  defp align_position(position, alignment) do
    remainder = rem(position, alignment)

    if remainder == 0 do
      position
    else
      position + (alignment - remainder)
    end
  end

  # Array-specific helper functions

  defp get_alignment({:byte, _}), do: 1
  defp get_alignment({:boolean, _}), do: 4
  defp get_alignment({:int16, _}), do: 2
  defp get_alignment({:uint16, _}), do: 2
  defp get_alignment({:int32, _}), do: 4
  defp get_alignment({:uint32, _}), do: 4
  defp get_alignment({:int64, _}), do: 8
  defp get_alignment({:uint64, _}), do: 8
  defp get_alignment({:double, _}), do: 8
  defp get_alignment({:string, _}), do: 4
  defp get_alignment({:object_path, _}), do: 4
  defp get_alignment({:signature, _}), do: 1
  defp get_alignment({:variant, _}), do: 1
  defp get_alignment({:unix_fd, _}), do: 4
  defp get_alignment({:array, _}), do: 4
  defp get_alignment({:struct, _}), do: 8
  defp get_alignment({:dict_entry, _, _}), do: 8

  defp decode_array_elements(_element_type, state, end_position, acc)
       when state.position >= end_position do
    {Enum.reverse(acc), state}
  end

  defp decode_array_elements(element_type, state, end_position, acc) do
    # For structs in arrays, each struct must be aligned to 8-byte boundary
    aligned_state =
      case element_type do
        {:struct, _} -> align_to(state, 8)
        # dict entries are also structs
        {:dict_entry, _, _} -> align_to(state, 8)
        _ -> state
      end

    element_state =
      if budgeted_array_element?(element_type),
        do: consume_element!(aligned_state),
        else: aligned_state

    {value, new_state} = decode_single(element_type, element_state)

    if new_state.position <= aligned_state.position do
      raise ArgumentError, "D-Bus array element did not consume input"
    end

    decode_array_elements(element_type, new_state, end_position, [value | acc])
  end

  defp budgeted_array_element?({type, _})
       when type in [:string, :object_path, :signature, :variant],
       do: true

  defp budgeted_array_element?({type, _}) when type in [:array, :struct], do: true
  defp budgeted_array_element?({:dict_entry, _, _}), do: true
  defp budgeted_array_element?(_), do: false

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
