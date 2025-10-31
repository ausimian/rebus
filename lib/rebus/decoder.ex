defmodule Rebus.Decoder do
  @moduledoc """
  D-Bus message decoder that unmarshals data according to D-Bus wire format.

  Implements the D-Bus unmarshaling format with proper alignment and byte ordering.
  All structs and arrays are represented as Elixir lists for consistency.
  """

  # D-Bus type codes (reuse from encoder)
  @type_byte 121
  @type_boolean 98
  @type_int16 110
  @type_uint16 113
  @type_int32 105
  @type_uint32 117
  @type_int64 120
  @type_uint64 116
  @type_double 100
  @type_string 115
  @type_object_path 111
  @type_signature 103
  @type_array 97
  @type_struct_begin 40
  @type_struct_end 41
  @type_variant 118
  @type_dict_begin 123
  @type_dict_end 125
  @type_unix_fd 104

  @type endianness :: :little | :big
  @type decoding_state :: %{
          endianness: endianness(),
          position: non_neg_integer(),
          data: binary()
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
    state = %{endianness: endianness, position: 0, data: data}

    signature
    |> parse_signature()
    |> decode_types(state)
    # Return just the values, not the final state
    |> elem(0)
  end

  # Parse a D-Bus signature into a list of type structures (reuse encoder logic)
  defp parse_signature(signature) when is_binary(signature) do
    signature
    |> :binary.bin_to_list()
    |> parse_signature_types([])
  end

  defp parse_signature_types([], acc), do: Enum.reverse(acc)

  defp parse_signature_types([type | rest], acc) do
    case type do
      @type_array ->
        {element_type, remaining} = parse_single_type(rest)
        parse_signature_types(remaining, [{:array, element_type} | acc])

      @type_struct_begin ->
        {struct_types, remaining} = parse_struct_types(rest, [])
        parse_signature_types(remaining, [{:struct, struct_types} | acc])

      @type_dict_begin ->
        {key_type, rest1} = parse_single_type(rest)
        {value_type, [@type_dict_end | rest2]} = parse_single_type(rest1)
        parse_signature_types(rest2, [{:dict_entry, key_type, value_type} | acc])

      _ ->
        {single_type, remaining} = parse_single_type([type | rest])
        parse_signature_types(remaining, [single_type | acc])
    end
  end

  defp parse_single_type([type | rest]) do
    case type do
      @type_byte ->
        {{:byte, nil}, rest}

      @type_boolean ->
        {{:boolean, nil}, rest}

      @type_int16 ->
        {{:int16, nil}, rest}

      @type_uint16 ->
        {{:uint16, nil}, rest}

      @type_int32 ->
        {{:int32, nil}, rest}

      @type_uint32 ->
        {{:uint32, nil}, rest}

      @type_int64 ->
        {{:int64, nil}, rest}

      @type_uint64 ->
        {{:uint64, nil}, rest}

      @type_double ->
        {{:double, nil}, rest}

      @type_string ->
        {{:string, nil}, rest}

      @type_object_path ->
        {{:object_path, nil}, rest}

      @type_signature ->
        {{:signature, nil}, rest}

      @type_variant ->
        {{:variant, nil}, rest}

      @type_unix_fd ->
        {{:unix_fd, nil}, rest}

      @type_array ->
        {element_type, remaining} = parse_single_type(rest)
        {{:array, element_type}, remaining}

      @type_struct_begin ->
        {struct_types, remaining} = parse_struct_types(rest, [])
        {{:struct, struct_types}, remaining}

      @type_dict_begin ->
        {key_type, rest1} = parse_single_type(rest)
        {value_type, [@type_dict_end | rest2]} = parse_single_type(rest1)
        {{:dict_entry, key_type, value_type}, rest2}
    end
  end

  defp parse_struct_types([@type_struct_end | rest], acc) do
    {Enum.reverse(acc), rest}
  end

  defp parse_struct_types(types, acc) do
    {type, remaining} = parse_single_type(types)
    parse_struct_types(remaining, [type | acc])
  end

  # Decode parsed types from binary data
  defp decode_types([], state), do: {[], state}

  defp decode_types([type | types], state) do
    {value, new_state} = decode_single(type, state)
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

    decoded_value =
      case state.endianness do
        :little ->
          <<result::little-float-64>> = value
          result

        :big ->
          <<result::big-float-64>> = value
          result
      end

    {decoded_value, new_state}
  end

  defp decode_single({:string, _}, state) do
    decode_string_like(state, 4)
  end

  defp decode_single({:object_path, _}, state) do
    decode_string_like(state, 4)
  end

  defp decode_single({:signature, _}, state) do
    decode_string_like(state, 1)
  end

  defp decode_single({:struct, field_types}, state) do
    # Structs are aligned to 8-byte boundary
    aligned_state = align_to(state, 8)
    {values, final_state} = decode_types(field_types, aligned_state)
    # Return struct as list
    {values, final_state}
  end

  defp decode_single({:array, element_type}, state) do
    # Read array length
    {array_length, length_state} = decode_uint32(state)

    # Align to element type boundary
    element_alignment = get_alignment(element_type)
    aligned_state = align_to(length_state, element_alignment)

    # Track where array data ends
    array_end_position = aligned_state.position + array_length

    # Decode elements until we reach the end
    {elements, _final_state} =
      decode_array_elements(element_type, aligned_state, array_end_position, [])

    # Update state position to end of array
    final_state = %{aligned_state | position: array_end_position}

    {elements, final_state}
  end

  defp decode_single({:variant, _}, state) do
    # Read signature first
    {signature, signature_state} = decode_single({:signature, nil}, state)

    # Parse signature and decode value
    [parsed_type] = parse_signature(signature)
    {value, final_state} = decode_single(parsed_type, signature_state)

    {{signature, value}, final_state}
  end

  defp decode_single({:unix_fd, _}, state) do
    decode_uint32(state)
  end

  defp decode_single({:dict_entry, key_type, value_type}, state) do
    # Dictionary entries are like structs with key and value
    aligned_state = align_to(state, 8)
    {key, key_state} = decode_single(key_type, aligned_state)
    {value, final_state} = decode_single(value_type, key_state)
    {{key, value}, final_state}
  end

  # Helper functions

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
    {_null, final_state} = read_bytes(string_state, 1)

    {string_data, final_state}
  end

  defp read_aligned_bytes(state, size, alignment) do
    aligned_state = align_to(state, alignment)
    read_bytes(aligned_state, size)
  end

  defp read_bytes(state, size) do
    <<value::binary-size(size), _rest::binary>> =
      binary_part(state.data, state.position, byte_size(state.data) - state.position)

    new_state = %{state | position: state.position + size}
    {value, new_state}
  end

  defp align_to(state, alignment) do
    current_pos = state.position
    aligned_pos = align_position(current_pos, alignment)
    %{state | position: aligned_pos}
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
    {value, new_state} = decode_single(element_type, state)
    decode_array_elements(element_type, new_state, end_position, [value | acc])
  end
end
