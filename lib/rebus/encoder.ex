defmodule Rebus.Encoder do
  @moduledoc """
  D-Bus message encoder that marshals data according to D-Bus wire format.

  Implements the D-Bus marshaling format with proper alignment and byte ordering.
  """

  # D-Bus type codes
  # 'y'
  @type_byte 121
  # 'b'
  @type_boolean 98
  # 'n'
  @type_int16 110
  # 'q'
  @type_uint16 113
  # 'i'
  @type_int32 105
  # 'u'
  @type_uint32 117
  # 'x'
  @type_int64 120
  # 't'
  @type_uint64 116
  # 'd'
  @type_double 100
  # 's'
  @type_string 115
  # 'o'
  @type_object_path 111
  # 'g'
  @type_signature 103
  # 'a'
  @type_array 97
  # '('
  @type_struct_begin 40
  # ')'
  @type_struct_end 41
  # 'v'
  @type_variant 118
  # '{'
  @type_dict_begin 123
  # '}'
  @type_dict_end 125
  # 'h'
  @type_unix_fd 104

  @type endianness :: :little | :big
  @type encoding_state :: %{
          endianness: endianness(),
          position: non_neg_integer(),
          buffer: iodata()
        }

  @doc """
  Encodes data according to a D-Bus type signature into the wire format.

  This function takes a D-Bus type signature string and corresponding data,
  then marshals it into the binary format specified by the D-Bus protocol.
  The output follows D-Bus alignment rules and byte ordering.

  ## Parameters

    * `signature` - A D-Bus type signature string (e.g., "i", "s", "a(is)", etc.)
    * `data` - A list of values to encode that match the signature types
    * `endianness` - Byte order for encoding (`:little` or `:big`). Defaults to `:little`

  ## Returns

  Returns an iodata structure containing the encoded binary data that can be
  converted to binary using `IO.iodata_to_binary/1`.

  ## Examples

      # Encode a simple integer
      iex> Rebus.Encoder.encode("i", [42])
      [[[], <<42, 0, 0, 0>>]]

      # Encode a string
      iex> Rebus.Encoder.encode("s", ["hello"])
      [[[], <<5, 0, 0, 0>>, "hello", <<0>>]]

      # Encode an array of integers
      iex> Rebus.Encoder.encode("ai", [[1, 2, 3]])
      [[[], <<12, 0, 0, 0>>, <<1, 0, 0, 0>>, <<2, 0, 0, 0>>, <<3, 0, 0, 0>>]]

      # Encode a struct with mixed types
      iex> Rebus.Encoder.encode("(si)", [["hello", 42]])
      [[[], <<5, 0, 0, 0>>, "hello", [<<0>>, <<0, 0, 0>>], <<42, 0, 0, 0>>]]

  ## D-Bus Type Signatures

  Common D-Bus type codes:
    * `"y"` - byte (0-255)
    * `"b"` - boolean (0 or 1)
    * `"n"` - signed 16-bit integer
    * `"q"` - unsigned 16-bit integer
    * `"i"` - signed 32-bit integer
    * `"u"` - unsigned 32-bit integer
    * `"x"` - signed 64-bit integer
    * `"t"` - unsigned 64-bit integer
    * `"d"` - IEEE 754 double
    * `"s"` - UTF-8 string
    * `"o"` - object path
    * `"g"` - signature
    * `"a"` - array (followed by element type)
    * `"("` and `")"` - struct boundaries
    * `"v"` - variant
    * `"{"` and `"}"` - dictionary entry

  ## Alignment Rules

  The encoder automatically handles D-Bus alignment requirements:
    * 1-byte alignment: byte, boolean
    * 2-byte alignment: int16, uint16
    * 4-byte alignment: int32, uint32, string length, array length
    * 8-byte alignment: int64, uint64, double, struct start

  """
  @spec encode(binary(), [any()], endianness()) :: iodata()
  def encode(signature, data, endianness \\ :little) do
    state = %{endianness: endianness, position: 0, buffer: []}

    signature
    |> parse_signature()
    |> encode_types(data, state)
    |> then(fn %{buffer: buffer} -> Enum.reverse(buffer) end)
  end

  # Parse a D-Bus signature into a list of type structures
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

  # Encode parsed types with corresponding data
  defp encode_types([], [], state), do: state

  defp encode_types([type | types], [data | rest_data], state) do
    new_state = encode_single(type, data, state)
    encode_types(types, rest_data, new_state)
  end

  # Encode individual values based on their type
  defp encode_single({:byte, _}, value, state)
       when is_integer(value) and value >= 0 and value <= 255 do
    add_aligned_data(state, <<value::8>>, 1)
  end

  defp encode_single({:boolean, _}, value, state) when is_boolean(value) do
    bool_value = if value, do: 1, else: 0
    encode_uint32(bool_value, state)
  end

  defp encode_single({:int16, _}, value, state) when is_integer(value) do
    data =
      case state.endianness do
        :little -> <<value::little-signed-16>>
        :big -> <<value::big-signed-16>>
      end

    add_aligned_data(state, data, 2)
  end

  defp encode_single({:uint16, _}, value, state) when is_integer(value) and value >= 0 do
    data =
      case state.endianness do
        :little -> <<value::little-16>>
        :big -> <<value::big-16>>
      end

    add_aligned_data(state, data, 2)
  end

  defp encode_single({:int32, _}, value, state) when is_integer(value) do
    encode_int32(value, state)
  end

  defp encode_single({:uint32, _}, value, state) when is_integer(value) and value >= 0 do
    encode_uint32(value, state)
  end

  defp encode_single({:int64, _}, value, state) when is_integer(value) do
    data =
      case state.endianness do
        :little -> <<value::little-signed-64>>
        :big -> <<value::big-signed-64>>
      end

    add_aligned_data(state, data, 8)
  end

  defp encode_single({:uint64, _}, value, state) when is_integer(value) and value >= 0 do
    data =
      case state.endianness do
        :little -> <<value::little-64>>
        :big -> <<value::big-64>>
      end

    add_aligned_data(state, data, 8)
  end

  defp encode_single({:double, _}, value, state) when is_number(value) do
    data =
      case state.endianness do
        :little -> <<value::little-float-64>>
        :big -> <<value::big-float-64>>
      end

    add_aligned_data(state, data, 8)
  end

  defp encode_single({:string, _}, value, state) when is_binary(value) do
    encode_string_like(value, state, 4)
  end

  defp encode_single({:object_path, _}, value, state) when is_binary(value) do
    # TODO: Validate object path format
    encode_string_like(value, state, 4)
  end

  defp encode_single({:signature, _}, value, state) when is_binary(value) do
    # TODO: Validate signature format
    encode_string_like(value, state, 1)
  end

  defp encode_single({:struct, field_types}, values, state) when is_list(values) do
    # Structs are aligned to 8-byte boundary
    aligned_state = align_to(state, 8)

    # Encode each field in sequence
    encode_types(field_types, values, aligned_state)
  end

  defp encode_single({:array, element_type}, values, state) when is_list(values) do
    # First, encode all array elements to calculate total length
    element_alignment = get_alignment(element_type)

    # Create a temporary state to encode elements and calculate length
    temp_state = %{state | position: 0, buffer: []}
    temp_aligned = align_to(temp_state, element_alignment)

    # Encode all elements
    final_temp = encode_array_elements(element_type, values, temp_aligned)

    # Calculate data length (after alignment padding)
    data_length = final_temp.position - temp_aligned.position

    # Now encode for real: length + alignment + data
    length_state = encode_uint32(data_length, state)
    aligned_state = align_to(length_state, element_alignment)

    # Encode elements again in the real buffer
    encode_array_elements(element_type, values, aligned_state)
  end

  defp encode_single({:variant, _}, {signature, value}, state)
       when is_binary(signature) do
    # Variant: encode signature followed by value
    # First encode the signature
    signature_state = encode_single({:signature, nil}, signature, state)

    # Then parse and encode the value according to the signature
    [parsed_type] = parse_signature(signature)
    encode_single(parsed_type, value, signature_state)
  end

  defp encode_single({:unix_fd, _}, fd_index, state)
       when is_integer(fd_index) and fd_index >= 0 do
    # Unix FD: encode as UINT32 index into the file descriptor array
    encode_uint32(fd_index, state)
  end

  defp encode_single({:dict_entry, key_type, value_type}, {key, value}, state) do
    # Dictionary entry: encode as struct with key and value
    # Dict entries are aligned to 8-byte boundary like structs
    aligned_state = align_to(state, 8)

    # Encode key then value
    key_state = encode_single(key_type, key, aligned_state)
    encode_single(value_type, value, key_state)
  end

  # Helper functions

  defp encode_int32(value, state) do
    data =
      case state.endianness do
        :little -> <<value::little-signed-32>>
        :big -> <<value::big-signed-32>>
      end

    add_aligned_data(state, data, 4)
  end

  defp encode_uint32(value, state) do
    data =
      case state.endianness do
        :little -> <<value::little-32>>
        :big -> <<value::big-32>>
      end

    add_aligned_data(state, data, 4)
  end

  defp encode_string_like(string, state, length_size) do
    string_bytes = :unicode.characters_to_binary(string, :utf8)
    length = byte_size(string_bytes)

    # Encode length
    length_state =
      case length_size do
        1 -> add_aligned_data(state, <<length::8>>, 1)
        4 -> encode_uint32(length, state)
      end

    # Add string data and null terminator
    string_state = add_data(length_state, string_bytes)
    add_data(string_state, <<0>>)
  end

  defp add_aligned_data(state, data, alignment) do
    aligned_state = align_to(state, alignment)
    add_data(aligned_state, data)
  end

  defp add_data(state, data) do
    data_size = IO.iodata_length(data)
    %{state | buffer: [data | state.buffer], position: state.position + data_size}
  end

  defp align_to(state, alignment) do
    current_pos = state.position
    aligned_pos = align_position(current_pos, alignment)
    padding_size = aligned_pos - current_pos

    if padding_size > 0 do
      padding = :binary.copy(<<0>>, padding_size)
      add_data(state, padding)
    else
      state
    end
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

  defp encode_array_elements(_element_type, [], state), do: state

  defp encode_array_elements(element_type, [value | rest], state) do
    new_state = encode_single(element_type, value, state)
    encode_array_elements(element_type, rest, new_state)
  end
end
