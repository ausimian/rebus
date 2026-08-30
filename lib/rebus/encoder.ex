defmodule Rebus.Encoder do
  @moduledoc """
  D-Bus message encoder that marshals data according to D-Bus wire format.

  Implements the D-Bus marshaling format with proper alignment and byte ordering.
  Double special values use `:infinity`, `:negative_infinity`, and `:nan`;
  `:nan` encodes as a canonical quiet NaN.
  """

  alias Rebus.ResourceLimitError
  alias Rebus.Signature
  alias Rebus.WireValue

  @min_int16 -32_768
  @max_int16 32_767
  @max_uint16 65_535
  @min_int32 -2_147_483_648
  @max_int32 2_147_483_647
  @max_uint32 4_294_967_295
  @min_int64 -9_223_372_036_854_775_808
  @max_int64 9_223_372_036_854_775_807
  @max_uint64 18_446_744_073_709_551_615

  @type endianness :: :little | :big
  @type encoding_state :: %{
          endianness: endianness(),
          position: non_neg_integer(),
          buffer: iodata(),
          scalar_budget: non_neg_integer()
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

  Encoding accepts at most 1,000,000 fixed-width scalar elements in total per
  encode operation, matching the decoder's scalar-array cap. The budget is
  shared cumulatively across every fixed-width scalar array in that operation.

  ## Examples

      # Encode a simple integer
      iex> Rebus.Encoder.encode("i", [42]) |> IO.iodata_to_binary()
      <<42, 0, 0, 0>>

      # Encode a string
      iex> Rebus.Encoder.encode("s", ["hello"]) |> IO.iodata_to_binary()
      <<5, 0, 0, 0, "hello", 0>>

      # Encode an array of integers
      iex> Rebus.Encoder.encode("ai", [[1, 2, 3]]) |> IO.iodata_to_binary()
      <<12, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0>>

      # Encode a struct with mixed types
      iex> Rebus.Encoder.encode("(si)", [["hello", 42]]) |> IO.iodata_to_binary()
      <<5, 0, 0, 0, "hello", 0, 0, 0, 42, 0, 0, 0>>

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
    encode_at_position(signature, data, endianness, 0)
  end

  @doc """
  Encode data with a specific starting position for alignment calculations.

  This is useful when the encoded data will be inserted at a specific position
  in a larger message, and alignment must be calculated relative to that position.
  """
  @spec encode_at_position(binary(), [any()], endianness(), non_neg_integer()) :: iodata()
  def encode_at_position(signature, data, endianness, starting_position) do
    state =
      Map.merge(
        %{
          endianness: endianness,
          position: starting_position,
          buffer: [],
          scalar_budget: Rebus.Message.max_scalar_elements()
        },
        Signature.new_nesting_state()
      )

    signature
    |> Signature.parse!()
    |> encode_types(data, state)
    |> then(fn %{buffer: buffer} -> Enum.reverse(buffer) end)
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

  defp encode_single({:int16, _}, value, state)
       when is_integer(value) and value >= @min_int16 and value <= @max_int16 do
    data =
      case state.endianness do
        :little -> <<value::little-signed-16>>
        :big -> <<value::big-signed-16>>
      end

    add_aligned_data(state, data, 2)
  end

  defp encode_single({:uint16, _}, value, state)
       when is_integer(value) and value >= 0 and value <= @max_uint16 do
    data =
      case state.endianness do
        :little -> <<value::little-16>>
        :big -> <<value::big-16>>
      end

    add_aligned_data(state, data, 2)
  end

  defp encode_single({:int32, _}, value, state)
       when is_integer(value) and value >= @min_int32 and value <= @max_int32 do
    encode_int32(value, state)
  end

  defp encode_single({:uint32, _}, value, state)
       when is_integer(value) and value >= 0 and value <= @max_uint32 do
    encode_uint32(value, state)
  end

  defp encode_single({:int64, _}, value, state)
       when is_integer(value) and value >= @min_int64 and value <= @max_int64 do
    data =
      case state.endianness do
        :little -> <<value::little-signed-64>>
        :big -> <<value::big-signed-64>>
      end

    add_aligned_data(state, data, 8)
  end

  defp encode_single({:uint64, _}, value, state)
       when is_integer(value) and value >= 0 and value <= @max_uint64 do
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

  defp encode_single({:double, _}, value, state)
       when value in [:infinity, :negative_infinity, :nan] do
    bits =
      case value do
        :infinity -> 0x7FF0_0000_0000_0000
        :negative_infinity -> 0xFFF0_0000_0000_0000
        :nan -> 0x7FF8_0000_0000_0000
      end

    data =
      case state.endianness do
        :little -> <<bits::little-64>>
        :big -> <<bits::big-64>>
      end

    add_aligned_data(state, data, 8)
  end

  defp encode_single({:string, _}, value, state) when is_binary(value) do
    WireValue.validate!(:string, value)
    encode_string_like(value, state, 4)
  end

  defp encode_single({:object_path, _}, value, state) when is_binary(value) do
    WireValue.validate!(:object_path, value)
    encode_string_like(value, state, 4)
  end

  defp encode_single({:signature, _}, value, state) when is_binary(value) do
    WireValue.validate!(:signature, value)
    encode_string_like(value, state, 1)
  end

  defp encode_single({:struct, field_types}, values, state) when is_list(values) do
    # Structs are aligned to 8-byte boundary
    nested_state = Signature.enter_container!(state, :struct)
    aligned_state = align_to(nested_state, 8)

    # Encode each field in sequence
    field_types |> encode_types(values, aligned_state) |> Signature.leave_container(state)
  end

  defp encode_single({:array, element_type}, values, state) when is_list(values) do
    scalar_state = charge_scalar_array!(state, element_type, length(values))
    element_alignment = get_alignment(element_type)

    # Reserve the aligned uint32 length field so elements are encoded at their
    # actual stream position. Their padding can depend on that position.
    array_state = align_to(scalar_state, 4)
    element_state = %{array_state | position: array_state.position + 4, buffer: []}
    nested_element_state = Signature.enter_container!(element_state, :array)
    aligned_element_state = align_to(nested_element_state, element_alignment)
    final_element_state = encode_array_elements(element_type, values, aligned_element_state)

    data_length = final_element_state.position - aligned_element_state.position

    if data_length > Rebus.Message.max_array_size() do
      raise ArgumentError, "D-Bus array size limit exceeded"
    end

    length_data =
      case state.endianness do
        :little -> <<data_length::little-32>>
        :big -> <<data_length::big-32>>
      end

    # The encoder stores chunks in reverse order. Keep the elements as one
    # iodata chunk so the finalized output places the length before them.
    %{
      array_state
      | buffer: [Enum.reverse(final_element_state.buffer), length_data | array_state.buffer],
        position: final_element_state.position,
        scalar_budget: final_element_state.scalar_budget
    }
  end

  defp encode_single({:variant, _}, {signature, value}, state)
       when is_binary(signature) do
    # Variant: encode signature followed by value
    # First encode the signature
    signature_state = encode_single({:signature, nil}, signature, state)

    # Then parse and encode the value according to the signature
    [parsed_type] = Signature.parse!(signature)
    nested_state = Signature.enter_container!(signature_state, :variant)
    Signature.validate_nesting!([parsed_type], nested_state)

    payload_state =
      encode_single(parsed_type, value, nested_state)

    # A variant contributes to nesting only while its payload is being encoded.
    # Its siblings share the parent's depth, just as decoded variants do.
    Signature.leave_container(payload_state, state)
  end

  defp encode_single({:unix_fd, _}, fd_index, state)
       when is_integer(fd_index) and fd_index >= 0 and fd_index <= @max_uint32 do
    # Unix FD: encode as UINT32 index into the file descriptor array
    encode_uint32(fd_index, state)
  end

  defp encode_single({:dict_entry, key_type, value_type}, {key, value}, state) do
    # Dictionary entry: encode as struct with key and value
    # Dict entries are aligned to 8-byte boundary like structs
    nested_state = Signature.enter_container!(state, :dict_entry)
    aligned_state = align_to(nested_state, 8)

    # Encode key then value
    key_state = encode_single(key_type, key, aligned_state)

    key_state
    |> then(&encode_single(value_type, value, &1))
    |> Signature.leave_container(state)
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
    if (length_size == 1 and length > 255) or (length_size == 4 and length > @max_uint32) do
      raise ArgumentError, "D-Bus string length exceeds its wire limit"
    end

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
    # For structs in arrays, each struct must be aligned to 8-byte boundary
    aligned_state =
      case element_type do
        {:struct, _} -> align_to(state, 8)
        # dict entries are also structs
        {:dict_entry, _, _} -> align_to(state, 8)
        _ -> state
      end

    new_state = encode_single(element_type, value, aligned_state)
    encode_array_elements(element_type, rest, new_state)
  end

  defp charge_scalar_array!(state, element_type, count) do
    if scalar_width(element_type) do
      if state.scalar_budget >= count do
        %{state | scalar_budget: state.scalar_budget - count}
      else
        raise ResourceLimitError, limit: :scalar
      end
    else
      state
    end
  end

  defp scalar_width({:byte, _}), do: true
  defp scalar_width({:boolean, _}), do: true

  defp scalar_width({type, _})
       when type in [:int16, :uint16, :int32, :uint32, :int64, :uint64, :double, :unix_fd],
       do: true

  defp scalar_width(_), do: false
end
