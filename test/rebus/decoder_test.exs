defmodule Rebus.DecoderTest do
  use ExUnit.Case, async: true

  alias Rebus.{Encoder, Decoder}

  describe "basic types" do
    test "decodes byte" do
      data = <<42>>
      result = Decoder.decode("y", data)
      assert result == [42]
    end

    test "decodes boolean true" do
      data = <<1, 0, 0, 0>>
      result = Decoder.decode("b", data)
      assert result == [true]
    end

    test "decodes boolean false" do
      data = <<0, 0, 0, 0>>
      result = Decoder.decode("b", data)
      assert result == [false]
    end

    test "decodes int32" do
      data = <<42, 0, 0, 0>>
      result = Decoder.decode("i", data)
      assert result == [42]
    end

    test "decodes uint32" do
      data = <<42, 0, 0, 0>>
      result = Decoder.decode("u", data)
      assert result == [42]
    end

    test "decodes int16" do
      # 1000 in little endian
      data = <<232, 3>>
      result = Decoder.decode("n", data)
      assert result == [1000]
    end

    test "decodes int16 negative" do
      # -1000 in little endian
      data = <<24, 252>>
      result = Decoder.decode("n", data)
      assert result == [-1000]
    end

    test "decodes uint16" do
      # 65000 in little endian
      data = <<232, 253>>
      result = Decoder.decode("q", data)
      assert result == [65000]
    end

    test "decodes int64" do
      data = <<240, 222, 188, 154, 120, 86, 52, 18>>
      result = Decoder.decode("x", data)
      assert result == [0x123456789ABCDEF0]
    end

    test "decodes uint64" do
      data = <<240, 222, 188, 154, 120, 86, 52, 18>>
      result = Decoder.decode("t", data)
      assert result == [0x123456789ABCDEF0]
    end

    test "decodes double" do
      # Encode 3.14159 to get the exact binary representation
      encoded = Encoder.encode("d", [3.14159])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("d", data)
      [decoded_value] = result
      assert abs(decoded_value - 3.14159) < 0.00001
    end

    test "decodes string" do
      data = <<5, 0, 0, 0, "hello", 0>>
      result = Decoder.decode("s", data)
      assert result == ["hello"]
    end

    test "decodes signature" do
      data = <<2, "ii", 0>>
      result = Decoder.decode("g", data)
      assert result == ["ii"]
    end

    test "decodes object path" do
      data = <<21, 0, 0, 0, "/org/freedesktop/DBus", 0>>
      result = Decoder.decode("o", data)
      assert result == ["/org/freedesktop/DBus"]
    end
  end

  describe "multiple values" do
    test "decodes multiple basic types" do
      # Encode then decode multiple values
      encoded = Encoder.encode("ius", [42, 1000, "test"])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ius", data)
      assert result == [42, 1000, "test"]
    end
  end

  describe "alignment" do
    test "properly handles int16 after byte" do
      # Encode then decode with alignment
      encoded = Encoder.encode("yn", [42, 1000])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("yn", data)
      assert result == [42, 1000]
    end

    test "properly handles int32 after byte" do
      encoded = Encoder.encode("yi", [42, 1000])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("yi", data)
      assert result == [42, 1000]
    end
  end

  describe "endianness" do
    test "decodes with big endian" do
      data = <<0x12, 0x34, 0x56, 0x78>>
      result = Decoder.decode("i", data, :big)
      assert result == [0x12345678]
    end

    test "decodes with little endian" do
      data = <<0x78, 0x56, 0x34, 0x12>>
      result = Decoder.decode("i", data, :little)
      assert result == [0x12345678]
    end

    test "decodes int16 with big endian" do
      data = <<0x12, 0x34>>
      result = Decoder.decode("n", data, :big)
      assert result == [0x1234]
    end

    test "decodes double with big endian" do
      encoded = Encoder.encode("d", [3.14159], :big)
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("d", data, :big)
      [decoded_value] = result
      assert abs(decoded_value - 3.14159) < 0.00001
    end
  end

  describe "struct types" do
    test "decodes simple struct with two integers" do
      encoded = Encoder.encode("(ii)", [[42, 1000]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("(ii)", data)
      assert result == [[42, 1000]]
    end

    test "decodes struct with mixed types" do
      encoded = Encoder.encode("(yis)", [[42, 1000, "test"]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("(yis)", data)
      assert result == [[42, 1000, "test"]]
    end

    test "decodes struct after byte with alignment" do
      encoded = Encoder.encode("y(ii)", [42, [100, 200]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("y(ii)", data)
      assert result == [42, [100, 200]]
    end

    test "decodes nested struct" do
      encoded = Encoder.encode("((ii)i)", [[[10, 20], 30]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("((ii)i)", data)
      assert result == [[[10, 20], 30]]
    end

    test "decodes empty struct" do
      encoded = Encoder.encode("()", [[]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("()", data)
      assert result == [[]]
    end
  end

  describe "array types" do
    test "decodes simple array of integers" do
      encoded = Encoder.encode("ai", [[10, 20, 30]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ai", data)
      assert result == [[10, 20, 30]]
    end

    test "decodes array of strings" do
      encoded = Encoder.encode("as", [["hello", "world"]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("as", data)
      assert result == [["hello", "world"]]
    end

    test "decodes array of bytes" do
      encoded = Encoder.encode("ay", [[1, 2, 3, 4, 5]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ay", data)
      assert result == [[1, 2, 3, 4, 5]]
    end

    test "decodes array of structs" do
      encoded = Encoder.encode("a(ii)", [[[10, 20], [30, 40]]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("a(ii)", data)
      assert result == [[[10, 20], [30, 40]]]
    end

    test "decodes nested array of structs" do
      encoded = Encoder.encode("a(ai)", [[[[1, 2]]]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("a(ai)", data)
      assert result == [[[[1, 2]]]]
    end

    test "decodes empty array" do
      encoded = Encoder.encode("ai", [[]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ai", data)
      assert result == [[]]
    end
  end

  describe "variant and dictionary types" do
    test "decodes variant" do
      encoded = Encoder.encode("v", [{"i", 42}])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("v", data)
      assert result == [{"i", 42}]
    end

    test "decodes dictionary entry array" do
      encoded = Encoder.encode("a{si}", [[{"key1", 100}, {"key2", 200}]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("a{si}", data)
      assert result == [[{"key1", 100}, {"key2", 200}]]
    end

    test "decodes unix file descriptor" do
      encoded = Encoder.encode("h", [42])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("h", data)
      assert result == [42]
    end
  end

  describe "round-trip encoding/decoding" do
    test "round-trip with complex nested structure" do
      original_data = [
        # byte
        42,
        # struct
        [
          # string
          "hello",
          # array of structs
          [
            # struct with int and string
            [10, "world"],
            # another struct
            [20, "test"]
          ]
        ]
      ]

      # Encode
      encoded = Encoder.encode("y(sa(is))", original_data)
      data = IO.iodata_to_binary(encoded)

      # Decode
      result = Decoder.decode("y(sa(is))", data)

      assert result == original_data
    end

    test "round-trip with various endianness" do
      # Use positive 32-bit values
      original_data = [0x12345678, 0x7BCDEF01]

      # Test little endian
      encoded_little = Encoder.encode("ii", original_data, :little)
      data_little = IO.iodata_to_binary(encoded_little)
      result_little = Decoder.decode("ii", data_little, :little)
      assert result_little == original_data

      # Test big endian
      encoded_big = Encoder.encode("ii", original_data, :big)
      data_big = IO.iodata_to_binary(encoded_big)
      result_big = Decoder.decode("ii", data_big, :big)
      assert result_big == original_data
    end

    test "round-trip with unicode strings" do
      original_data = ["Hello 世界", "🚀 Elixir"]

      encoded = Encoder.encode("ss", original_data)
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ss", data)
      assert result == original_data
    end

    test "round-trip with maximum integer values" do
      original_data = [
        # max byte
        255,
        # max int32
        2_147_483_647,
        # max uint32
        4_294_967_295,
        # max int64
        9_223_372_036_854_775_807
      ]

      encoded = Encoder.encode("yiut", original_data)
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("yiut", data)
      assert result == original_data
    end
  end

  describe "edge cases" do
    test "decodes empty string" do
      encoded = Encoder.encode("s", [""])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("s", data)
      assert result == [""]
    end

    test "decodes complex alignment scenarios" do
      # int64 after byte requires 7 bytes padding
      encoded = Encoder.encode("yx", [42, 0x123456789ABCDEF0])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("yx", data)
      assert result == [42, 0x123456789ABCDEF0]
    end

    test "decodes mixed array types separately" do
      # Test that different array types work correctly
      encoded1 = Encoder.encode("ay", [[1, 2, 3]])
      data1 = IO.iodata_to_binary(encoded1)

      encoded2 = Encoder.encode("an", [[1000, 2000]])
      data2 = IO.iodata_to_binary(encoded2)

      result1 = Decoder.decode("ay", data1)
      result2 = Decoder.decode("an", data2)

      assert result1 == [[1, 2, 3]]
      assert result2 == [[1000, 2000]]
    end

    test "decodes single character string" do
      encoded = Encoder.encode("s", ["a"])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("s", data)
      assert result == ["a"]
    end

    test "decodes single signature character" do
      encoded = Encoder.encode("g", ["i"])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("g", data)
      assert result == ["i"]
    end

    test "decodes array with single element" do
      encoded = Encoder.encode("ai", [[42]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ai", data)
      assert result == [[42]]
    end

    test "decodes struct with single element" do
      encoded = Encoder.encode("(i)", [[42]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("(i)", data)
      assert result == [[42]]
    end

    test "decodes boolean array" do
      encoded = Encoder.encode("ab", [[true, false, true]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ab", data)
      assert result == [[true, false, true]]
    end

    test "decodes double with special values" do
      # Test with zero
      encoded_zero = Encoder.encode("d", [0.0])
      data_zero = IO.iodata_to_binary(encoded_zero)
      result_zero = Decoder.decode("d", data_zero)
      assert result_zero == [0.0]

      # Test with negative value
      encoded_neg = Encoder.encode("d", [-42.5])
      data_neg = IO.iodata_to_binary(encoded_neg)
      result_neg = Decoder.decode("d", data_neg)
      assert result_neg == [-42.5]
    end

    test "decodes maximum boundary values" do
      # Test maximum byte value
      encoded_byte = Encoder.encode("y", [255])
      data_byte = IO.iodata_to_binary(encoded_byte)
      result_byte = Decoder.decode("y", data_byte)
      assert result_byte == [255]

      # Test maximum uint16 value
      encoded_uint16 = Encoder.encode("q", [65535])
      data_uint16 = IO.iodata_to_binary(encoded_uint16)
      result_uint16 = Decoder.decode("q", data_uint16)
      assert result_uint16 == [65535]
    end

    test "decodes negative int values at boundaries" do
      # Test minimum int16
      encoded_int16 = Encoder.encode("n", [-32768])
      data_int16 = IO.iodata_to_binary(encoded_int16)
      result_int16 = Decoder.decode("n", data_int16)
      assert result_int16 == [-32768]

      # Test minimum int32
      encoded_int32 = Encoder.encode("i", [-2_147_483_648])
      data_int32 = IO.iodata_to_binary(encoded_int32)
      result_int32 = Decoder.decode("i", data_int32)
      assert result_int32 == [-2_147_483_648]
    end

    test "decodes complex variant with different types" do
      # Variant containing a string
      encoded_string = Encoder.encode("v", [{"s", "hello"}])
      data_string = IO.iodata_to_binary(encoded_string)
      result_string = Decoder.decode("v", data_string)
      assert result_string == [{"s", "hello"}]

      # Variant containing a double
      encoded_double = Encoder.encode("v", [{"d", 3.14}])
      data_double = IO.iodata_to_binary(encoded_double)
      result_double = Decoder.decode("v", data_double)
      [{"d", decoded_double}] = result_double
      assert abs(decoded_double - 3.14) < 0.001
    end

    test "decodes variant with struct" do
      encoded = Encoder.encode("v", [{"(ii)", [10, 20]}])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("v", data)
      assert result == [{"(ii)", [10, 20]}]
    end

    test "decodes variant with array" do
      encoded = Encoder.encode("v", [{"ai", [1, 2, 3]}])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("v", data)
      assert result == [{"ai", [1, 2, 3]}]
    end

    test "decodes complex nested array" do
      # Test a simpler but still complex nested structure
      original = [[[1, 2], [3, 4]], [[5, 6], [7, 8]]]
      encoded = Encoder.encode("a(ii)a(ii)", original)
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("a(ii)a(ii)", data)
      assert result == original
    end

    test "decodes multiple variants" do
      original = [{"i", 42}, {"s", "hello"}]
      encoded = Encoder.encode("vv", original)
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("vv", data)
      assert result == original
    end

    test "decodes long strings" do
      long_string = String.duplicate("x", 1000)
      encoded = Encoder.encode("s", [long_string])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("s", data)
      assert result == [long_string]
    end

    test "decodes object path with various valid paths" do
      paths = ["/", "/a", "/org/freedesktop", "/org/freedesktop/DBus/Local"]

      for path <- paths do
        encoded = Encoder.encode("o", [path])
        data = IO.iodata_to_binary(encoded)
        result = Decoder.decode("o", data)
        assert result == [path]
      end
    end
  end

  describe "comprehensive endianness tests" do
    test "decodes all integer types with both endianness modes" do
      test_values = [
        # type, value, little_endian_bytes, big_endian_bytes
        {"n", 0x1234, <<0x34, 0x12>>, <<0x12, 0x34>>},
        {"q", 0x1234, <<0x34, 0x12>>, <<0x12, 0x34>>},
        {"u", 0x12345678, <<0x78, 0x56, 0x34, 0x12>>, <<0x12, 0x34, 0x56, 0x78>>},
        {"i", 0x12345678, <<0x78, 0x56, 0x34, 0x12>>, <<0x12, 0x34, 0x56, 0x78>>}
      ]

      for {type, value, little_bytes, big_bytes} <- test_values do
        # Test little endian
        result_little = Decoder.decode(type, little_bytes, :little)
        assert result_little == [value], "Failed for type #{type} with little endian"

        # Test big endian
        result_big = Decoder.decode(type, big_bytes, :big)
        assert result_big == [value], "Failed for type #{type} with big endian"
      end
    end

    test "decodes 64-bit values with both endianness" do
      value = 0x123456789ABCDEF0
      little_bytes = <<0xF0, 0xDE, 0xBC, 0x9A, 0x78, 0x56, 0x34, 0x12>>
      big_bytes = <<0x12, 0x34, 0x56, 0x78, 0x9A, 0xBC, 0xDE, 0xF0>>

      # Test int64
      result_x_little = Decoder.decode("x", little_bytes, :little)
      assert result_x_little == [value]

      result_x_big = Decoder.decode("x", big_bytes, :big)
      assert result_x_big == [value]

      # Test uint64
      result_t_little = Decoder.decode("t", little_bytes, :little)
      assert result_t_little == [value]

      result_t_big = Decoder.decode("t", big_bytes, :big)
      assert result_t_big == [value]
    end

    test "decodes complex structures with different endianness" do
      original = [[1000, "test", 2000]]

      # Little endian
      encoded_little = Encoder.encode("(isi)", original, :little)
      data_little = IO.iodata_to_binary(encoded_little)
      result_little = Decoder.decode("(isi)", data_little, :little)
      assert result_little == original

      # Big endian
      encoded_big = Encoder.encode("(isi)", original, :big)
      data_big = IO.iodata_to_binary(encoded_big)
      result_big = Decoder.decode("(isi)", data_big, :big)
      assert result_big == original
    end
  end

  describe "error resilience" do
    test "handles empty signature" do
      result = Decoder.decode("", <<>>)
      assert result == []
    end

    test "decodes zero values correctly" do
      # All zero values should decode properly
      zero_tests = [
        {"y", <<0>>, [0]},
        {"n", <<0, 0>>, [0]},
        {"q", <<0, 0>>, [0]},
        {"i", <<0, 0, 0, 0>>, [0]},
        {"u", <<0, 0, 0, 0>>, [0]},
        {"b", <<0, 0, 0, 0>>, [false]},
        {"h", <<0, 0, 0, 0>>, [0]}
      ]

      for {sig, data, expected} <- zero_tests do
        result = Decoder.decode(sig, data)
        assert result == expected, "Failed for signature #{sig}"
      end
    end

    test "decodes array elements at exact boundaries" do
      # Test array where elements exactly fill the declared length
      encoded = Encoder.encode("ay", [[1, 2, 3, 4]])
      data = IO.iodata_to_binary(encoded)

      result = Decoder.decode("ay", data)
      assert result == [[1, 2, 3, 4]]
    end
  end
end
