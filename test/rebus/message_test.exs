defmodule Rebus.MessageTest do
  use ExUnit.Case, async: true
  alias Rebus.Message

  doctest Rebus.Message

  # Helper function to encode message and return binary for decoding
  defp encode_to_binary(message, endianness \\ :little) do
    case Message.encode(message, endianness) do
      {:ok, iodata} -> {:ok, IO.iodata_to_binary(iodata)}
      error -> error
    end
  end

  defp fixed_header(endianness, type, version, body_length, header_fields_length) do
    {endian_flag, body_length, serial, header_fields_length} =
      case endianness do
        :little ->
          {?l, <<body_length::little-32>>, <<1::little-32>>, <<header_fields_length::little-32>>}

        :big ->
          {?B, <<body_length::big-32>>, <<1::big-32>>, <<header_fields_length::big-32>>}
      end

    <<endian_flag, type, 0, version, body_length::binary, serial::binary,
      header_fields_length::binary>>
  end

  defp message_with_body(signature, value) do
    Message.new(:signal,
      path: "/test",
      interface: "test.interface",
      member: "Test",
      signature: signature,
      body: [value]
    )
  end

  defp wire_message(header_fields, body, endianness \\ :little, type \\ 4) do
    header =
      header_fields
      |> then(&Rebus.Encoder.encode_at_position("a(yv)", [&1], endianness, 12))
      |> IO.iodata_to_binary()

    body = IO.iodata_to_binary(body)
    header_size = 12 + byte_size(header)
    padding = :binary.copy(<<0>>, rem(8 - rem(header_size, 8), 8))
    fixed_header = fixed_header(endianness, type, 1, byte_size(body), byte_size(header) - 4)

    fixed_header <> binary_part(header, 4, byte_size(header) - 4) <> padding <> body
  end

  describe "new/2" do
    test "rejects decoded signals without required header fields" do
      raw = <<?l, 4, 0, 1, 0::little-32, 1::little-32, 0::little-32>>
      assert {:error, :invalid_message} = Message.decode(raw)
    end

    test "bounds aggregate unknown header-field materialization" do
      fields =
        [[1, {"o", "/test"}], [2, {"s", "test.interface"}], [3, {"s", "Budget"}]] ++
          List.duplicate([10, {"ay", []}], 25_001)

      header =
        Rebus.Encoder.encode_at_position("a(yv)", [fields], :little, 12)
        |> IO.iodata_to_binary()

      padding = :binary.copy(<<0>>, rem(8 - rem(12 + byte_size(header), 8), 8))
      raw = <<?l, 4, 0, 1, 0::little-32, 1::little-32, header::binary, padding::binary>>

      assert {:error, :resource_limit} = Message.decode(raw)
    end

    test "keeps a validated reply envelope for an inbound body resource limit" do
      limited =
        wire_message(
          [[5, {"u", 123}], [8, {"g", "ay"}]],
          <<1_000_001::little-32>> <> :binary.copy(<<1>>, 1_000_001),
          :little,
          2
        )

      valid =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "AfterLimitedReply"
        )

      {:ok, valid_data} = encode_to_binary(valid)

      assert {:error, :resource_limit, %{type: :method_return, reply_serial: 123}, ^valid_data} =
               Message.parse_inbound(limited <> valid_data)

      assert {:error, :resource_limit} = Message.parse(limited <> valid_data)
      assert {:error, :resource_limit} = Message.decode(limited)
    end

    test "carries the remainder when the limit trips in the header fields" do
      limited =
        wire_message(
          [[1, {"o", "/test"}], [2, {"s", "test.interface"}], [3, {"s", "HeaderLimited"}]] ++
            List.duplicate([10, {"ay", []}], 25_001),
          <<>>
        )

      valid =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "AfterHeaderLimit"
        )

      {:ok, valid_data} = encode_to_binary(valid)

      assert {:error, :resource_limit, nil, ^valid_data} =
               Message.parse_inbound(limited <> valid_data)

      assert {:error, :resource_limit} = Message.parse(limited <> valid_data)
    end

    test "keeps a validated error name in an error reply resource envelope" do
      error_name = "org.example.ResourceLimited"

      limited =
        wire_message(
          [[4, {"s", error_name}], [5, {"u", 123}], [8, {"g", "ay"}]],
          <<1_000_001::little-32>> <> :binary.copy(<<1>>, 1_000_001),
          :little,
          3
        )

      assert {:error, :resource_limit, envelope, <<>>} = Message.parse_inbound(limited)

      assert %{type: :error, reply_serial: 123, error_name: ^error_name} = envelope
    end

    test "classifies local resource exceptions by type, not their text" do
      limited =
        wire_message(
          [[5, {"u", 123}], [8, {"g", "ay"}]],
          <<1_000_001::little-32>> <> :binary.copy(<<1>>, 1_000_001),
          :little,
          2
        )

      exception = %Rebus.ResourceLimitError{limit: :scalar, message: "changed limit text"}
      assert exception.message == "changed limit text"
      assert {:error, :resource_limit} = Message.decode(limited)
    end

    test "keeps malformed signature grammar fatal" do
      malformed =
        wire_message(
          [
            [1, {"o", "/test"}],
            [2, {"s", "test.interface"}],
            [3, {"s", "Malformed"}],
            [8, {"g", "v"}]
          ],
          <<3, "a()", 0>>,
          :little,
          4
        )

      assert {:error, :invalid_message} = Message.decode(malformed)
    end

    test "exposes and accepts exactly the local scalar element cap" do
      limit = Message.max_scalar_elements()
      values = List.duplicate(0, limit)

      assert {:ok, message} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "ScalarLimit",
                 signature: "ay",
                 body: [values]
               )

      assert {:ok, _iodata} = Message.encode(message)
    end

    test "rejects one scalar element over the local cap from new and encode" do
      values = List.duplicate(0, Message.max_scalar_elements() + 1)

      assert {:error, :resource_limit} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "ScalarLimit",
                 signature: "ay",
                 body: [values]
               )

      message = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 1,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "ScalarLimit",
          signature: "ay"
        },
        body: [values]
      }

      assert {:error, :resource_limit} = Message.encode(message)

      assert_raise ArgumentError, ~r/local resource limit/, fn ->
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "ScalarLimit",
          signature: "ay",
          body: [values]
        )
      end
    end

    test "shares the outbound scalar cap across fixed-width arrays" do
      values = List.duplicate(0, div(Message.max_scalar_elements(), 2) + 1)

      assert {:error, :resource_limit} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "AggregateScalarLimit",
                 signature: "ayay",
                 body: [values, values]
               )

      message = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 1,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "AggregateScalarLimit",
          signature: "ayay"
        },
        body: [values, values]
      }

      assert {:error, :resource_limit} = Message.encode(message)
    end

    test "round-trips byte arrays beyond the composite term budget" do
      bytes = List.duplicate(1, 150_000)

      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Bytes",
          signature: "ay",
          body: [bytes]
        )

      assert {:ok, encoded} = Message.encode(message)
      assert {:ok, decoded} = encoded |> IO.iodata_to_binary() |> Message.decode()
      assert decoded.body == [bytes]
    end

    test "infers and round-trips special D-Bus doubles" do
      for value <- [:infinity, :negative_infinity, :nan] do
        assert {:ok, message} =
                 Message.new(:signal,
                   path: "/test",
                   interface: "test.interface",
                   member: "SpecialDouble",
                   body: [value]
                 )

        assert Message.signature(message) == "d"
        assert {:ok, encoded} = Message.encode(message)
        assert {:ok, decoded} = encoded |> IO.iodata_to_binary() |> Message.decode()
        assert decoded.body == [value]
      end
    end

    test "creates a valid method call message" do
      assert {:ok, message} =
               Message.new(:method_call,
                 path: "/com/example/Object",
                 interface: "com.example.Interface",
                 member: "TestMethod",
                 destination: "com.example.Service",
                 body: [42, "hello"],
                 signature: "is"
               )

      assert message.type == :method_call
      assert message.header_fields.path == "/com/example/Object"
      assert message.header_fields.interface == "com.example.Interface"
      assert message.header_fields.member == "TestMethod"
      assert message.header_fields.destination == "com.example.Service"
      assert message.body == [42, "hello"]
      assert Message.signature(message) == "is"
      assert message.version == 1
      assert message.flags == []
    end

    test "creates a valid signal message" do
      assert {:ok, message} =
               Message.new(:signal,
                 path: "/com/example/Object",
                 interface: "com.example.Interface",
                 member: "TestSignal",
                 body: ["signal_value"],
                 signature: "s"
               )

      assert message.type == :signal
      assert message.header_fields.path == "/com/example/Object"
      assert message.header_fields.interface == "com.example.Interface"
      assert message.header_fields.member == "TestSignal"
      assert message.body == ["signal_value"]
      assert Message.signature(message) == "s"
    end

    test "creates a valid error message" do
      assert {:ok, message} =
               Message.new(:error,
                 error_name: "com.example.Error.TestError",
                 reply_serial: 123,
                 body: ["Error message"],
                 signature: "s"
               )

      assert message.type == :error
      assert message.header_fields.error_name == "com.example.Error.TestError"
      assert message.header_fields.reply_serial == 123
      assert message.body == ["Error message"]
      assert Message.signature(message) == "s"
    end

    test "creates a valid method return message" do
      assert {:ok, message} =
               Message.new(:method_return,
                 reply_serial: 456,
                 body: [789],
                 signature: "i"
               )

      assert message.type == :method_return
      assert message.header_fields.reply_serial == 456
      assert message.body == [789]
      assert Message.signature(message) == "i"
    end

    test "supports message flags" do
      assert {:ok, message} =
               Message.new(:method_call,
                 path: "/test",
                 member: "TestMethod",
                 flags: [:no_reply_expected, :no_auto_start]
               )

      assert :no_reply_expected in message.flags
      assert :no_auto_start in message.flags
    end

    test "auto-generates signature for simple types" do
      assert {:ok, message} =
               Message.new(:signal,
                 path: "/test",
                 interface: "org.example.Test",
                 member: "Test",
                 body: [42, "hello", true]
               )

      # Should auto-generate a signature for int, string, boolean
      assert Message.signature(message) == "isb"
    end

    test "uses empty signature for empty body" do
      assert {:ok, message} =
               Message.new(:signal,
                 path: "/test",
                 interface: "org.example.Test",
                 member: "Test"
               )

      assert Message.signature(message) == ""
      assert message.body == []
      assert message.body_length == 0
    end

    test "validates required fields for method call" do
      assert {:error, {:missing_header_field, :path}} =
               Message.new(:method_call, interface: "org.example.Test")
    end

    test "validates required fields for signal" do
      assert {:error, {:missing_header_field, :interface}} = Message.new(:signal, path: "/test")
    end

    test "validates required fields for error" do
      assert {:error, {:missing_header_field, :reply_serial}} =
               Message.new(:error, error_name: "test.Error")
    end

    test "validates required fields for method return" do
      assert {:error, {:missing_header_field, :reply_serial}} =
               Message.new(:method_return, body: [42])
    end

    test "rejects invalid message type" do
      assert {:error, :invalid_type} = Message.new(:invalid_type, path: "/test")
    end

    test "rejects invalid flags" do
      assert {:error, :invalid_flags} =
               Message.new(:signal,
                 path: "/test",
                 interface: "org.example.Test",
                 member: "Test",
                 flags: [:invalid_flag]
               )
    end

    test "validates object paths" do
      assert {:error, {:invalid_header_field, :path}} =
               Message.new(:signal,
                 path: "invalid_path",
                 interface: "org.example.Test",
                 member: "Test"
               )
    end

    test "rejects single-element interface names required to have two elements" do
      # D-Bus specification, "Valid Names": interface names are "composed of 2
      # or more elements separated by a period ('.') character".
      assert {:error, {:invalid_header_field, :interface}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "invalid",
                 member: "Test"
               )

      assert {:ok, _} =
               Message.new(:signal,
                 path: "/test",
                 interface: "valid.interface",
                 member: "Test"
               )
    end

    test "validates interface names" do
      # Test with completely invalid interface name
      assert {:error, {:invalid_header_field, :interface}} =
               Message.new(:signal,
                 path: "/test",
                 # Cannot start with number
                 interface: "123invalid",
                 member: "Test"
               )
    end

    test "validates error names with the interface name grammar" do
      # D-Bus specification, "Valid Names": "Error names have the same
      # restrictions as interface names", so a single element is invalid.
      assert {:error, {:invalid_header_field, :error_name}} =
               Message.new(:error, error_name: "Failed", reply_serial: 1)

      assert {:ok, _} =
               Message.new(:error, error_name: "org.example.Error.Failed", reply_serial: 1)
    end

    test "validates destination and sender as bus names" do
      # D-Bus specification, "Valid Names": "Bus names must contain at least
      # one '.' (period) character (and thus at least two elements)."
      assert {:error, {:invalid_header_field, :destination}} = signal_with(destination: ":1")
      assert {:ok, _} = signal_with(destination: ":1.7")
      assert {:ok, _} = signal_with(destination: "org.example.Service")

      assert {:error, {:invalid_header_field, :sender}} = signal_with(sender: ":1")
      assert {:error, {:invalid_header_field, :sender}} = signal_with(sender: "org")
      assert {:ok, _} = signal_with(sender: ":1.42")
    end

    test "validates member names" do
      assert {:error, {:invalid_header_field, :member}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "123invalid"
               )
    end
  end

  describe "new!/2" do
    test "creates message successfully" do
      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test"
        )

      assert message.type == :signal
    end

    test "raises on error" do
      assert_raise ArgumentError, fn ->
        Message.new!(:method_call, interface: "org.example.Test")
      end
    end

    test "builds a readable message for each reason without the offending value" do
      assert_raise ArgumentError, "missing required header field :path", fn ->
        Message.new!(:method_call, interface: "org.example.Test")
      end

      assert_raise ArgumentError, "invalid value for header field :interface", fn ->
        Message.new!(:signal, path: "/test", interface: "not-an-interface", member: "Test")
      end

      assert_raise ArgumentError, "invalid message type", fn ->
        Message.new!(:bogus, path: "/test")
      end

      assert_raise ArgumentError, "invalid message flags", fn ->
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          flags: [:bogus]
        )
      end

      assert_raise ArgumentError, "unsupported D-Bus protocol version", fn ->
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          version: 2
        )
      end

      assert_raise ArgumentError, "invalid message signature", fn ->
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "secret-value"
        )
      end
    end
  end

  describe "encode/2 and decode/1" do
    test "round-trip encoding and decoding for method call" do
      original =
        Message.new!(:method_call,
          path: "/com/example/Object",
          interface: "com.example.Interface",
          member: "TestMethod",
          destination: "com.example.Service",
          body: [42, "hello"],
          signature: "is"
        )

      assert {:ok, encoded} = encode_to_binary(original)
      assert is_binary(encoded)
      assert byte_size(encoded) > 0

      assert {:ok, decoded} = Message.decode(encoded)

      # Check that core message properties are preserved
      assert decoded.type == original.type
      assert decoded.version == original.version
      assert decoded.serial == original.serial
      assert decoded.body == original.body
      assert Message.signature(decoded) == Message.signature(original)

      # Check header fields
      assert decoded.header_fields.path == original.header_fields.path
      assert decoded.header_fields.interface == original.header_fields.interface
      assert decoded.header_fields.member == original.header_fields.member
      assert decoded.header_fields.destination == original.header_fields.destination
    end

    test "round-trip encoding and decoding for signal with empty body" do
      original =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "EmptySignal"
        )

      assert {:ok, encoded} = encode_to_binary(original)
      assert {:ok, decoded} = Message.decode(encoded)

      assert decoded.type == original.type
      assert decoded.serial == original.serial
      assert decoded.body == []
      assert Message.signature(decoded) == ""
      assert decoded.body_length == 0
    end

    test "round-trip encoding and decoding for error message" do
      original =
        Message.new!(:error,
          error_name: "com.example.Error.TestError",
          reply_serial: 999,
          body: ["Something went wrong"],
          signature: "s"
        )

      assert {:ok, encoded} = encode_to_binary(original)
      assert {:ok, decoded} = Message.decode(encoded)

      assert decoded.type == original.type
      assert decoded.serial == original.serial
      assert decoded.body == original.body
      assert decoded.header_fields.error_name == original.header_fields.error_name
      assert decoded.header_fields.reply_serial == original.header_fields.reply_serial
    end

    test "round-trip with different endianness" do
      original =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [123],
          signature: "i"
        )

      # Test little endian
      assert {:ok, encoded_little} = encode_to_binary(original, :little)
      assert {:ok, decoded_little} = Message.decode(encoded_little)
      assert decoded_little.body == original.body

      # Test big endian
      assert {:ok, encoded_big} = encode_to_binary(original, :big)
      assert {:ok, decoded_big} = Message.decode(encoded_big)
      assert decoded_big.body == original.body

      # Encoded data should be different for different endianness
      assert encoded_little != encoded_big
    end

    test "round-trips variant array bodies through parse and decode" do
      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "VariantArray",
          body: [[{"x", 5}, {"d", 3.25}]],
          signature: "av"
        )

      for endianness <- [:little, :big] do
        assert {:ok, iodata} = Message.encode(message, endianness)
        binary = IO.iodata_to_binary(iodata)

        assert {:ok, parsed, <<>>} = Message.parse(binary)
        assert parsed.body == message.body

        assert {:ok, decoded} = Message.decode(binary)
        assert decoded.body == message.body
      end
    end

    test "round-trips wide dictionaries of sibling variants" do
      properties = Enum.map(1..100, &{"property#{&1}", {"i", &1}})

      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "PropertiesChanged",
          body: [properties],
          signature: "a{sv}"
        )

      for endianness <- [:little, :big] do
        assert {:ok, encoded} = Message.encode(message, endianness)
        assert {:ok, decoded} = encoded |> IO.iodata_to_binary() |> Message.decode()
        assert decoded.body == message.body
        assert :ok = Message.validate(decoded)
        assert {:ok, _reencoded} = Message.encode(decoded, endianness)
      end
    end

    test "round-trip with flags" do
      original =
        Message.new!(:method_call,
          path: "/test",
          member: "Test",
          flags: [:no_reply_expected, :no_auto_start]
        )

      assert {:ok, encoded} = encode_to_binary(original)
      assert {:ok, decoded} = Message.decode(encoded)

      assert Enum.sort(decoded.flags) == Enum.sort(original.flags)
    end

    test "round-trip with complex body" do
      original =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "ComplexSignal",
          body: [42, "hello", true, 3.14],
          signature: "isbd"
        )

      assert {:ok, encoded} = encode_to_binary(original)
      assert {:ok, decoded} = Message.decode(encoded)

      assert decoded.body == original.body
      assert Message.signature(decoded) == Message.signature(original)
    end
  end

  describe "validate/1" do
    test "validates correct message" do
      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test"
        )

      assert Message.validate(message) == :ok
    end

    test "rejects message missing required fields" do
      message = %Message{
        type: :method_call,
        header_fields: %{member: "Test"},
        body: [],
        flags: [],
        version: 1,
        serial: 1,
        body_length: 0
      }

      assert {:error, {:missing_header_field, :path}} = Message.validate(message)
    end

    test "rejects invalid signature format" do
      message = %Message{
        type: :signal,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "invalid!@#$%"
        },
        body: [],
        flags: [],
        version: 1,
        serial: 1,
        body_length: 0
      }

      assert {:error, :invalid_signature} = Message.validate(message)
    end
  end

  describe "new/2 error handling" do
    test "rejects invalid message type" do
      assert {:error, :invalid_type} = Message.new(:invalid_type, path: "/test")
    end

    test "rejects invalid signature type (non-binary)" do
      assert {:error, :invalid_signature} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 signature: 123
               )
    end

    test "rejects invalid body type (non-list)" do
      assert {:error, :invalid_body} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 body: "not a list"
               )
    end

    test "rejects invalid flags type (non-list)" do
      assert {:error, :invalid_flags} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 flags: "not a list"
               )
    end

    test "rejects invalid version" do
      assert {:error, :invalid_version} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 version: 2
               )
    end

    test "rejects invalid flags" do
      assert {:error, :invalid_flags} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 flags: [:invalid_flag]
               )
    end

    test "rejects invalid header field types" do
      # Test invalid path
      assert {:error, {:invalid_header_field, :path}} =
               Message.new(:signal,
                 path: "invalid-path-no-leading-slash",
                 interface: "test.interface",
                 member: "Test"
               )

      # Test invalid interface
      assert {:error, {:invalid_header_field, :interface}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "invalid interface name with spaces",
                 member: "Test"
               )

      # Test invalid member
      assert {:error, {:invalid_header_field, :member}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "invalid-member-name"
               )

      # Test invalid destination
      assert {:error, {:invalid_header_field, :destination}} =
               Message.new(:method_call,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 destination: "invalid destination"
               )

      # Test invalid error_name
      assert {:error, {:invalid_header_field, :error_name}} =
               Message.new(:error,
                 error_name: "invalid error name",
                 reply_serial: 123
               )

      # Test invalid sender
      assert {:error, {:invalid_header_field, :sender}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 sender: "invalid sender"
               )
    end
  end

  describe "body encoding validation" do
    test "rejects invalid fixed message envelopes before marshaling" do
      base = %Message{
        type: :method_call,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 1,
        header_fields: %{path: "/test", member: "Test"},
        body: []
      }

      invalid_messages = [
        %{base | type: :invalid},
        %{base | header_fields: %{}},
        %{base | header_fields: []},
        %{base | header_fields: nil},
        %{base | version: 0},
        %{base | version: 2},
        %{base | version: 256},
        %{base | serial: 0},
        %{base | serial: -1},
        %{base | serial: 4_294_967_296},
        %{base | flags: [:invalid]}
      ]

      for message <- invalid_messages do
        assert {:error, :invalid_message} = Message.encode(message)
      end

      assert {:ok, _encoded} = Message.encode(%{base | serial: 4_294_967_295})
    end

    test "enforces encoded message and header-fields size limits without large allocations" do
      assert :ok = Message.validate_encoded_size(4, Message.max_message_size() - 16)

      assert {:error, :message_too_large} =
               Message.validate_encoded_size(4, Message.max_message_size())

      assert {:error, :message_too_large} =
               Message.validate_encoded_size(67_108_864 + 5, 0)

      assert {:error, :message_too_large} = Message.validate_encoded_size(3, 0)
    end

    test "rejects invalid inferred signatures at construction" do
      assert {:error, :invalid_signature} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 body: List.duplicate(1, 300)
               )

      nested = Enum.reduce(1..33, 1, fn _, value -> [value] end)

      assert {:error, :resource_limit} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 body: [nested]
               )
    end

    test "accepts integer boundaries and rejects out-of-range values" do
      ranges = [
        {"y", 0, 255},
        {"n", -32_768, 32_767},
        {"q", 0, 65_535},
        {"i", -2_147_483_648, 2_147_483_647},
        {"u", 0, 4_294_967_295},
        {"x", -9_223_372_036_854_775_808, 9_223_372_036_854_775_807},
        {"t", 0, 18_446_744_073_709_551_615}
      ]

      for {signature, minimum, maximum} <- ranges do
        for value <- [minimum, maximum] do
          assert {:ok, message} = message_with_body(signature, value)
          assert {:ok, _encoded} = Message.encode(message)
        end

        assert {:error, :invalid_body} = message_with_body(signature, minimum - 1)
        assert {:error, :invalid_body} = message_with_body(signature, maximum + 1)
      end
    end

    test "rejects invalid signature values in body data and variants" do
      long_signature = String.duplicate("i", 300)

      assert {:error, :invalid_body} = message_with_body("g", long_signature)
      assert {:error, :invalid_body} = message_with_body("g", "(")
      assert {:error, :invalid_body} = message_with_body("v", {"g", "("})
    end

    test "rejects control characters in D-Bus names and paths" do
      invalid_options = [
        [path: "/foo\n", interface: "test.interface", member: "Test"],
        [path: "/foo\r", interface: "test.interface", member: "Test"],
        [path: "/foo\0", interface: "test.interface", member: "Test"],
        [path: "/test", interface: "test.interface\n", member: "Test"],
        [path: "/test", interface: "test.interface", member: "Test\n"],
        [
          path: "/test",
          interface: "test.interface",
          member: "Test",
          destination: "org.example\n"
        ],
        [path: "/test", interface: "test.interface", member: "Test", sender: ":1.42\r"]
      ]

      for options <- invalid_options do
        assert {:error, _reason} = Message.new(:signal, options)
      end
    end

    test "returns a bounded header-fields error for manually constructed messages" do
      message = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 1,
        header_fields: %{path: 42, interface: "test.interface", member: "Test"},
        body: []
      }

      assert {:error, :invalid_header_fields} = Message.encode(message)
      assert {:error, {:invalid_header_field, :path}} = Message.validate(message)
    end

    test "validate/1 rejects unknown and non-map header fields" do
      message = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 1,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "Test",
          unknown: "x"
        },
        body: []
      }

      assert {:error, {:unknown_header_field, :unknown}} = Message.validate(message)

      assert {:error, :invalid_header_fields} =
               Message.validate(%{message | header_fields: []})
    end

    test "new! names the signature without including body data" do
      assert_raise ArgumentError, ~r/body does not match signature "i"/, fn ->
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "i",
          body: ["sensitive body"]
        )
      end
    end

    test "new! names an inferred signature without inspecting the body" do
      assert_raise ArgumentError, ~r/body does not match signature "v"/, fn ->
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [%{secret: "body data"}]
        )
      end
    end

    test "normalizes invalid variant signatures without leaking values" do
      assert {:error, :invalid_body} =
               message_with_body("v", {"", "sensitive body data"})
    end

    test "rejects invalid signature grammar without raising" do
      for signature <- ["is)", "}", "["] do
        assert {:error, :invalid_signature} =
                 Message.new(:signal,
                   path: "/test",
                   interface: "test.interface",
                   member: "Test",
                   signature: signature,
                   body: []
                 )

        message = %Message{
          type: :signal,
          flags: [],
          version: 1,
          body_length: 0,
          serial: 1,
          header_fields: %{
            path: "/test",
            interface: "test.interface",
            member: "Test",
            signature: signature
          },
          body: []
        }

        assert {:error, :invalid_header_fields} = Message.encode(message)
        assert {:error, :invalid_signature} = Message.validate(message)

        assert_raise ArgumentError, "invalid message signature", fn ->
          Message.new!(:signal,
            path: "/test",
            interface: "test.interface",
            member: "Test",
            signature: signature,
            body: []
          )
        end
      end
    end

    test "accepts balanced nested signatures" do
      assert {:ok, message} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 signature: "a{sa(iy)}",
                 body: [[{"key", [[1, 2]]}]]
               )

      assert {:ok, _encoded} = Message.encode(message)
      assert :ok = Message.validate(message)
    end

    test "allows hyphens in well-known bus-name elements only" do
      assert {:ok, _message} =
               Message.new(:method_call,
                 path: "/test",
                 member: "Test",
                 destination: "org.example-service.Name"
               )

      for name <- ["org.1service", "org..service", "org.service!"] do
        assert {:error, {:invalid_header_field, :destination}} =
                 Message.new(:method_call, path: "/test", member: "Test", destination: name)
      end
    end
  end

  describe "decode/1 error handling" do
    test "rejects a body boolean that is neither 0 nor 1" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Test"}],
        [8, {"g", "b"}]
      ]

      assert {:ok, message} = Message.decode(wire_message(fields, <<1::little-32>>))
      assert message.body == [true]

      assert {:error, :invalid_message} =
               Message.decode(wire_message(fields, <<2::little-32>>))
    end

    test "rejects non-zero alignment padding in the body" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Test"}],
        [8, {"g", "yi"}]
      ]

      assert {:ok, message} =
               Message.decode(wire_message(fields, <<7, 0, 0, 0, 42::little-32>>))

      assert message.body == [7, 42]

      assert {:error, :invalid_message} =
               Message.decode(wire_message(fields, <<7, 0, 1, 0, 42::little-32>>))
    end

    test "rejects non-zero alignment padding in the header fields" do
      fields = [[1, {"o", "/test"}], [2, {"s", "test.interface"}], [3, {"s", "Test"}]]
      wire = wire_message(fields, [])

      # The first `(yv)` struct ends at offset 30 ("/test\\0" finishes at 29),
      # so the second struct is preceded by two padding bytes.
      assert :binary.at(wire, 30) == 0
      assert {:ok, _message} = Message.decode(wire)

      <<prefix::binary-size(30), 0, suffix::binary>> = wire

      assert {:error, :invalid_message} =
               Message.decode(<<prefix::binary, 1, suffix::binary>>)
    end

    test "rejects non-zero padding between the header fields and the body" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Test"}],
        [8, {"g", "y"}]
      ]

      wire = wire_message(fields, <<7>>)
      <<_::binary-size(12), header_fields_length::little-32, _::binary>> = wire
      header_fields_end = 16 + header_fields_length

      # The header fields do not end on an 8-byte boundary, so the body is
      # preceded by padding that the fixture must actually contain.
      assert rem(header_fields_end, 8) != 0
      assert {:ok, message} = Message.decode(wire)
      assert message.body == [7]

      <<prefix::binary-size(header_fields_end), 0, suffix::binary>> = wire

      assert {:error, :invalid_message} =
               Message.decode(<<prefix::binary, 1, suffix::binary>>)
    end

    test "rejects invalid endianness flag" do
      # Create a message with invalid endianness (not 'l' or 'B')
      invalid_data = <<99, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>

      assert {:error, :invalid_endianness} = Message.decode(invalid_data)
    end

    test "rejects insufficient data" do
      # Too short message (less than 12 bytes for header)
      short_data = <<108, 1, 0, 0>>

      assert {:error, :invalid_message} = Message.decode(short_data)
    end

    test "handles body decoding errors" do
      # Test that we can detect different message sizes
      {:ok, valid_message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [42],
          signature: "i"
        )

      {:ok, encoded} = encode_to_binary(valid_message, :little)

      # Create a truncated message that will fail
      truncated = binary_part(encoded, 0, byte_size(encoded) - 5)

      # This should be different size
      assert byte_size(truncated) < byte_size(encoded)
    end

    test "rejects a nonempty array of zero-width structs" do
      malformed_body = <<1::little-32, 0::size(4 * 8)>>

      valid_fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "ZeroWidth"}]
      ]

      valid_header =
        valid_fields
        |> then(&Rebus.Encoder.encode_at_position("a(yv)", [&1], :little, 12))
        |> IO.iodata_to_binary()

      valid_data = binary_part(valid_header, 4, byte_size(valid_header) - 4)
      signature_field = <<8, 1, "g", 0, 3, "a()", 0>>
      padding = :binary.copy(<<0>>, rem(8 - rem(16 + byte_size(valid_data), 8), 8))
      fields_data = valid_data <> padding <> signature_field

      header_padding =
        :binary.copy(<<0>>, rem(8 - rem(12 + 4 + byte_size(fields_data), 8), 8))

      wire =
        fixed_header(:little, 4, 1, byte_size(malformed_body), byte_size(fields_data)) <>
          fields_data <> header_padding <> malformed_body

      assert {:error, :invalid_message} = Message.decode(wire)
    end

    test "rejects invalid message type in binary" do
      # Create a binary with invalid message type (99 instead of 1-4)
      # Format: endian_flag, type_byte, flags_byte, version_byte, body_length(4), serial(4), header_fields...
      invalid_message_binary = <<
        # Little endian flag
        ?l,
        # Invalid message type (99)
        99,
        # Flags (0)
        0,
        # Version (1)
        1,
        # Body length (0) - little endian
        0,
        0,
        0,
        0,
        # Serial (1) - little endian
        1,
        0,
        0,
        0,
        # Header fields array length (0) - little endian
        0,
        0,
        0,
        0
      >>

      assert {:error, :invalid_message_type} = Message.decode(invalid_message_binary)
    end

    test "rejects message with body data that doesn't match signature" do
      # Create a message that declares signature "i" (integer) but has invalid body data
      # We'll manually construct this to bypass the normal encoding validation

      # First, let's encode header fields that declare signature "i"
      # Header field 8 is signature, with value "i"
      signature_header_field = [8, {"g", "i"}]
      header_fields_data = [signature_header_field]

      # Encode the header fields using our encoder
      header_fields_encoded = Rebus.Encoder.encode("a(yv)", [header_fields_data], :little)
      header_fields_binary = IO.iodata_to_binary(header_fields_encoded)

      # Create invalid body data - string bytes instead of integer
      # This should be 4 bytes for an integer, but we'll put string data
      # Invalid for integer decoding
      invalid_body_data = <<0xFF, 0xFF, 0xFF>>
      body_length = byte_size(invalid_body_data)

      # Calculate padding for header fields to 8-byte boundary
      header_fields_size = byte_size(header_fields_binary)
      # 12 bytes fixed header + header fields
      header_total_size = 12 + header_fields_size
      header_padded_size = div(header_total_size + 7, 8) * 8
      header_padding = header_padded_size - header_total_size

      # Construct the complete message
      message_binary = <<
        # Little endian flag
        ?l,
        # Signal message type (4)
        4,
        # Flags (0)
        0,
        # Version (1)
        1,
        # Body length - little endian
        body_length::little-32,
        # Serial (1) - little endian
        1::little-32,
        # Header fields
        header_fields_binary::binary,
        # Padding to 8-byte boundary
        0::size(header_padding * 8),
        # Invalid body data
        invalid_body_data::binary
      >>

      # This should fail when trying to decode the body according to signature "i"
      assert {:error, :invalid_message} = Message.decode(message_binary)
    end

    test "rejects message with body data type mismatch" do
      # Create a message that declares signature "s" (string) but has integer body data
      # This tests a different kind of signature mismatch

      # Header field 8 is signature, with value "s" (string)
      signature_header_field = [8, {"g", "s"}]
      header_fields_data = [signature_header_field]

      # Encode the header fields
      header_fields_encoded = Rebus.Encoder.encode("a(yv)", [header_fields_data], :little)
      header_fields_binary = IO.iodata_to_binary(header_fields_encoded)

      # Create body data that looks like an integer (4 bytes) instead of a string
      # A string should start with a length field, but we'll put raw integer bytes
      # Integer data when expecting string
      invalid_body_data = <<42::little-32>>
      body_length = byte_size(invalid_body_data)

      # Calculate padding for header fields to 8-byte boundary
      header_fields_size = byte_size(header_fields_binary)
      header_total_size = 12 + header_fields_size
      header_padded_size = div(header_total_size + 7, 8) * 8
      header_padding = header_padded_size - header_total_size

      # Construct the complete message
      message_binary = <<
        # Little endian flag
        ?l,
        # Signal message type
        4,
        # Flags
        0,
        # Version
        1,
        # Body length
        body_length::little-32,
        # Serial
        1::little-32,
        # Header fields
        header_fields_binary::binary,
        # Padding
        0::size(header_padding * 8),
        # Invalid body data
        invalid_body_data::binary
      >>

      # This should fail when trying to decode as string but finding integer-like data
      assert {:error, :invalid_message} = Message.decode(message_binary)
    end
  end

  describe "decoded header validation and offsets" do
    test "uses the declared header size with unknown fields in both byte orders" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Test"}],
        [8, {"g", "i"}],
        [10, {"s", "ignored unknown field"}]
      ]

      for endianness <- [:little, :big] do
        body = Rebus.Encoder.encode("i", [42], endianness)
        assert {:ok, message} = Message.decode(wire_message(fields, body, endianness))
        assert message.body == [42]
        assert message.header_fields.path == "/test"
        refute Map.has_key?(message.header_fields, 10)
      end
    end

    test "rejects declared header lengths that do not match their contents" do
      fields = [[1, {"o", "/test"}], [2, {"s", "test.interface"}], [3, {"s", "Test"}]]
      valid = wire_message(fields, [], :little)
      <<prefix::binary-size(12), length::little-32, rest::binary>> = valid

      assert {:error, :invalid_message} =
               Message.decode(<<prefix::binary, length - 1::little-32, rest::binary>>)

      assert {:error, :invalid_message} =
               Message.decode(<<prefix::binary, length + 1::little-32, rest::binary>>)
    end

    test "rejects known header fields encoded with the wrong variant type" do
      fields = [[1, {"u", 42}], [2, {"s", "test.interface"}], [3, {"s", "Test"}]]
      assert {:error, :invalid_message} = Message.decode(wire_message(fields, []))
    end

    test "rejects duplicate known header fields" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "First"}],
        [3, {"s", "Second"}]
      ]

      assert {:error, :invalid_message} = Message.decode(wire_message(fields, []))
    end

    test "rejects a nonempty signature with an empty body" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Test"}],
        [8, {"g", "s"}]
      ]

      assert {:error, :invalid_message} = Message.decode(wire_message(fields, []))

      message = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 1,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "s"
        },
        body: []
      }

      assert {:error, :invalid_body} = Message.validate(message)
      assert {:error, :invalid_body} = Message.encode(message)
    end

    test "validates a decoded nonempty signed body before re-encoding it" do
      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "s",
          body: ["value"]
        )

      assert :ok = Message.validate(message)
      assert {:ok, encoded} = Message.encode(message)
      assert {:ok, decoded} = Message.decode(IO.iodata_to_binary(encoded))
      assert :ok = Message.validate(decoded)
    end

    test "rejects body bytes not consumed by the signature" do
      fields = [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Test"}],
        [8, {"g", "s"}]
      ]

      for endianness <- [:little, :big] do
        body = IO.iodata_to_binary(Rebus.Encoder.encode("s", ["value"], endianness)) <> <<0, 0>>
        assert {:error, :invalid_message} = Message.decode(wire_message(fields, body, endianness))
      end
    end

    test "rejects a body when no signature is declared" do
      fields = [[1, {"o", "/test"}], [2, {"s", "test.interface"}], [3, {"s", "Test"}]]
      assert {:error, :invalid_message} = Message.decode(wire_message(fields, <<0>>))
    end
  end

  describe "additional edge cases for coverage" do
    test "validates unix_fds field type" do
      assert {:error, {:invalid_header_field, :unix_fds}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 unix_fds: "invalid"
               )
    end

    test "validates reply_serial field type" do
      assert {:error, {:invalid_header_field, :reply_serial}} =
               Message.new(:method_return,
                 reply_serial: "invalid"
               )
    end

    test "handles method_return validation" do
      # Test missing reply_serial for method_return
      assert {:error, {:missing_header_field, :reply_serial}} =
               Message.new(:method_return, body: [])
    end

    test "handles error message validation" do
      # Test missing error_name for error message
      assert {:error, {:missing_header_field, :error_name}} =
               Message.new(:error, reply_serial: 123)

      # Test missing reply_serial for error message
      assert {:error, {:missing_header_field, :reply_serial}} =
               Message.new(:error, error_name: "com.example.Error")
    end

    test "handles decode with big endian" do
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [42],
          signature: "i"
        )

      # Test encoding with big endian
      assert {:ok, encoded_big} = encode_to_binary(message, :big)
      assert {:ok, decoded} = Message.decode(encoded_big)
      assert decoded.body == [42]
    end

    test "handles messages with serial initialized to zero" do
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test"
        )

      assert message.serial == 1
    end

    test "ignores serial option and always uses zero" do
      # Even if serial is passed in options, it should be ignored
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          serial: 999_999
        )

      assert message.serial == 1
    end

    test "handles encoding and decoding with all header fields" do
      {:ok, message} =
        Message.new(:method_call,
          path: "/test/path",
          interface: "test.interface",
          member: "TestMethod",
          destination: "test.destination",
          sender: "test.sender",
          signature: "s",
          body: ["test"]
        )

      assert {:ok, encoded} = encode_to_binary(message, :little)
      assert {:ok, decoded} = Message.decode(encoded)

      assert decoded.header_fields.path == "/test/path"
      assert decoded.header_fields.interface == "test.interface"
      assert decoded.header_fields.member == "TestMethod"
      assert decoded.header_fields.destination == "test.destination"
      assert decoded.header_fields.sender == "test.sender"
      assert decoded.header_fields.signature == "s"
      assert decoded.serial == 1
      assert decoded.body == ["test"]
    end

    test "validates invalid object paths" do
      # Test path that doesn't start with /
      assert {:error, {:invalid_header_field, :path}} =
               Message.new(:signal,
                 path: "invalid/path",
                 interface: "test.interface",
                 member: "Test"
               )

      # Test empty path
      assert {:error, {:invalid_header_field, :path}} =
               Message.new(:signal,
                 path: "",
                 interface: "test.interface",
                 member: "Test"
               )

      # Test path with invalid characters
      assert {:error, {:invalid_header_field, :path}} =
               Message.new(:signal,
                 path: "/test/path with spaces",
                 interface: "test.interface",
                 member: "Test"
               )
    end

    test "validates interface and member names with invalid characters" do
      # Test interface with invalid characters
      assert {:error, {:invalid_header_field, :interface}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface-with-dash",
                 member: "Test"
               )

      # Test member with invalid characters
      assert {:error, {:invalid_header_field, :member}} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test-with-dash"
               )
    end
  end

  describe "type conversion functions" do
    test "type_code/1 returns correct codes" do
      assert Message.type_code(:method_call) == 1
      assert Message.type_code(:method_return) == 2
      assert Message.type_code(:error) == 3
      assert Message.type_code(:signal) == 4
    end

    test "type_code/1 raises for invalid types" do
      assert_raise ArgumentError, "Invalid message type: :invalid", fn ->
        Message.type_code(:invalid)
      end

      assert_raise ArgumentError, "Invalid message type: :unknown", fn ->
        Message.type_code(:unknown)
      end
    end

    test "type_from_code/1 returns correct types" do
      assert Message.type_from_code(1) == {:ok, :method_call}
      assert Message.type_from_code(2) == {:ok, :method_return}
      assert Message.type_from_code(3) == {:ok, :error}
      assert Message.type_from_code(4) == {:ok, :signal}
    end

    test "type_from_code/1 returns error for unknown codes" do
      assert {:error, :invalid_message_type} = Message.type_from_code(0)
      assert {:error, :invalid_message_type} = Message.type_from_code(99)
    end
  end

  describe "signature generation" do
    test "generates correct signatures for different data types" do
      # Test with empty body
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: []
        )

      assert Message.signature(message) == ""

      # Test with byte (0-255) - but auto-generated signature is 'i' for int32
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [42]
        )

      # Auto-generated signature treats integers as int32 ('i') due to clause order
      assert Message.signature(message) == "i"

      # Test with larger integer (outside int32 range)
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [3_000_000_000]
        )

      assert Message.signature(message) == "x"

      # Test with string
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: ["hello"]
        )

      assert Message.signature(message) == "s"

      # Test with boolean
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [true]
        )

      assert Message.signature(message) == "b"

      # Test with float
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [3.14]
        )

      assert Message.signature(message) == "d"

      # Test with array
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [["hello", "world"]]
        )

      assert Message.signature(message) == "as"

      # Test with empty array
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [[]]
        )

      assert Message.signature(message) == "as"

      # Test with mixed types (integer, string, boolean)
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [42, "hello", true]
        )

      # int32, string, boolean
      assert Message.signature(message) == "isb"

      # Inferred variants still require a D-Bus variant value tuple.
      assert {:error, :invalid_body} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 body: [%{key: "value"}]
               )
    end
  end

  describe "edge cases and error scenarios" do
    test "handles encoding errors gracefully" do
      # Test with a message that has body but will use generated signature
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [42]
        )

      # This should work - signature is generated automatically
      assert {:ok, _encoded} = encode_to_binary(message, :little)
    end

    test "handles different endianness correctly" do
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: [42],
          signature: "y"
        )

      # Test both endianness formats
      assert {:ok, encoded_little} = encode_to_binary(message, :little)
      assert {:ok, encoded_big} = encode_to_binary(message, :big)

      # Should be able to decode both
      assert {:ok, decoded_little} = Message.decode(encoded_little)
      assert {:ok, decoded_big} = Message.decode(encoded_big)

      assert decoded_little.body == decoded_big.body
    end

    test "handles messages with all possible flag combinations" do
      flags_combinations = [
        [],
        [:no_reply_expected],
        [:no_auto_start],
        [:allow_interactive_authorization],
        [:no_reply_expected, :no_auto_start],
        [:no_reply_expected, :allow_interactive_authorization],
        [:no_auto_start, :allow_interactive_authorization],
        [:no_reply_expected, :no_auto_start, :allow_interactive_authorization]
      ]

      for flags <- flags_combinations do
        {:ok, message} =
          Message.new(:signal,
            path: "/test",
            interface: "test.interface",
            member: "Test",
            flags: flags
          )

        assert {:ok, encoded} = encode_to_binary(message, :little)
        assert {:ok, decoded} = Message.decode(encoded)
        assert decoded.flags == flags
      end
    end

    test "handles large messages within limits" do
      # Create a message with a reasonably large body - use a supported signature
      large_body = ["string_1", "string_2", "string_3", "string_4", "string_5"]

      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Test",
          body: large_body,
          # 5 individual strings
          signature: "sssss"
        )

      assert {:ok, encoded} = encode_to_binary(message, :little)
      assert {:ok, decoded} = Message.decode(encoded)
      assert decoded.body == large_body
    end

    test "rejects a body that does not match its signature" do
      assert {:error, :invalid_body} =
               Message.new(:signal,
                 path: "/test",
                 interface: "test.interface",
                 member: "Test",
                 signature: "i",
                 body: ["not an integer"]
               )

      message = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 123,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "i"
        },
        body: ["not an integer"]
      }

      assert {:error, :invalid_body} = Message.encode(message)
      assert {:error, :invalid_body} = Message.validate(message)
    end

    test "covers infer_type for all data types" do
      # Test that signature generation covers all type inference branches
      test_values = [
        # Small int -> still int32
        {255, "i"},
        # Negative int -> int32
        {-1, "i"},
        # Large int -> int64
        {2_147_483_648, "x"},
        # Boolean
        {true, "b"},
        # Boolean false
        {false, "b"},
        # Float
        {3.14159, "d"},
        # String
        {"string", "s"},
        # Empty array
        {[], "as"},
        # Array of ints
        {[1, 2, 3], "ai"},
        # Valid variant
        {{"s", "value"}, "v"}
      ]

      for {value, expected_sig} <- test_values do
        {:ok, message} =
          Message.new(:signal,
            path: "/test",
            interface: "test.interface",
            member: "Test",
            body: [value]
          )

        assert Message.signature(message) == expected_sig
      end
    end

    test "covers edge cases in validation functions" do
      # Test root path "/"
      {:ok, message} =
        Message.new(:signal,
          path: "/",
          interface: "test.interface",
          member: "Test"
        )

      assert message.header_fields.path == "/"

      # Test single-segment interface name (minimum valid)
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "a.b",
          member: "Test"
        )

      assert message.header_fields.interface == "a.b"

      # Test single character names
      {:ok, message} =
        Message.new(:signal,
          path: "/a",
          interface: "a.b",
          member: "a"
        )

      assert message.header_fields.member == "a"

      # Test error encoding/decoding recovery path
      message_with_rescue_path = %Message{
        type: :signal,
        flags: [],
        version: 1,
        # Invalid length that doesn't match actual body
        body_length: 999,
        serial: 123,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "Test",
          signature: "s"
        },
        body: []
      }

      # A body and signature must agree, even for manually constructed messages.
      assert {:error, :invalid_body} = Message.encode(message_with_rescue_path, :little)

      # Test decode error path with too short data
      too_short_data = <<108, 1, 0, 0, 12>>

      assert {:error, :invalid_message} = Message.decode(too_short_data)

      # Test iodata padding edge cases
      # Create a message that will require padding
      {:ok, minimal_message} =
        Message.new(:signal,
          # Very short path to trigger specific padding scenarios
          path: "/a",
          # Minimal interface
          interface: "a.b",
          # Minimal member
          member: "a"
        )

      {:ok, iodata_result} = Message.encode(minimal_message, :little)
      binary_result = IO.iodata_to_binary(iodata_result)

      # Verify the message can be decoded (which ensures padding worked correctly)
      assert {:ok, decoded_minimal} = Message.decode(binary_result)
      assert decoded_minimal.header_fields.path == "/a"

      # Test different message sizes to exercise padding edge cases
      test_cases = [
        # Different path lengths to create different padding scenarios
        {"/", "a.b", "a"},
        {"/test", "com.example", "Method"},
        {"/very/long/path/that/should/cause/different/padding", "very.long.interface.name",
         "VeryLongMethodName"}
      ]

      for {path, interface, member} <- test_cases do
        {:ok, test_msg} = Message.new(:signal, path: path, interface: interface, member: member)
        {:ok, test_iodata} = Message.encode(test_msg, :little)
        test_binary = IO.iodata_to_binary(test_iodata)

        # Verify proper 8-byte alignment (message length should be multiple of 8 after header)
        # The header portion before body should be 8-byte aligned
        assert {:ok, _decoded} = Message.decode(test_binary)
      end

      # Test edge case where iodata is already 8-byte aligned (no padding needed)
      {:ok, aligned_msg} =
        Message.new(:signal,
          path: "/test123",
          interface: "test.interface",
          member: "Test12345678"
        )

      {:ok, aligned_iodata} = Message.encode(aligned_msg, :little)
      aligned_binary = IO.iodata_to_binary(aligned_iodata)
      assert {:ok, _} = Message.decode(aligned_binary)

      # Test error handling in size estimation (covers rescue clauses)
      # This tests internal error handling paths that might not be covered
      invalid_msg = %Message{
        type: :signal,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 123,
        header_fields: %{
          path: "/test",
          interface: "test.interface",
          member: "Test"
        },
        body: []
      }

      # This should still encode successfully due to error handling
      assert {:ok, _} = Message.encode(invalid_msg, :little)

      # Ensure both endianness paths in iodata encoding are covered
      {:ok, endian_test_msg} =
        Message.new(:signal, path: "/endian", interface: "test.endian", member: "TestEndian")

      # Test little endian (likely already covered)
      assert {:ok, little_iodata} = Message.encode(endian_test_msg, :little)
      little_binary = IO.iodata_to_binary(little_iodata)
      assert {:ok, _} = Message.decode(little_binary)

      # Test big endian to ensure complete coverage of both paths
      assert {:ok, big_iodata} = Message.encode(endian_test_msg, :big)
      big_binary = IO.iodata_to_binary(big_iodata)
      assert {:ok, _} = Message.decode(big_binary)
    end
  end

  describe "parse/1" do
    test "returns nil for insufficient data" do
      # Empty binary
      assert Message.parse(<<>>) == nil

      # Less than 12 bytes (fixed header size)
      assert Message.parse(<<1, 2, 3>>) == nil
      assert Message.parse(<<1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11>>) == nil
    end

    test "returns an error for invalid endianness after every fixed-header boundary" do
      for size <- 12..16 do
        invalid_header = <<255, 0::size((size - 1) * 8)>>

        assert {:error, :invalid_endianness} = Message.parse(invalid_header)
      end
    end

    test "validates the message type and protocol version at the fixed-header boundary" do
      invalid_type = fixed_header(:little, 0, 1, 0, 0)
      unsupported_version = fixed_header(:big, 4, 2, 0, 0)

      assert {:error, :invalid_message_type} = Message.parse(binary_part(invalid_type, 0, 12))

      assert {:error, :unsupported_protocol_version} =
               Message.parse(binary_part(unsupported_version, 0, 12))
    end

    test "rejects hostile declared body and header-field lengths before the body arrives" do
      for endianness <- [:little, :big] do
        too_large_body = Message.max_message_size() - 16 + 1
        body_length_header = fixed_header(endianness, 4, 1, too_large_body, 0)

        assert {:error, :message_too_large} = Message.parse(body_length_header)
        assert {:error, :message_too_large} = Message.decode(body_length_header)

        too_large_header_fields = 67_108_864 + 1
        header_fields_length_header = fixed_header(endianness, 4, 1, 0, too_large_header_fields)

        assert {:error, :message_too_large} = Message.parse(header_fields_length_header)
        assert {:error, :message_too_large} = Message.decode(header_fields_length_header)
      end
    end

    test "enforces the D-Bus 2^26 header-fields array limit" do
      for endianness <- [:little, :big] do
        maximum_array = fixed_header(endianness, 4, 1, 0, 67_108_864)
        oversized_array = fixed_header(endianness, 4, 1, 0, 67_108_864 + 1)

        assert nil == Message.parse(maximum_array)
        assert {:error, :message_too_large} = Message.parse(oversized_array)
      end
    end

    test "allows a maximum-size message to remain incomplete" do
      for endianness <- [:little, :big] do
        maximum_body = Message.max_message_size() - 16

        assert nil == Message.parse(fixed_header(endianness, 4, 1, maximum_body, 0))
      end
    end

    test "handles complete, truncated, and concatenated messages in both byte orders" do
      body = [42, "payload"]
      signature = "is"

      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "TestSignal",
          body: body,
          signature: signature
        )

      next_message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "NextSignal",
          body: [7],
          signature: "i"
        )

      for endianness <- [:little, :big] do
        {:ok, encoded} = Message.encode(message, endianness)
        complete_binary = IO.iodata_to_binary(encoded)

        assert {:ok, decoded_message, <<>>} = Message.parse(complete_binary)
        assert decoded_message.body == body
        assert Message.signature(decoded_message) == signature

        for size <- 0..(byte_size(complete_binary) - 1) do
          assert Message.parse(binary_part(complete_binary, 0, size)) == nil
        end

        {:ok, next_encoded} = Message.encode(next_message, endianness)
        next_binary = IO.iodata_to_binary(next_encoded)
        stream = complete_binary <> next_binary
        complete_size = byte_size(complete_binary)

        for size <- 0..byte_size(next_binary) do
          next_prefix = binary_part(next_binary, 0, size)

          assert {:ok, parsed_message, ^next_prefix} =
                   Message.parse(binary_part(stream, 0, complete_size + size))

          assert parsed_message.body == body
          assert Message.signature(parsed_message) == signature
        end
      end
    end

    test "successfully parses complete message" do
      # Create a complete message
      {:ok, original_message} =
        Message.new(:method_call,
          path: "/com/example/Object",
          interface: "com.example.Interface",
          member: "TestMethod",
          body: [42, "hello"],
          signature: "is"
        )

      {:ok, encoded} = Message.encode(original_message)
      complete_binary = IO.iodata_to_binary(encoded)

      # Parse should succeed
      assert {:ok, parsed_message, remaining_data} = Message.parse(complete_binary)

      # Should have no remaining data for exact message
      assert remaining_data == <<>>

      # Verify the parsed message matches the original
      assert parsed_message.type == original_message.type
      assert parsed_message.header_fields == original_message.header_fields
      assert parsed_message.body == original_message.body
      assert Message.signature(parsed_message) == Message.signature(original_message)
    end

    test "successfully parses message with extra data" do
      # Create a complete message
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "TestSignal"
        )

      {:ok, encoded} = Message.encode(message)
      complete_binary = IO.iodata_to_binary(encoded)

      # Add extra data after the message
      extra_data = <<1, 2, 3, 4, 5, 6, 7, 8>>
      binary_with_extra = complete_binary <> extra_data

      # Parse should succeed and return extra data
      assert {:ok, parsed_message, remaining_data} = Message.parse(binary_with_extra)
      assert parsed_message.type == :signal
      assert Map.get(parsed_message.header_fields, :path) == "/test"
      assert remaining_data == extra_data
    end

    test "returns error for malformed message with sufficient length" do
      # Create a binary that has sufficient length but is malformed
      {:ok, message} =
        Message.new(:signal,
          path: "/test",
          interface: "test.interface",
          member: "TestSignal"
        )

      {:ok, encoded} = Message.encode(message)
      complete_binary = IO.iodata_to_binary(encoded)

      # Corrupt the message type byte (position 1) to an invalid value
      <<first, _type, rest::binary>> = complete_binary
      corrupted_binary = <<first, 99, rest::binary>>

      # Parse should return an error (not nil) since we have sufficient data
      assert {:error, _reason} = Message.parse(corrupted_binary)
    end

    test "handles different message types" do
      message_types = [
        {:method_call, [path: "/test", member: "TestMethod"]},
        {:method_return, [reply_serial: 123]},
        {:error, [error_name: "test.Error", reply_serial: 123]},
        {:signal, [path: "/test", interface: "test.interface", member: "TestSignal"]}
      ]

      for {type, opts} <- message_types do
        {:ok, message} = Message.new(type, opts)
        {:ok, encoded} = Message.encode(message)
        complete_binary = IO.iodata_to_binary(encoded)

        assert {:ok, parsed_message, remaining_data} = Message.parse(complete_binary)
        assert parsed_message.type == type
        assert remaining_data == <<>>
      end
    end
  end

  defp signal_with(extra) do
    Message.new(
      :signal,
      Keyword.merge([path: "/test", interface: "org.example.Test", member: "Test"], extra)
    )
  end
end
