defmodule Rebus.Connection.InboundTest do
  use ExUnit.Case, async: true

  alias Rebus.Connection.Inbound
  alias Rebus.Message

  @max_read_chunk 65_536

  describe "append/2" do
    test "keeps byte order for increasing fragment sizes" do
      fragments = for size <- 1..12, do: :binary.copy(<<size>>, size)

      assert {:ok, inbound} = append_all(fragments)
      assert buffered(inbound) == IO.iodata_to_binary(fragments)
      assert inbound.size == byte_size(IO.iodata_to_binary(fragments))
    end

    test "keeps byte order for decreasing fragment sizes" do
      fragments = for size <- 12..1//-1, do: :binary.copy(<<size>>, size)

      assert {:ok, inbound} = append_all(fragments)
      assert buffered(inbound) == IO.iodata_to_binary(fragments)
    end

    test "keeps byte order for equal fragment sizes" do
      fragments = for index <- 1..12, do: :binary.copy(<<index>>, 4)

      assert {:ok, inbound} = append_all(fragments)
      assert buffered(inbound) == IO.iodata_to_binary(fragments)
    end

    test "merges only with smaller or equal predecessors" do
      assert {:ok, inbound} = append_all([<<1, 1, 1, 1>>, <<2>>, <<3>>])

      # The two one-byte fragments merge with each other but not with the
      # larger fragment ahead of them.
      assert inbound.segments == [{2, <<2, 3>>}, {4, <<1, 1, 1, 1>>}]
      assert buffered(inbound) == <<1, 1, 1, 1, 2, 3>>
    end

    test "ignores an empty fragment" do
      assert {:ok, inbound} = Inbound.append(Inbound.new(), <<0>>)
      assert {:ok, ^inbound} = Inbound.append(inbound, <<>>)
    end

    test "rejects pathological decreasing fragment sizes at the segment cap" do
      fragments = for size <- 128..1//-1, do: :binary.copy(<<0>>, size)

      assert {:error, :message_too_large} = append_all(fragments)
    end

    test "accepts far more fragments than the cap for ordinary traffic" do
      fragments = for _index <- 1..10_000, do: <<0>>

      assert {:ok, inbound} = append_all(fragments)
      assert inbound.size == 10_000
    end
  end

  describe "next/1" do
    test "is incomplete on an empty buffer" do
      assert {:incomplete, inbound} = Inbound.next(Inbound.new())
      assert inbound.expected_size == nil
    end

    test "is incomplete below sixteen retained bytes" do
      frame = encoded_frame("Partial")

      for size <- 1..15 do
        {:ok, inbound} = Inbound.append(Inbound.new(), binary_part(frame, 0, size))

        assert {:incomplete, inbound} = Inbound.next(inbound)
        assert inbound.expected_size == nil
        assert inbound.flatten_count == 0
      end
    end

    test "rejects a bad endianness byte as soon as sixteen bytes exist" do
      assert {:error, :invalid_endianness} = next_from(fixed_header(endian: ?x))
    end

    test "rejects an unknown message type as soon as sixteen bytes exist" do
      assert {:error, :invalid_message_type} = next_from(fixed_header(type: 9))
    end

    test "rejects an unsupported protocol version as soon as sixteen bytes exist" do
      assert {:error, :unsupported_protocol_version} = next_from(fixed_header(version: 2))
    end

    test "returns a complete frame exactly at the boundary" do
      frame = encoded_frame("Boundary")
      last = byte_size(frame) - 1

      {:ok, inbound} = Inbound.append(Inbound.new(), binary_part(frame, 0, last))
      assert {:incomplete, inbound} = Inbound.next(inbound)
      assert inbound.expected_size == byte_size(frame)

      {:ok, inbound} = Inbound.append(inbound, binary_part(frame, last, 1))
      assert {:frame, ^frame, inbound} = Inbound.next(inbound)
      assert inbound.segments == []
      assert inbound.size == 0
      assert inbound.expected_size == nil
      assert inbound.flatten_count == 1
    end

    test "buffers the remainder that follows a complete frame" do
      frame = encoded_frame("Remainder")
      tail = binary_part(frame, 0, 4)

      {:ok, inbound} = Inbound.append(Inbound.new(), frame <> tail)
      assert {:frame, flat, inbound} = Inbound.next(inbound)
      assert flat == frame <> tail

      # The caller parses whole frames out of the flat bytes and hands back
      # only the trailing fragment it could not parse.
      {:ok, inbound} = Inbound.append(inbound, Inbound.retain_remainder(tail, flat))
      assert {:incomplete, inbound} = Inbound.next(inbound)
      assert buffered(inbound) == tail
      assert inbound.flatten_count == 1
    end

    test "materialises two coalesced frames once" do
      first = encoded_frame("First")
      second = encoded_frame("Second")

      {:ok, inbound} = Inbound.append(Inbound.new(), first <> second)

      assert {:frame, flat, inbound} = Inbound.next(inbound)
      assert flat == first <> second
      assert inbound.flatten_count == 1

      assert {:incomplete, inbound} = Inbound.next(inbound)
      assert inbound.flatten_count == 1
    end

    test "advances the flatten count once per materialisation" do
      frame = encoded_frame("Counted")

      inbound =
        Enum.reduce(1..3, Inbound.new(), fn _index, inbound ->
          {:ok, inbound} = Inbound.append(inbound, frame)
          assert {:frame, ^frame, inbound} = Inbound.next(inbound)
          inbound
        end)

      assert inbound.flatten_count == 3
    end

    test "rejects a frame declared larger than the D-Bus maximum before its body arrives" do
      body_length = Message.max_message_size()

      assert {:error, :message_too_large} = next_from(fixed_header(body_length: body_length))
    end
  end

  describe "retain_remainder/2" do
    test "copies a remainder well under a quarter of its source" do
      source = :binary.copy(<<0>>, 1_000)
      remainder = binary_part(source, 0, 100)

      assert Inbound.retain_remainder(remainder, source) == remainder
      assert :binary.referenced_byte_size(Inbound.retain_remainder(remainder, source)) == 100
    end

    test "keeps a remainder that is not small relative to its source" do
      source = :binary.copy(<<0>>, 1_000)
      remainder = binary_part(source, 0, 500)

      assert :binary.referenced_byte_size(Inbound.retain_remainder(remainder, source)) == 1_000
    end
  end

  describe "receive_size/2" do
    test "reads up to the fixed header while the frame length is unknown" do
      assert Inbound.receive_size(Inbound.new(), @max_read_chunk) == 16

      {:ok, inbound} = Inbound.append(Inbound.new(), <<0, 0, 0, 0, 0>>)
      assert Inbound.receive_size(inbound, @max_read_chunk) == 11
    end

    test "reads up to the frame boundary once the frame length is known" do
      frame = encoded_frame("Sized")

      {:ok, inbound} = Inbound.append(Inbound.new(), binary_part(frame, 0, 16))
      assert {:incomplete, inbound} = Inbound.next(inbound)

      assert Inbound.receive_size(inbound, @max_read_chunk) == byte_size(frame) - 16
    end

    test "never exceeds the caller's maximum chunk" do
      frame = encoded_frame("Chunked")

      assert Inbound.receive_size(Inbound.new(), 4) == 4

      {:ok, inbound} = Inbound.append(Inbound.new(), binary_part(frame, 0, 16))
      assert {:incomplete, inbound} = Inbound.next(inbound)

      assert Inbound.receive_size(inbound, 4) == 4
    end

    test "never asks for fewer than one byte" do
      {:ok, inbound} = Inbound.append(Inbound.new(), :binary.copy(<<0>>, 20))

      assert Inbound.receive_size(inbound, @max_read_chunk) == 1
      assert Inbound.receive_size(%{inbound | expected_size: 8}, @max_read_chunk) == 1
    end
  end

  describe "new/1" do
    test "seeds an empty buffer with the bytes read past the handshake" do
      assert Inbound.new(<<>>) == Inbound.new()

      frame = encoded_frame("Seeded")
      assert {:frame, ^frame, _inbound} = Inbound.next(Inbound.new(frame))
    end
  end

  defp append_all(fragments) do
    Enum.reduce_while(fragments, {:ok, Inbound.new()}, fn fragment, {:ok, inbound} ->
      case Inbound.append(inbound, fragment) do
        {:ok, inbound} -> {:cont, {:ok, inbound}}
        {:error, reason} -> {:halt, {:error, reason}}
      end
    end)
  end

  defp next_from(data) do
    {:ok, inbound} = Inbound.append(Inbound.new(), data)
    Inbound.next(inbound)
  end

  defp buffered(%Inbound{segments: segments}) do
    segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> IO.iodata_to_binary()
  end

  defp encoded_frame(member) do
    message =
      Message.new!(:method_call,
        path: "/test",
        interface: "org.example.Test",
        member: member,
        signature: "s",
        body: ["frame body"]
      )

    {:ok, encoded} = Message.encode(message)
    IO.iodata_to_binary(encoded)
  end

  # A minimal valid fixed header plus a zero-length header-fields array, so a
  # single field can be corrupted per test.
  defp fixed_header(overrides) do
    endian = Keyword.get(overrides, :endian, ?l)
    type = Keyword.get(overrides, :type, 1)
    version = Keyword.get(overrides, :version, 1)
    body_length = Keyword.get(overrides, :body_length, 0)

    <<endian, type, 0, version, body_length::little-32, 1::little-32, 0::little-32>>
  end
end
