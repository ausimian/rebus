defmodule Rebus.SignatureTest do
  use ExUnit.Case, async: true

  alias Rebus.{ResourceLimitError, Signature}

  defp arrays(depth), do: String.duplicate("a", depth) <> "i"
  defp structs(0), do: "i"
  defp structs(depth), do: "(" <> structs(depth - 1) <> ")"
  defp interleaved(0), do: "i"
  defp interleaved(depth), do: "a(" <> interleaved(depth - 1) <> ")"

  test "parses complete shared ASTs" do
    assert {:ok,
            [
              {:array,
               {:dict_entry, {:string, nil}, {:array, {:struct, [{:int32, nil}, {:byte, nil}]}}}}
            ]} =
             Signature.parse("a{sa(iy)}")
  end

  test "rejects malformed grammar without raising" do
    for signature <- [
          "is)",
          "}",
          "[",
          "()",
          "a()",
          "{si}",
          "a{vi}",
          "a{si",
          "a{si}}",
          "({si})",
          "a{s}",
          "a"
        ] do
      assert {:error, :invalid_signature} = Signature.parse(signature)
    end

    assert {:error, :invalid_signature} = Signature.parse(String.duplicate("a", 33) <> "()")
  end

  test "enforces length and container depth limits" do
    assert {:ok, _} = Signature.parse(arrays(32))
    assert {:error, :resource_limit} = Signature.parse(arrays(33))
    assert {:ok, _} = Signature.parse(structs(32))
    assert {:error, :resource_limit} = Signature.parse(structs(33))
    assert {:ok, _} = Signature.parse(interleaved(32))
    assert {:error, :resource_limit} = Signature.parse(interleaved(33))
    assert {:error, :invalid_signature} = Signature.parse(String.duplicate("i", 256))
  end

  test "counts variants as total containers" do
    assert {:ok, _} =
             Signature.parse(String.duplicate("a(", 32) <> "i" <> String.duplicate(")", 32))

    assert {:error, :resource_limit} =
             Signature.parse(String.duplicate("a(", 32) <> "v" <> String.duplicate(")", 32))
  end

  test "raising parser uses a bounded error" do
    assert_raise ArgumentError, "invalid D-Bus signature", fn -> Signature.parse!("a()") end

    assert_raise ResourceLimitError, fn -> Signature.parse!(arrays(33)) end
  end

  test "shares bounded nesting state across marshaling implementations" do
    state = Signature.new_nesting_state()
    assert Signature.max_array_depth() == 32
    assert Signature.max_struct_depth() == 32
    assert Signature.max_total_depth() == 64

    nested = state |> Signature.enter_container!(:array) |> Signature.enter_container!(:struct)
    assert nested == %{array_depth: 1, struct_depth: 1, total_depth: 2}
    assert Signature.leave_container(nested, state) == state
    assert :ok = Signature.validate_nesting!([{:array, {:int32, nil}}], state)
  end
end
