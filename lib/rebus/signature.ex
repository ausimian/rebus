defmodule Rebus.Signature do
  @moduledoc """
  Parses D-Bus type signatures into structured Elixir type descriptions.

  Signature grammar violations are reported as `:invalid_signature`. Container
  nesting is a local resource and safety cap; a grammatically valid signature
  that exceeds it is reported as `:resource_limit` (or raises
  `Rebus.ResourceLimitError` from `parse!/1`).
  """

  alias Rebus.ResourceLimitError

  @max_length 255
  @max_array_depth 32
  @max_struct_depth 32
  @max_total_depth 64

  @doc false
  @spec max_total_depth() :: pos_integer()
  def max_total_depth, do: @max_total_depth

  @doc false
  @spec max_array_depth() :: pos_integer()
  def max_array_depth, do: @max_array_depth

  @doc false
  @spec max_struct_depth() :: pos_integer()
  def max_struct_depth, do: @max_struct_depth

  @typedoc false
  @type nesting_state :: map()

  @doc false
  @spec new_nesting_state() :: nesting_state()
  def new_nesting_state, do: %{array_depth: 0, struct_depth: 0, total_depth: 0}

  @doc false
  @spec validate_nesting!(any(), nesting_state()) :: :ok
  def validate_nesting!(types, state) when is_list(types) do
    Enum.each(types, &validate_nesting!(&1, state))
  end

  def validate_nesting!({:array, element_type}, state) do
    state |> enter_container!(:array) |> then(&validate_nesting!(element_type, &1))
  end

  def validate_nesting!({:struct, field_types}, state) do
    validate_nesting!(field_types, enter_container!(state, :struct))
  end

  def validate_nesting!({:dict_entry, key_type, value_type}, state) do
    validate_nesting!([key_type, value_type], enter_container!(state, :dict_entry))
  end

  def validate_nesting!({:variant, _}, state) do
    _ = enter_container!(state, :variant)
    :ok
  end

  def validate_nesting!(_type, _state), do: :ok

  @doc false
  @spec enter_container!(map(), :array | :struct | :dict_entry | :variant) :: map()
  def enter_container!(%{total_depth: total_depth} = state, :variant)
      when total_depth < @max_total_depth,
      do: %{state | total_depth: total_depth + 1}

  def enter_container!(%{array_depth: array_depth, total_depth: total_depth} = state, :array)
      when array_depth < @max_array_depth and total_depth < @max_total_depth,
      do: %{state | array_depth: array_depth + 1, total_depth: total_depth + 1}

  def enter_container!(%{struct_depth: struct_depth, total_depth: total_depth} = state, kind)
      when kind in [:struct, :dict_entry] and struct_depth < @max_struct_depth and
             total_depth < @max_total_depth,
      do: %{state | struct_depth: struct_depth + 1, total_depth: total_depth + 1}

  def enter_container!(_state, _kind), do: raise(ResourceLimitError, limit: :nesting)

  @doc false
  @spec leave_container(map(), nesting_state()) :: map()
  def leave_container(state, parent_state) do
    %{
      state
      | array_depth: parent_state.array_depth,
        struct_depth: parent_state.struct_depth,
        total_depth: parent_state.total_depth
    }
  end

  @typedoc """
  A parsed D-Bus type.

  Basic types are `{name, nil}`. Containers retain their element or field
  types: `{:array, type}`, `{:struct, fields}`, and
  `{:dict_entry, key_type, value_type}`. A variant is `{:variant, nil}` because
  its contained signature is carried in each encoded value.
  """
  @type ast ::
          {:byte
           | :boolean
           | :int16
           | :uint16
           | :int32
           | :uint32
           | :int64
           | :uint64
           | :double
           | :string
           | :object_path
           | :signature
           | :variant
           | :unix_fd, nil}
          | {:array, ast()}
          | {:struct, [ast()]}
          | {:dict_entry, ast(), ast()}

  @doc """
  Parses a D-Bus signature into one AST node per top-level type.

  Returns `{:error, :invalid_signature}` for malformed or over-long input and
  `{:error, :resource_limit}` when container nesting exceeds Rebus's local
  limit.

  ## Examples

      iex> Rebus.Signature.parse("sa{sv}")
      {:ok, [
        {:string, nil},
        {:array, {:dict_entry, {:string, nil}, {:variant, nil}}}
      ]}

      iex> Rebus.Signature.parse("(")
      {:error, :invalid_signature}
  """
  @spec parse(binary()) :: {:ok, [ast()]} | {:error, :invalid_signature | :resource_limit}
  def parse(signature) when is_binary(signature) and byte_size(signature) <= @max_length do
    case parse_types(:binary.bin_to_list(signature), 0, 0, 0, []) do
      {:ok, [], types} ->
        types |> Enum.reverse() |> check_nesting()

      _ ->
        {:error, :invalid_signature}
    end
  end

  def parse(_signature), do: {:error, :invalid_signature}

  @doc """
  Parses a D-Bus signature and raises when it is invalid.

  Raises `ArgumentError` for invalid input and `Rebus.ResourceLimitError` when
  the nesting limit is exceeded.

  ## Example

      iex> Rebus.Signature.parse!("a{si}")
      [{:array, {:dict_entry, {:string, nil}, {:int32, nil}}}]
  """
  @spec parse!(binary()) :: [ast()]
  def parse!(signature) do
    case parse(signature) do
      {:ok, types} -> types
      {:error, :invalid_signature} -> raise ArgumentError, "invalid D-Bus signature"
      {:error, :resource_limit} -> raise ResourceLimitError, limit: :nesting
    end
  end

  # Returns the D-Bus wire alignment, in bytes, of a parsed type.
  @doc false
  @spec alignment(ast()) :: 1 | 2 | 4 | 8
  def alignment({:byte, _}), do: 1
  def alignment({:boolean, _}), do: 4
  def alignment({:int16, _}), do: 2
  def alignment({:uint16, _}), do: 2
  def alignment({:int32, _}), do: 4
  def alignment({:uint32, _}), do: 4
  def alignment({:int64, _}), do: 8
  def alignment({:uint64, _}), do: 8
  def alignment({:double, _}), do: 8
  def alignment({:string, _}), do: 4
  def alignment({:object_path, _}), do: 4
  def alignment({:signature, _}), do: 1
  def alignment({:variant, _}), do: 1
  def alignment({:unix_fd, _}), do: 4
  def alignment({:array, _}), do: 4
  def alignment({:struct, _}), do: 8
  def alignment({:dict_entry, _, _}), do: 8

  # Returns the fixed wire width, in bytes, of a parsed type, or nil when it varies.
  @doc false
  @spec fixed_width(ast()) :: 1 | 2 | 4 | 8 | nil
  def fixed_width({:byte, _}), do: 1
  def fixed_width({:boolean, _}), do: 4
  def fixed_width({type, _}) when type in [:int16, :uint16], do: 2
  def fixed_width({type, _}) when type in [:int32, :uint32, :unix_fd], do: 4
  def fixed_width({type, _}) when type in [:int64, :uint64, :double], do: 8
  def fixed_width(_type), do: nil

  defp check_nesting(types) do
    :ok = validate_nesting!(types, new_nesting_state())
    {:ok, types}
  rescue
    ResourceLimitError -> {:error, :resource_limit}
  end

  defp parse_types([], _arrays, _structs, _total, acc), do: {:ok, [], acc}

  defp parse_types(types, arrays, structs, total, acc) do
    with {:ok, type, remaining} <- parse_type(types, arrays, structs, total, false) do
      parse_types(remaining, arrays, structs, total, [type | acc])
    end
  end

  defp parse_type([code | rest], _arrays, _structs, _total, _allow_dict)
       when code in ~c"ybnqiuxtdsogh" do
    {:ok, basic_type(code), rest}
  end

  defp parse_type([?v | rest], _arrays, _structs, _total, _allow_dict) do
    {:ok, {:variant, nil}, rest}
  end

  defp parse_type([?a | rest], arrays, structs, total, _allow_dict) do
    case rest do
      [?{ | dict_rest] ->
        with {:ok, dict, remaining} <- parse_dict(dict_rest, arrays + 1, structs, total + 1) do
          {:ok, {:array, dict}, remaining}
        end

      _ ->
        with {:ok, type, remaining} <- parse_type(rest, arrays + 1, structs, total + 1, true) do
          {:ok, {:array, type}, remaining}
        end
    end
  end

  defp parse_type([?( | rest], arrays, structs, total, _allow_dict) do
    parse_struct(rest, arrays, structs + 1, total + 1, [])
  end

  defp parse_type([?{ | _rest], _arrays, _structs, _total, false),
    do: {:error, :invalid_signature}

  defp parse_type(_types, _arrays, _structs, _total, _allow_dict),
    do: {:error, :invalid_signature}

  defp parse_dict([key | rest], arrays, structs, total) when key in ~c"ybnqiuxtdsogh" do
    case parse_type(rest, arrays, structs + 1, total + 1, true) do
      {:ok, value, [?} | remaining]} -> {:ok, {:dict_entry, basic_type(key), value}, remaining}
      _ -> {:error, :invalid_signature}
    end
  end

  defp parse_dict(_rest, _arrays, _structs, _total), do: {:error, :invalid_signature}

  defp parse_struct([?) | _rest], _arrays, _structs, _total, []), do: {:error, :invalid_signature}

  defp parse_struct([?) | rest], _arrays, _structs, _total, acc),
    do: {:ok, {:struct, Enum.reverse(acc)}, rest}

  defp parse_struct(types, arrays, structs, total, acc) do
    with {:ok, type, remaining} <- parse_type(types, arrays, structs, total, false) do
      parse_struct(remaining, arrays, structs, total, [type | acc])
    end
  end

  defp basic_type(?y), do: {:byte, nil}
  defp basic_type(?b), do: {:boolean, nil}
  defp basic_type(?n), do: {:int16, nil}
  defp basic_type(?q), do: {:uint16, nil}
  defp basic_type(?i), do: {:int32, nil}
  defp basic_type(?u), do: {:uint32, nil}
  defp basic_type(?x), do: {:int64, nil}
  defp basic_type(?t), do: {:uint64, nil}
  defp basic_type(?d), do: {:double, nil}
  defp basic_type(?s), do: {:string, nil}
  defp basic_type(?o), do: {:object_path, nil}
  defp basic_type(?g), do: {:signature, nil}
  defp basic_type(?h), do: {:unix_fd, nil}
end
