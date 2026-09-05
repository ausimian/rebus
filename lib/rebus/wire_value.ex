defmodule Rebus.WireValue do
  @moduledoc false

  alias Rebus.Signature

  @spec valid_string?(term()) :: boolean()
  def valid_string?(value),
    do: is_binary(value) and String.valid?(value) and :binary.match(value, <<0>>) == :nomatch

  @spec valid_object_path?(term()) :: boolean()
  def valid_object_path?("/"), do: true

  def valid_object_path?(path) when is_binary(path) do
    valid_string?(path) and String.match?(path, ~r{\A/[A-Za-z0-9_/]*\z}) and
      not String.ends_with?(path, "/") and not String.contains?(path, "//")
  end

  def valid_object_path?(_), do: false

  @spec valid_signature?(term()) :: boolean()
  def valid_signature?(value),
    do: valid_string?(value) and match?({:ok, _}, Signature.parse(value))

  @spec validate!(atom(), term()) :: :ok
  def validate!(:string, value) do
    if valid_string?(value), do: :ok, else: raise(ArgumentError, "invalid D-Bus string")
  end

  def validate!(:object_path, value) do
    if valid_object_path?(value), do: :ok, else: raise(ArgumentError, "invalid D-Bus object path")
  end

  def validate!(:signature, value) do
    if valid_string?(value) do
      case Signature.parse(value) do
        {:ok, _types} -> :ok
        {:error, :resource_limit} -> raise Rebus.ResourceLimitError, limit: :nesting
        {:error, :invalid_signature} -> raise ArgumentError, "invalid D-Bus signature"
      end
    else
      raise ArgumentError, "invalid D-Bus signature"
    end
  end
end
