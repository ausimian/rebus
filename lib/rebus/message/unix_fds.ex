defmodule Rebus.Message.UnixFDs do
  @moduledoc false

  # Unix file descriptor index validation for D-Bus messages. The `h` values in
  # a body are indexes into the descriptor list, so they must agree with the
  # `:unix_fds` header count and the local descriptor bound before any
  # descriptor is exposed.

  alias Rebus.Message
  alias Rebus.Signature

  @spec extract_unix_fds(keyword()) ::
          {:ok, [Rebus.UnixFD.t()]} | {:error, :invalid_unix_fds | :unix_fd_limit}
  def extract_unix_fds(opts) do
    case Keyword.get(opts, :fds, []) do
      fds when is_list(fds) -> validate_unix_fd_list(fds)
      _fds -> {:error, :invalid_unix_fds}
    end
  end

  @spec put_unix_fd_count(map(), [Rebus.UnixFD.t()]) :: {:ok, map()} | {:error, :invalid_unix_fds}
  def put_unix_fd_count(header_fields, fds) do
    count = length(fds)

    case Map.fetch(header_fields, :unix_fds) do
      :error when count == 0 -> {:ok, header_fields}
      :error -> {:ok, Map.put(header_fields, :unix_fds, count)}
      {:ok, ^count} -> {:ok, header_fields}
      {:ok, _count} -> {:error, :invalid_unix_fds}
    end
  end

  @spec validate_unix_fds(Message.t()) :: :ok | {:error, :invalid_unix_fds | :unix_fd_limit}
  def validate_unix_fds(%Message{header_fields: header_fields, unix_fds: fds} = message)
      when is_map(header_fields) and is_list(fds) do
    with {:ok, fds} <- validate_unix_fd_list(fds),
         count <- length(fds),
         ^count <- Map.get(header_fields, :unix_fds, 0),
         :ok <- validate_unix_fd_indices(Message.signature(message), message.body, fds) do
      :ok
    else
      _ -> {:error, :invalid_unix_fds}
    end
  end

  def validate_unix_fds(_message), do: {:error, :invalid_unix_fds}

  @spec validate_unix_fd_indices(term(), term(), [Rebus.UnixFD.t()]) ::
          :ok | {:error, :invalid_unix_fds}
  def validate_unix_fd_indices(signature, body, fds)
      when is_binary(signature) and is_list(body) do
    with {:ok, types} <- Signature.parse(signature),
         :ok <- validate_unix_fd_values(types, body, length(fds)) do
      :ok
    else
      _ -> {:error, :invalid_unix_fds}
    end
  end

  def validate_unix_fd_indices(_signature, _body, _fds), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_list(fds) when is_list(fds) do
    cond do
      length(fds) > Message.max_unix_fds() -> {:error, :unix_fd_limit}
      Enum.all?(fds, &(is_integer(&1) and &1 >= 0)) -> {:ok, fds}
      true -> {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fd_values([], [], _fd_count), do: :ok

  defp validate_unix_fd_values([type | types], [value | values], fd_count) do
    with :ok <- validate_unix_fd_value(type, value, fd_count) do
      validate_unix_fd_values(types, values, fd_count)
    end
  end

  defp validate_unix_fd_values(_types, _values, _fd_count), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_value({:unix_fd, _}, index, fd_count)
       when is_integer(index) and index >= 0 and index < fd_count,
       do: :ok

  defp validate_unix_fd_value({:unix_fd, _}, _index, _fd_count), do: {:error, :invalid_unix_fds}

  defp validate_unix_fd_value({:array, type}, values, fd_count) when is_list(values) do
    Enum.reduce_while(values, :ok, fn value, :ok ->
      case validate_unix_fd_value(type, value, fd_count) do
        :ok -> {:cont, :ok}
        error -> {:halt, error}
      end
    end)
  end

  defp validate_unix_fd_value({:struct, types}, values, fd_count) when is_list(values),
    do: validate_unix_fd_values(types, values, fd_count)

  defp validate_unix_fd_value({:dict_entry, key_type, value_type}, {key, value}, fd_count) do
    with :ok <- validate_unix_fd_value(key_type, key, fd_count) do
      validate_unix_fd_value(value_type, value, fd_count)
    end
  end

  defp validate_unix_fd_value({:variant, _}, {nested_signature, value}, fd_count)
       when is_binary(nested_signature) do
    case Signature.parse(nested_signature) do
      {:ok, [type]} -> validate_unix_fd_value(type, value, fd_count)
      _ -> {:error, :invalid_unix_fds}
    end
  end

  defp validate_unix_fd_value({kind, _}, _value, _fd_count)
       when kind in [:string, :object_path, :signature],
       do: :ok

  # Every other fixed-width basic type is fd-free; :unix_fd is matched above.
  defp validate_unix_fd_value({_kind, _} = type, _value, _fd_count) do
    if Signature.fixed_width(type), do: :ok, else: {:error, :invalid_unix_fds}
  end

  defp validate_unix_fd_value(_type, _value, _fd_count), do: {:error, :invalid_unix_fds}
end
