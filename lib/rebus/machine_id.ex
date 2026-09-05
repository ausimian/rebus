defmodule Rebus.MachineId do
  @moduledoc false

  # The D-Bus machine id answered by `org.freedesktop.DBus.Peer.GetMachineId`.
  # It is a host-local identifier written by systemd or dbus at install time:
  # exactly 32 hexadecimal characters, conventionally with a trailing newline.
  # Anything else is treated as no id at all rather than being repaired.

  @default_paths ["/etc/machine-id", "/var/lib/dbus/machine-id"]

  # A machine id file is 33 bytes. Reading a bounded prefix keeps a wrong file
  # (a device, a large text file) from being materialized in the connection.
  @max_read 256

  @spec default_paths() :: [Path.t()]
  def default_paths, do: @default_paths

  @spec read() :: {:ok, binary()} | {:error, :unavailable}
  def read, do: read(@default_paths)

  @spec read([Path.t()]) :: {:ok, binary()} | {:error, :unavailable}
  def read(paths) when is_list(paths) do
    Enum.reduce_while(paths, {:error, :unavailable}, fn path, unavailable ->
      case read_path(path) do
        {:ok, _id} = ok -> {:halt, ok}
        {:error, :unavailable} -> {:cont, unavailable}
      end
    end)
  end

  defp read_path(path) do
    case File.open(path, [:read, :binary], &IO.binread(&1, @max_read)) do
      {:ok, data} when is_binary(data) -> machine_id(data)
      _other -> {:error, :unavailable}
    end
  end

  defp machine_id(<<id::binary-size(32)>>), do: normalize(id, <<>>)
  defp machine_id(<<id::binary-size(32), "\n">>), do: normalize(id, <<>>)
  defp machine_id(<<id::binary-size(32), "\r\n">>), do: normalize(id, <<>>)
  defp machine_id(_data), do: {:error, :unavailable}

  defp normalize(<<>>, acc), do: {:ok, acc}

  defp normalize(<<char, rest::binary>>, acc) when char in ?0..?9 or char in ?a..?f,
    do: normalize(rest, <<acc::binary, char>>)

  defp normalize(<<char, rest::binary>>, acc) when char in ?A..?F,
    do: normalize(rest, <<acc::binary, char + 32>>)

  defp normalize(_data, _acc), do: {:error, :unavailable}
end
