defmodule Rebus.MachineIdTest do
  use ExUnit.Case, async: true

  alias Rebus.MachineId

  @tag :tmp_dir
  test "reads a 32-character hexadecimal machine id", %{tmp_dir: tmp_dir} do
    path = write!(tmp_dir, "machine-id", "0123456789abcdef0123456789abcdef")

    assert {:ok, "0123456789abcdef0123456789abcdef"} = MachineId.read([path])
  end

  @tag :tmp_dir
  test "accepts a trailing newline", %{tmp_dir: tmp_dir} do
    path = write!(tmp_dir, "machine-id", "0123456789abcdef0123456789abcdef\n")

    assert {:ok, "0123456789abcdef0123456789abcdef"} = MachineId.read([path])
  end

  @tag :tmp_dir
  test "lower-cases an upper-case machine id", %{tmp_dir: tmp_dir} do
    path = write!(tmp_dir, "machine-id", "0123456789ABCDEF0123456789ABCDEF\n")

    assert {:ok, "0123456789abcdef0123456789abcdef"} = MachineId.read([path])
  end

  @tag :tmp_dir
  test "rejects an id of the wrong length", %{tmp_dir: tmp_dir} do
    short = write!(tmp_dir, "short", "0123456789abcdef0123456789abcde\n")
    long = write!(tmp_dir, "long", "0123456789abcdef0123456789abcdef0\n")
    empty = write!(tmp_dir, "empty", "")

    assert {:error, :unavailable} = MachineId.read([short])
    assert {:error, :unavailable} = MachineId.read([long])
    assert {:error, :unavailable} = MachineId.read([empty])
  end

  @tag :tmp_dir
  test "rejects a non-hexadecimal id", %{tmp_dir: tmp_dir} do
    path = write!(tmp_dir, "machine-id", "0123456789abcdef0123456789abcdeg\n")

    assert {:error, :unavailable} = MachineId.read([path])
  end

  @tag :tmp_dir
  test "reports an unreadable path as unavailable", %{tmp_dir: tmp_dir} do
    assert {:error, :unavailable} = MachineId.read([Path.join(tmp_dir, "missing")])
    assert {:error, :unavailable} = MachineId.read([tmp_dir])
    assert {:error, :unavailable} = MachineId.read([])
  end

  @tag :tmp_dir
  test "falls back to the next path", %{tmp_dir: tmp_dir} do
    missing = Path.join(tmp_dir, "missing")
    path = write!(tmp_dir, "machine-id", "abcdef0123456789abcdef0123456789\n")

    assert {:ok, "abcdef0123456789abcdef0123456789"} = MachineId.read([missing, path])
  end

  test "reads the host machine id from the documented paths" do
    assert MachineId.default_paths() == ["/etc/machine-id", "/var/lib/dbus/machine-id"]

    case MachineId.read() do
      {:ok, id} -> assert String.match?(id, ~r/\A[0-9a-f]{32}\z/)
      {:error, :unavailable} -> assert Enum.all?(MachineId.default_paths(), &(not readable?(&1)))
    end
  end

  defp write!(dir, name, contents) do
    path = Path.join(dir, name)
    File.write!(path, contents)
    path
  end

  defp readable?(path) do
    match?({:ok, _id}, Rebus.MachineId.read([path]))
  end
end
