defmodule Rebus.TestTmp do
  @moduledoc false

  # Scratch paths for the suite.
  #
  # `test/test_helper.exs` creates one directory per run and removes it at VM
  # exit, so a killed run leaves a single directory rather than a file per
  # fixture. Everything the suite writes to the filesystem - Unix socket paths
  # above all - belongs under it.

  @key :test_tmp_dir

  @doc """
  Creates the per-run directory and arranges its removal at VM exit.

  Called once from `test/test_helper.exs`.
  """
  @spec setup!() :: String.t()
  def setup! do
    # Short names: macOS caps a `sun_path` at 104 bytes and `System.tmp_dir!/0`
    # there is already about half of that.
    dir = Path.join(System.tmp_dir!(), "rebus-#{System.pid()}")
    File.mkdir_p!(dir)
    Application.put_env(:rebus, @key, dir)
    System.at_exit(fn _status -> File.rm_rf(dir) end)
    dir
  end

  @doc """
  The per-run directory.
  """
  @spec dir() :: String.t()
  def dir, do: Application.fetch_env!(:rebus, @key)

  @doc """
  A unique path inside the per-run directory, prefixed with `prefix`.

  The path is not created; the caller decides what to put there.
  """
  @spec path(String.t()) :: String.t()
  def path(prefix), do: Path.join(dir(), "#{prefix}-#{System.unique_integer([:positive])}")
end
