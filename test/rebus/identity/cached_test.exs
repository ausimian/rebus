defmodule Rebus.Identity.CachedTest do
  # `:persistent_term` is VM-global, so this module owns the cache for the
  # duration of each test and clears it afterwards, leaving the real lookup to
  # be recomputed by whatever connects next.
  use ExUnit.Case, async: false

  alias Rebus.Identity.Cached
  alias Rebus.Identity.Posix

  setup do
    Cached.reset()
    on_exit(&Cached.reset/0)
  end

  describe "Rebus.Identity.Cached" do
    test "looks a successful result up once and replays it" do
      lookup = recording_lookup({:ok, "1000\n"})

      assert {:ok, "1000\n"} = Cached.auth_id(100, lookup)
      assert {:ok, "1000\n"} = Cached.auth_id(100, lookup)

      assert calls() == 1
    end

    test "does not cache an error" do
      failing = recording_lookup({:error, :enoent})

      assert {:error, :enoent} = Cached.auth_id(100, failing)
      assert {:error, :enoent} = Cached.auth_id(100, failing)

      assert calls() == 2

      succeeding = recording_lookup({:ok, "1000\n"})

      assert {:ok, "1000\n"} = Cached.auth_id(100, succeeding)
      assert {:ok, "1000\n"} = Cached.auth_id(100, succeeding)

      assert calls() == 1
    end

    test "caches the uid and the username independently" do
      uid = recording_lookup({:ok, "1000\n"})
      username = recording_lookup({:ok, "someone\n"})

      assert {:ok, "1000\n"} = Cached.auth_id(100, uid)
      assert {:ok, "someone\n"} = Cached.username(100, username)
      assert {:ok, "1000\n"} = Cached.auth_id(100, uid)
      assert {:ok, "someone\n"} = Cached.username(100, username)

      assert calls() == 2
    end

    test "reset/0 clears both entries" do
      lookup = recording_lookup({:ok, "1000\n"})

      assert {:ok, "1000\n"} = Cached.auth_id(100, lookup)
      assert {:ok, "1000\n"} = Cached.username(100, lookup)
      assert calls() == 2

      assert :ok = Cached.reset()

      assert {:ok, "1000\n"} = Cached.auth_id(100, lookup)
      assert {:ok, "1000\n"} = Cached.username(100, lookup)
      assert calls() == 2
    end

    test "defaults to the posix lookup" do
      assert {:ok, uid} = Posix.auth_id(5_000)
      assert {:ok, username} = Posix.username(5_000)

      assert {:ok, ^uid} = Cached.auth_id(5_000)
      assert {:ok, ^username} = Cached.username(5_000)

      assert {:ok, ^uid} = Cached.auth_id(5_000)
      assert {:ok, ^username} = Cached.username(5_000)
    end
  end

  defp recording_lookup(result) do
    owner = self()

    fn timeout when is_integer(timeout) and timeout > 0 ->
      send(owner, :lookup_called)
      result
    end
  end

  defp calls, do: calls(0)

  defp calls(count) do
    receive do
      :lookup_called -> calls(count + 1)
    after
      0 -> count
    end
  end
end
