defmodule Rebus.IdentityTest do
  use ExUnit.Case, async: true

  alias Rebus.Identity.Posix

  describe "Rebus.Identity.Posix" do
    test "reads the local uid and username from id(1)" do
      assert {:ok, uid} = Posix.auth_id(5_000)
      assert String.trim(uid) =~ ~r/^\d+$/

      assert {:ok, username} = Posix.username(5_000)
      assert String.trim(username) != ""
    end

    test "reports a missing executable as :enoent" do
      assert {:error, :enoent} = Posix.auth_id(100, fn _name -> nil end)
      assert {:error, :enoent} = Posix.username(100, fn _name -> nil end)
    end

    test "contains an executable lookup that raises or throws" do
      assert {:error, :enoent} =
               Posix.auth_id(100, fn _name -> raise "executable lookup failed" end)

      assert {:error, :enoent} =
               Posix.auth_id(100, fn _name -> throw(:executable_lookup_failed) end)
    end

    test "contains a port opener that raises or throws" do
      assert {:error, :port_open_failed} =
               Posix.auth_id(100, fn _name -> "/missing/id" end, fn _spec, _opts ->
                 raise "port open failed"
               end)

      assert {:error, :port_open_failed} =
               Posix.username(100, fn _name -> "/missing/id" end, fn _spec, _opts ->
                 throw(:port_open_failed)
               end)
    end

    test "rejects output larger than the bounded response" do
      assert {:error, :output_too_large} =
               Posix.auth_id(1_000, fn _name -> "/missing/id" end, fn _spec, _opts ->
                 emitting_port(fn port ->
                   send(self(), {port, {:data, String.duplicate("9", 65)}})
                 end)
               end)
    end

    test "reports a non-zero exit status and a port exit" do
      assert {:error, :exit_status} =
               Posix.auth_id(1_000, fn _name -> "/missing/id" end, fn _spec, _opts ->
                 emitting_port(fn port -> send(self(), {port, {:exit_status, 1}}) end)
               end)

      assert {:error, :port_exit} =
               Posix.auth_id(1_000, fn _name -> "/missing/id" end, fn _spec, _opts ->
                 emitting_port(fn port -> send(self(), {:EXIT, port, :killed}) end)
               end)
    end

    test "times out when the port produces nothing" do
      assert {:error, :timeout} =
               Posix.auth_id(50, fn _name -> "/missing/id" end, fn _spec, _opts ->
                 emitting_port(fn _port -> :ok end)
               end)
    end
  end

  # A port the collector can wait on without spawning an external program: the
  # messages the real port would send are queued for the calling process before
  # collection starts.
  defp emitting_port(emit) do
    port = Port.open({:spawn, "cat"}, [:binary])
    emit.(port)
    port
  end
end
