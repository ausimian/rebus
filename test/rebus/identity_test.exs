defmodule Rebus.IdentityTest do
  use ExUnit.Case, async: true

  alias Rebus.Identity.Posix

  # The first lines of a real `/proc/self/status`, up to the `Uid:` line.
  @status_head """
  Name:\tbeam.smp
  Umask:\t0022
  State:\tS (sleeping)
  Tgid:\t4242
  Ngid:\t0
  Pid:\t4242
  PPid:\t1
  TracerPid:\t0
  """

  describe "Rebus.Identity.Posix" do
    test "reads the local uid and username" do
      assert {:ok, uid} = Posix.auth_id(5_000)
      assert String.trim(uid) =~ ~r/^\d+$/

      assert {:ok, username} = Posix.username(5_000)
      assert String.trim(username) != ""
    end

    test "reads the same uid with or without /proc/self/status" do
      assert {:ok, uid} = Posix.auth_id(5_000)
      assert {:ok, spawned} = Posix.auth_id(5_000, read_status: fn -> {:error, :enoent} end)
      assert String.trim(uid) == String.trim(spawned)
    end

    test "reads the uid from /proc/self/status only on Linux" do
      no_spawn = [open_port: fn _spec, _opts -> raise "spawned id" end]

      case :os.type() do
        {:unix, :linux} ->
          assert {:ok, uid} = Posix.auth_id(5_000, no_spawn)
          assert uid =~ ~r/^\d+$/

        _other ->
          assert {:error, :port_open_failed} = Posix.auth_id(5_000, no_spawn)
      end
    end

    test "runs id from /usr/bin when it is there" do
      if File.regular?("/usr/bin/id") do
        default_probe = Keyword.delete(recording(), :executable?)

        assert {:ok, "4242\n"} = Posix.username(1_000, default_probe)
        assert_received {:opened, "/usr/bin/id", ["-un"]}
        refute_received {:found, _name}
      end
    end
  end

  describe "the /proc/self/status uid" do
    test "is the effective uid, read without running id" do
      options = recording(read_status: status_reader("Uid:\t1000\t1001\t1002\t1003\n"))

      assert {:ok, "1001"} = Posix.auth_id(1_000, options)
      refute_received {:probed, _path}
      refute_received {:opened, _executable, _args}

      options = recording(read_status: status_reader("Uid: 0 4294967295 0 0\n"))
      assert {:ok, "4294967295"} = Posix.auth_id(1_000, options)
    end

    test "falls back to id -u when the Uid line is malformed" do
      for uid_line <- [
            "",
            "Uid:\t1\t2\t3\n",
            "Uid:\t1\t2\t3\t4\t5\n",
            "Uid:\t1\tx\t3\t4\n",
            "Uid:\t1\t+2\t3\t4\n",
            "Uid:\t1\t-2\t3\t4\n",
            "Uid:\t1\t4294967296\t3\t4\n",
            "Uid:\t1\t00000000002\t3\t4\n",
            "Uid:\t1\t2\t3\t4\nUid:\t1\t2\t3\t4\n",
            "uid:\t1\t2\t3\t4\n"
          ] do
        options = trusted_id(read_status: status_reader(uid_line))

        assert {:ok, "4242\n"} = Posix.auth_id(1_000, options), inspect(uid_line)
        assert_received {:opened, "/usr/bin/id", ["-u"]}
      end
    end

    test "falls back to id -u when the prefix ends inside the Uid line" do
      options = trusted_id(read_status: fn -> {:ok, @status_head <> "Uid:\t1\t2\t3\t4"} end)

      assert {:ok, "4242\n"} = Posix.auth_id(1_000, options)
      assert_received {:opened, "/usr/bin/id", ["-u"]}
    end

    test "falls back to id -u when the status is larger than its bounded prefix" do
      status = @status_head <> "Uid:\t1\t2\t3\t4\n" <> String.duplicate("x", 4096) <> "\n"
      options = trusted_id(read_status: fn -> {:ok, status} end)

      assert {:ok, "4242\n"} = Posix.auth_id(1_000, options)
      assert_received {:opened, "/usr/bin/id", ["-u"]}
    end

    test "falls back to id -u when the status cannot be read" do
      for read_status <- [
            fn -> {:error, :eacces} end,
            fn -> raise "status read failed" end,
            fn -> throw(:status_read_failed) end,
            fn -> {:ok, :not_a_binary} end,
            fn -> :unexpected end
          ] do
        assert {:ok, "4242\n"} = Posix.auth_id(1_000, trusted_id(read_status: read_status))
        assert_received {:opened, "/usr/bin/id", ["-u"]}
      end
    end

    test "is never consulted for the username" do
      assert {:ok, "4242\n"} = Posix.username(1_000, trusted_id())
      refute_received :read_status
      assert_received {:opened, "/usr/bin/id", ["-un"]}
    end

    test "counts a slow read against the deadline" do
      for result <- [{:error, :enoent}, {:ok, @status_head <> "Uid:\t1\t2\t3\t4\n"}] do
        read_status = fn ->
          Process.sleep(80)
          result
        end

        assert {:error, :timeout} = Posix.auth_id(50, trusted_id(read_status: read_status))
        refute_received {:opened, _executable, _args}
      end
    end
  end

  describe "the id executable" do
    test "is taken from /usr/bin, then /bin, before PATH" do
      both = probe(fn _path -> true end)
      assert {:ok, "4242\n"} = Posix.username(1_000, recording(executable?: both))
      assert_received {:opened, "/usr/bin/id", ["-un"]}
      refute_received {:found, _name}

      bin_only = probe(&(&1 == "/bin/id"))
      assert {:ok, "4242\n"} = Posix.username(1_000, recording(executable?: bin_only))
      assert_received {:probed, "/usr/bin/id"}
      assert_received {:probed, "/bin/id"}
      assert_received {:opened, "/bin/id", ["-un"]}
      refute_received {:found, _name}
    end

    test "is looked up through PATH only when neither trusted path holds it" do
      options = recording(find_executable: finder("/run/current-system/sw/bin/id"))

      assert {:ok, "4242\n"} = Posix.username(1_000, options)
      assert_received {:probed, "/usr/bin/id"}
      assert_received {:probed, "/bin/id"}
      assert_received {:found, "id"}
      assert_received {:opened, "/run/current-system/sw/bin/id", ["-un"]}
    end

    test "is refused when PATH resolves it to a relative path" do
      for relative <- ["./id", "id", "bin/id"] do
        options = recording(find_executable: finder(relative))

        assert {:error, :enoent} = Posix.username(1_000, options)
        refute_received {:opened, _executable, _args}
      end
    end

    test "is reported missing as :enoent" do
      assert {:error, :enoent} = Posix.auth_id(100, recording())
      assert {:error, :enoent} = Posix.username(100, recording())
      refute_received {:opened, _executable, _args}
    end

    test "treats a trusted-path probe that raises or throws as absent" do
      raising = fn _path -> raise "probe failed" end
      throwing = fn _path -> throw(:probe_failed) end

      for executable? <- [raising, throwing] do
        options = recording(executable?: executable?, find_executable: finder("/opt/bin/id"))

        assert {:ok, "4242\n"} = Posix.username(1_000, options)
        assert_received {:opened, "/opt/bin/id", ["-un"]}
      end
    end

    test "contains a PATH lookup that raises or throws" do
      raising = fn _name -> raise "executable lookup failed" end
      throwing = fn _name -> throw(:executable_lookup_failed) end

      assert {:error, :enoent} = Posix.auth_id(100, recording(find_executable: raising))
      assert {:error, :enoent} = Posix.auth_id(100, recording(find_executable: throwing))
    end
  end

  describe "the id port" do
    test "contains a port opener that raises or throws" do
      assert {:error, :port_open_failed} =
               Posix.auth_id(
                 100,
                 trusted_id(open_port: fn _spec, _opts -> raise "port open failed" end)
               )

      assert {:error, :port_open_failed} =
               Posix.username(
                 100,
                 trusted_id(open_port: fn _spec, _opts -> throw(:port_open_failed) end)
               )
    end

    test "rejects output larger than the bounded response" do
      assert {:error, :output_too_large} =
               Posix.auth_id(
                 1_000,
                 trusted_id(
                   open_port: fn _spec, _opts ->
                     emitting_port(fn port ->
                       send(self(), {port, {:data, String.duplicate("9", 65)}})
                     end)
                   end
                 )
               )
    end

    test "reports a non-zero exit status and a port exit" do
      assert {:error, :exit_status} =
               Posix.auth_id(
                 1_000,
                 trusted_id(
                   open_port: fn _spec, _opts ->
                     emitting_port(fn port -> send(self(), {port, {:exit_status, 1}}) end)
                   end
                 )
               )

      assert {:error, :port_exit} =
               Posix.auth_id(
                 1_000,
                 trusted_id(
                   open_port: fn _spec, _opts ->
                     emitting_port(fn port -> send(self(), {:EXIT, port, :killed}) end)
                   end
                 )
               )
    end

    test "times out when the port produces nothing" do
      assert {:error, :timeout} =
               Posix.auth_id(
                 50,
                 trusted_id(open_port: fn _spec, _opts -> emitting_port(fn _port -> :ok end) end)
               )
    end
  end

  # Options under which every source reports to the test process. By default
  # nothing is found: the status is unreadable, no trusted path holds `id`
  # and PATH has none. An opened port answers `4242`.
  defp recording(overrides \\ []) do
    test = self()

    Keyword.merge(
      [
        read_status: fn ->
          send(test, :read_status)
          {:error, :enoent}
        end,
        executable?: probe(fn _path -> false end),
        find_executable: finder(nil),
        open_port: fn {:spawn_executable, executable}, opts ->
          send(test, {:opened, List.to_string(executable), Keyword.fetch!(opts, :args)})

          emitting_port(fn port ->
            send(test, {port, {:data, "4242\n"}})
            send(test, {port, {:exit_status, 0}})
          end)
        end
      ],
      overrides
    )
  end

  # As `recording/1`, with `id` installed at `/usr/bin/id`.
  defp trusted_id(overrides \\ []) do
    recording(Keyword.merge([executable?: probe(&(&1 == "/usr/bin/id"))], overrides))
  end

  defp probe(executable?) do
    test = self()

    fn path ->
      send(test, {:probed, path})
      executable?.(path)
    end
  end

  defp finder(result) do
    test = self()

    fn name ->
      send(test, {:found, name})
      result
    end
  end

  defp status_reader(uid_line) do
    status = @status_head <> uid_line <> "Gid:\t100\t100\t100\t100\n"
    fn -> {:ok, status} end
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
