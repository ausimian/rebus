defmodule Rebus.Integration.SessionBusTest do
  # Exercises Rebus against a real `dbus-daemon` rather than the in-process
  # `Rebus.TestServer`. Every test here crosses a daemon socket, so each wait
  # uses an explicit window of at least a second: a real bus is markedly slower
  # than the fake one, and the default `assert_receive` budget is not enough on
  # a loaded runner.
  #
  # Run with `dbus-run-session -- mix test --only integration` on a host that
  # has D-Bus, or `mix test.integration` to do that inside Docker.
  use ExUnit.Case, async: false

  alias Rebus.MatchRule
  alias Rebus.Message

  @moduletag :integration

  @bus_env "DBUS_SESSION_BUS_ADDRESS"

  # `@moduletag :skip` cannot be made conditional per test, so the decision is
  # taken once when the file is loaded. Without a session bus the whole module
  # is skipped with a message that names the command to use, instead of failing
  # with a connection error or silently reporting success.
  if System.get_env(@bus_env) in [nil, ""] do
    @moduletag skip:
                 "#{@bus_env} is not set: run `dbus-run-session -- mix test --only integration` " <>
                   "on a host with dbus, or `mix test.integration` to run it under Docker"
  end

  @driver "org.freedesktop.DBus"
  @driver_path "/org/freedesktop/DBus"
  @well_known_name "org.example.RebusIntegration"
  @object_path "/org/example/RebusIntegration"
  @signal_interface "org.example.RebusIntegration.Events"

  # Every reply and signal crosses the daemon socket.
  @call_timeout 5_000
  @window 2_000

  describe "session bus" do
    test "connects, is given a unique name, and can list the daemon's names" do
      conn = connect!()

      assert {:ok, %Message{type: :method_return, body: [names]}} = driver_call(conn, "ListNames")
      assert is_list(names)
      assert @driver in names

      # Hello gave this connection a unique name, and the daemon reports it.
      assert unique_name!(conn) in names
    end

    test "routes NameOwnerChanged to a subscriber while a peer takes and releases a name" do
      observer = connect!()
      owner = connect!()

      rule =
        MatchRule.new!(
          sender: @driver,
          path: @driver_path,
          interface: @driver,
          member: "NameOwnerChanged",
          args: %{0 => @well_known_name}
        )

      assert {:ok, ref} = Rebus.add_match(observer, rule, @call_timeout)

      # 1 is DBUS_REQUEST_NAME_REPLY_PRIMARY_OWNER.
      assert {:ok, %Message{type: :method_return, body: [1]}} =
               driver_call(owner, "RequestName", signature: "su", body: [@well_known_name, 0])

      assert_receive {^ref, %Message{type: :signal} = acquired}, @window
      assert [@well_known_name, "", new_owner] = acquired.body
      assert new_owner == unique_name!(owner)

      # 1 is DBUS_RELEASE_NAME_REPLY_RELEASED.
      assert {:ok, %Message{type: :method_return, body: [1]}} =
               driver_call(owner, "ReleaseName", signature: "s", body: [@well_known_name])

      assert_receive {^ref, %Message{type: :signal} = released}, @window
      assert [@well_known_name, ^new_owner, ""] = released.body

      assert :ok = Rebus.remove_match(observer, ref, @call_timeout)
    end

    test "returns UnknownMethod for a method call routed to another connection" do
      caller = connect!()
      callee = connect!()

      message =
        Message.new!(:method_call,
          destination: unique_name!(callee),
          path: @object_path,
          interface: @signal_interface,
          member: "NoSuchMethod"
        )

      assert {:error,
              %Message{
                type: :error,
                header_fields: %{error_name: "org.freedesktop.DBus.Error.UnknownMethod"}
              }} = Rebus.call(caller, message, @call_timeout)
    end

    test "answers org.freedesktop.DBus.Peer on a peer connection and on the daemon" do
      caller = connect!()
      callee = connect!()
      callee_name = unique_name!(callee)

      assert {:ok, %Message{type: :method_return, body: []}} =
               Rebus.call(caller, peer_call(callee_name, "Ping"), @call_timeout)

      reply = Rebus.call(caller, peer_call(callee_name, "GetMachineId"), @call_timeout)

      # Whether a Rebus connection can answer GetMachineId depends on the host
      # having a machine-id file, so the expectation branches on the same read
      # the connection performs.
      case Rebus.MachineId.read() do
        {:ok, machine_id} ->
          assert {:ok, %Message{type: :method_return, body: [^machine_id]}} = reply

        {:error, :unavailable} ->
          assert {:error,
                  %Message{
                    type: :error,
                    header_fields: %{error_name: "org.freedesktop.DBus.Error.Failed"}
                  }} = reply
      end

      # The daemon always has one.
      assert {:ok, %Message{type: :method_return, body: [daemon_id]}} =
               Rebus.call(caller, peer_call(@driver, "GetMachineId"), @call_timeout)

      assert daemon_id =~ ~r/\A[0-9a-f]{32}\z/
    end

    test "delivers a broadcast signal from one connection to a matched subscriber" do
      subscriber = connect!()
      emitter = connect!()

      # A broadcast signal reaches a connection on a real bus only once the
      # daemon holds a match rule for it; the catch-all signal handler alone
      # would never see it.
      rule = MatchRule.new!(interface: @signal_interface, member: "Pinged")

      assert {:ok, match_ref} = Rebus.add_match(subscriber, rule, @call_timeout)
      assert {:ok, handler_ref} = Rebus.add_signal_handler(subscriber)

      signal =
        Message.new!(:signal,
          path: @object_path,
          interface: @signal_interface,
          member: "Pinged",
          signature: "s",
          body: ["from-the-bus"]
        )

      assert :ok = Rebus.send(emitter, signal)

      assert_receive {^match_ref, %Message{type: :signal, body: ["from-the-bus"]}}, @window
      assert_receive {^handler_ref, %Message{type: :signal, body: ["from-the-bus"]}}, @window

      assert :ok = Rebus.delete_signal_handler(subscriber, handler_ref)
      assert :ok = Rebus.remove_match(subscriber, match_ref, @call_timeout)
    end

    test "refuses an FD-bearing method call over the daemon socket without leaking it" do
      # The daemon does pass the descriptor: both connections negotiated
      # NEGOTIATE_UNIX_FD over the session socket. What cannot be exercised yet
      # is a *successful* transfer, because Rebus has no service-side API - a
      # connection answers every inbound method call with UnknownMethod, and
      # FD-bearing signals are dropped by design. So this test pins the two
      # halves that are observable today: the call is refused, and the
      # descriptor the receiving connection was handed is closed rather than
      # retained. Revisit once method calls can be served.
      caller = connect!()
      callee = connect!()
      fd = borrowed_fd!()

      before = open_fd_count()

      message =
        Message.new!(:method_call,
          destination: unique_name!(callee),
          path: @object_path,
          interface: @signal_interface,
          member: "TakeDescriptor",
          signature: "h",
          body: [0],
          fds: [fd]
        )

      assert {:error,
              %Message{
                type: :error,
                header_fields: %{error_name: "org.freedesktop.DBus.Error.UnknownMethod"}
              }} = Rebus.call(caller, message, @call_timeout)

      # /proc is Linux-only; elsewhere the refusal above is all this asserts.
      if before do
        assert eventually(fn -> open_fd_count() <= before end),
               "the received descriptor was not closed: #{open_fd_count()} open, #{before} before"
      end
    end

    test "connects to a non-bus peer endpoint with bus: false and completes a Ping" do
      # Not the daemon: `dbus-daemon` is always a bus, and Rebus has no
      # listener, so the only peer-mode endpoint available is the test server.
      # It is a real socket speaking the real handshake, which is what
      # `bus: false` needs to be exercised end to end.
      path = "/tmp/rebus-integration-peer-#{System.unique_integer([:positive])}.sock"

      {:ok, server} =
        start_supervised(
          {Rebus.TestServer, tap: self(), family: :local, path: path, auto_hello: false}
        )

      on_exit(fn -> File.rm(path) end)

      {:ok, address} = Rebus.TestServer.get_listen_addr(server)
      assert {:ok, conn} = Rebus.connect(address, bus: false)
      on_exit(fn -> Rebus.close(conn) end)

      ping =
        Message.new!(:method_call,
          path: @driver_path,
          interface: "org.freedesktop.DBus.Peer",
          member: "Ping"
        )

      task = Task.async(fn -> Rebus.call(conn, ping, @call_timeout) end)

      assert_receive {^server,
                      %Message{type: :method_call, header_fields: %{member: "Ping"}} = call},
                     @window

      # Serial 1 proves no Hello preceded it: `bus: false` sent nothing itself.
      assert call.serial == 1

      :ok = Rebus.TestServer.push(server, Message.new!(:method_return, reply_serial: call.serial))

      assert {:ok, %Message{type: :method_return, body: []}} = Task.await(task, @window)
    end
  end

  defp connect! do
    {:ok, conn} = Rebus.connect(:session)
    on_exit(fn -> Rebus.close(conn) end)
    conn
  end

  defp driver_call(conn, member, opts \\ []) do
    message =
      Message.new!(
        :method_call,
        Keyword.merge(
          [
            destination: @driver,
            path: @driver_path,
            interface: @driver,
            member: member
          ],
          opts
        )
      )

    Rebus.call(conn, message, @call_timeout)
  end

  defp peer_call(destination, member) do
    Message.new!(:method_call,
      destination: destination,
      path: @object_path,
      interface: "org.freedesktop.DBus.Peer",
      member: member
    )
  end

  # Rebus exposes no accessor for the name Hello returned, so it is read back
  # off the wire: every driver reply is directed at the caller's unique name.
  defp unique_name!(conn) do
    assert {:ok, %Message{header_fields: %{destination: name}}} = driver_call(conn, "GetId")
    assert String.starts_with?(name, ":")
    name
  end

  # A descriptor the VM keeps open for the life of the test. Rebus borrows
  # outbound descriptors and never closes them, so the listener stays valid.
  defp borrowed_fd! do
    path = "/tmp/rebus-integration-fd-#{System.unique_integer([:positive])}.sock"
    {:ok, listener} = :socket.open(:local, :stream, :default)
    :ok = :socket.bind(listener, %{family: :local, path: path})
    :ok = :socket.listen(listener, 1)

    on_exit(fn ->
      :socket.close(listener)
      File.rm(path)
    end)

    {:ok, fd} = :socket.getopt(listener, {:otp, :fd})
    fd
  end

  # Descriptor numbers are compared by count rather than by set: listing the
  # directory itself consumes a descriptor whose number varies between reads.
  defp open_fd_count do
    case :os.type() do
      {:unix, :linux} -> length(File.ls!("/proc/self/fd"))
      _other -> nil
    end
  end

  defp eventually(predicate, attempts \\ 200)

  defp eventually(predicate, 0), do: predicate.()

  defp eventually(predicate, attempts) do
    if predicate.() do
      true
    else
      Process.sleep(10)
      eventually(predicate, attempts - 1)
    end
  end
end
