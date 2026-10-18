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
  alias Rebus.Service
  alias Rebus.Service.Call

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
  @calculator Rebus.Interface.new!("org.example.RebusIntegration.Calculator",
                methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}]
              )
  @settings Rebus.Interface.new!("org.example.RebusIntegration.Settings",
              properties: [{"Precision", "u", :readwrite}, {"Version", "s", :read}]
            )

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

    test "reports the unique name Hello assigned" do
      conn = connect!()

      # The daemon addresses its replies to the name it assigned, so the
      # accessor is checked against the wire rather than against itself.
      assert {:ok, name} = Rebus.unique_name(conn)
      assert name == unique_name!(conn)

      assert {:ok, %Message{body: [^name]}} =
               driver_call(conn, "GetNameOwner", signature: "s", body: [name])
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

    test "returns UnknownObject for a call to a path another connection has not exported" do
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
              {:dbus_error,
               %Message{
                 type: :error,
                 header_fields: %{error_name: "org.freedesktop.DBus.Error.UnknownObject"}
               }}} = Rebus.call(caller, message, @call_timeout)
    end

    test "serves an exported object to another Rebus connection" do
      caller = connect!()
      exporter = connect!()
      {:ok, ref} = Service.export(exporter, @object_path, [@calculator])
      caller_name = unique_name!(caller)

      add = fn opts ->
        Message.new!(
          :method_call,
          Keyword.merge(
            [
              destination: unique_name!(exporter),
              path: @object_path,
              member: "Add",
              signature: "ii",
              body: [2, 3]
            ],
            opts
          )
        )
      end

      # With the interface, and without it: only one exported interface
      # declares Add, so the exporter resolves it.
      for message <- [add.(interface: "org.example.RebusIntegration.Calculator"), add.([])] do
        task = Task.async(fn -> Rebus.call(caller, message, @call_timeout) end)

        assert_receive {^ref, %Call{member: "Add", body: [2, 3], sender: ^caller_name} = call},
                       @window

        assert call.interface == "org.example.RebusIntegration.Calculator"
        assert :ok = Service.reply(call, [5])
        assert {:ok, %Message{type: :method_return, body: [5]}} = Task.await(task, @window)
      end

      assert {:error,
              {:dbus_error,
               %Message{header_fields: %{error_name: "org.freedesktop.DBus.Error.InvalidArgs"}}}} =
               Rebus.call(caller, add.(signature: "s", body: ["2"]), @call_timeout)

      task = Task.async(fn -> Rebus.call(caller, add.([]), @call_timeout) end)
      assert_receive {^ref, %Call{} = call}, @window
      assert :ok = Service.reply_error(call, "org.example.RebusIntegration.Error.Overflow", "no")

      assert {:error,
              {:dbus_error,
               %Message{
                 header_fields: %{error_name: "org.example.RebusIntegration.Error.Overflow"},
                 body: ["no"]
               }}} = Task.await(task, @window)
    end

    test "serves an exported object to dbus-send" do
      exporter = connect!()
      {:ok, ref} = Service.export(exporter, @object_path, [@calculator])

      args = [
        "--session",
        "--print-reply",
        "--dest=#{unique_name!(exporter)}",
        @object_path,
        "org.example.RebusIntegration.Calculator.Add",
        "int32:2",
        "int32:3"
      ]

      task = Task.async(fn -> System.cmd("dbus-send", args, stderr_to_stdout: true) end)

      assert_receive {^ref, %Call{member: "Add", body: [2, 3]} = call}, @window
      assert :ok = Service.reply(call, [5])

      assert {output, 0} = Task.await(task, @window)
      assert output =~ "int32 5"
    end

    test "answers Introspect on the object and on every node above it" do
      caller = connect!()
      exporter = connect!()
      {:ok, _ref} = Service.export(exporter, @object_path, [@calculator])

      introspect = fn path ->
        message =
          Message.new!(:method_call,
            destination: unique_name!(exporter),
            path: path,
            interface: "org.freedesktop.DBus.Introspectable",
            member: "Introspect"
          )

        assert {:ok, %Message{type: :method_return, body: [xml]}} =
                 Rebus.call(caller, message, @call_timeout)

        xml
      end

      assert introspect.("/") =~ ~s(<node name="org"/>)
      assert introspect.("/org/example") =~ ~s(<node name="RebusIntegration"/>)

      object = introspect.(@object_path)
      assert object =~ ~s(<interface name="org.example.RebusIntegration.Calculator">)
      assert object =~ ~s(<interface name="org.freedesktop.DBus.Peer">)
      refute object =~ "<node name="
    end

    test "answers dbus-send's introspection request" do
      exporter = connect!()
      {:ok, _ref} = Service.export(exporter, @object_path, [@calculator])

      args = [
        "--session",
        "--print-reply",
        "--dest=#{unique_name!(exporter)}",
        "/org",
        "org.freedesktop.DBus.Introspectable.Introspect"
      ]

      assert {output, 0} = System.cmd("dbus-send", args, stderr_to_stdout: true)
      assert output =~ ~s(<interface name="org.freedesktop.DBus.Introspectable">)
      assert output =~ ~s(<node name="example"/>)
    end

    test "serves Properties Get, Set and GetAll to another Rebus connection" do
      caller = connect!()
      exporter = connect!()
      {:ok, ref} = Service.export(exporter, @object_path, [@settings])

      properties = fn member, signature, args ->
        message =
          Message.new!(:method_call,
            destination: unique_name!(exporter),
            path: @object_path,
            interface: "org.freedesktop.DBus.Properties",
            member: member,
            signature: signature,
            body: ["org.example.RebusIntegration.Settings" | args]
          )

        Task.async(fn -> Rebus.call(caller, message, @call_timeout) end)
      end

      task = properties.("Get", "ss", ["Precision"])
      assert_receive {^ref, %Call{member: "Get"} = call}, @window
      assert :ok = Service.reply(call, [{"u", 3}])
      assert {:ok, %Message{body: [{"u", 3}]}} = Task.await(task, @window)

      task = properties.("Set", "ssv", ["Precision", {"u", 4}])

      assert_receive {^ref,
                      %Call{member: "Set", body: [_interface, "Precision", {"u", 4}]} = call},
                     @window

      assert :ok = Service.reply(call, [])
      assert {:ok, %Message{type: :method_return, body: []}} = Task.await(task, @window)

      task = properties.("GetAll", "s", [])
      assert_receive {^ref, %Call{member: "GetAll"} = call}, @window
      values = %{"Precision" => 4, "Version" => "1.0"}
      assert :ok = Service.reply(call, [Rebus.Interface.wrap_properties!(@settings, values)])

      assert {:ok, %Message{body: [[{"Precision", {"u", 4}}, {"Version", {"s", "1.0"}}]]}} =
               Task.await(task, @window)

      task = properties.("Set", "ssv", ["Version", {"s", "2.0"}])

      assert {:error,
              {:dbus_error,
               %Message{
                 header_fields: %{error_name: "org.freedesktop.DBus.Error.PropertyReadOnly"}
               }}} = Task.await(task, @window)
    end

    test "delivers PropertiesChanged to a subscriber's match rule" do
      subscriber = connect!()
      exporter = connect!()
      {:ok, _ref} = Service.export(exporter, @object_path, [@settings])

      rule =
        MatchRule.new!(
          sender: unique_name!(exporter),
          path: @object_path,
          interface: "org.freedesktop.DBus.Properties",
          member: "PropertiesChanged",
          args: %{0 => "org.example.RebusIntegration.Settings"}
        )

      assert {:ok, match_ref} = Rebus.add_match(subscriber, rule, @call_timeout)

      assert :ok =
               Service.properties_changed(
                 exporter,
                 @object_path,
                 @settings,
                 %{"Precision" => 5},
                 []
               )

      assert_receive {^match_ref, %Message{type: :signal} = signal}, @window
      assert Message.signature(signal) == "sa{sv}as"

      assert signal.body == [
               "org.example.RebusIntegration.Settings",
               [{"Precision", {"u", 5}}],
               []
             ]

      assert :ok = Rebus.remove_match(subscriber, match_ref, @call_timeout)
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
                  {:dbus_error,
                   %Message{
                     type: :error,
                     header_fields: %{error_name: "org.freedesktop.DBus.Error.Failed"}
                   }}} = reply
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
      # is a *successful* transfer, because inbound calls do not yet hand their
      # descriptors to a handler, and FD-bearing signals are dropped by design.
      # So this test pins the two halves that are observable today: the call
      # to a path with nothing exported is refused, and the descriptor the
      # receiving connection was handed is closed rather than retained.
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
              {:dbus_error,
               %Message{
                 type: :error,
                 header_fields: %{error_name: "org.freedesktop.DBus.Error.UnknownObject"}
               }}} = Rebus.call(caller, message, @call_timeout)

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
      path = Rebus.TestTmp.path("int-peer")

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

  # The name Hello returned, read back off the wire independently of
  # `Rebus.unique_name/1`: every driver reply is directed at the caller's
  # unique name.
  defp unique_name!(conn) do
    assert {:ok, %Message{header_fields: %{destination: name}}} = driver_call(conn, "GetId")
    assert String.starts_with?(name, ":")
    name
  end

  # A descriptor the VM keeps open for the life of the test. Rebus borrows
  # outbound descriptors and never closes them, so the listener stays valid.
  defp borrowed_fd! do
    path = Rebus.TestTmp.path("int-fd")
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
