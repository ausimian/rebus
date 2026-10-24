defmodule Rebus.ObjectTest do
  use ExUnit.Case

  import ExUnit.CaptureLog

  alias Rebus.Message
  alias Rebus.Service
  alias Rebus.Service.Call
  alias Rebus.TestServer

  @path "/org/example/Calc"

  defmodule Calc do
    use Rebus.Object

    @calculator Rebus.Interface.new!("org.example.Calculator",
                  methods: [
                    {"Add", in: [a: "i", b: "i"], out: [sum: "i"]},
                    {"SlowAdd", in: [a: "i", b: "i"], out: [sum: "i"]},
                    {"Divide", in: [a: "i", b: "i"], out: [quotient: "i"]},
                    {"Crash", in: [], out: []},
                    {"Garble", in: [], out: [sum: "i"]},
                    {"Stop", in: [], out: []},
                    {"Bare", in: [], out: []}
                  ],
                  properties: [
                    {"Precision", "u", :readwrite},
                    {"Cache", "as", :readwrite, emits_changed: :invalidates},
                    {"Quiet", "u", :readwrite, emits_changed: false},
                    {"Version", "s", :read, emits_changed: :const},
                    {"Secret", "s", :write}
                  ]
                )

    @faulty Rebus.Interface.new!("org.example.Faulty",
              properties: [{"Fine", "u", :read}, {"Unreadable", "u", :read}]
            )

    def calculator, do: @calculator

    @impl true
    def interfaces, do: [@calculator, @faulty]

    @impl true
    def init({:stop, reason}), do: {:stop, reason}
    def init(:ignore), do: :ignore
    def init(:raise), do: raise("init failed")

    def init(parent) do
      send(parent, {:object_started, self()})
      {:ok, %{parent: parent, precision: 2, cache: [], quiet: 0}}
    end

    @impl true
    def handle_method(%Call{member: "Add"}, [a, b], state), do: {:reply, [a + b], state}

    def handle_method(%Call{member: "SlowAdd"} = call, [a, b], state) do
      Task.start(fn ->
        send(state.parent, {:slow_reply, Service.reply(call, [a + b])})
      end)

      {:noreply, state}
    end

    def handle_method(%Call{member: "Divide"}, [_a, 0], state),
      do: {:error, "org.example.Error.DivideByZero", "Division by zero", state}

    def handle_method(%Call{member: "Divide"}, [a, b], state), do: {:reply, [div(a, b)], state}
    def handle_method(%Call{member: "Crash"}, [], _state), do: raise("boom")
    def handle_method(%Call{member: "Garble"}, [], state), do: {:reply, ["five"], state}
    def handle_method(%Call{member: "Stop"}, [], state), do: {:stop, :normal, state}
    def handle_method(%Call{member: "Bare"}, [], state), do: {:reply, [], state, fds: []}

    @impl true
    def handle_get_property(_interface, "Precision", state), do: {:ok, state.precision, state}
    def handle_get_property(_interface, "Cache", state), do: {:ok, state.cache, state}
    def handle_get_property(_interface, "Quiet", state), do: {:ok, state.quiet, state}
    def handle_get_property(_interface, "Version", state), do: {:ok, "1.0", state}
    def handle_get_property(_interface, "Fine", state), do: {:ok, 1, state}

    def handle_get_property(_interface, "Unreadable", state),
      do: {:error, "org.example.Error.Unreadable", "Cannot read", state}

    @impl true
    def handle_set_property(_interface, "Precision", precision, state) when precision > 16,
      do: {:error, "org.freedesktop.DBus.Error.InvalidArgs", "Precision above 16", state}

    def handle_set_property(_interface, "Precision", 13, state),
      do: {:ok, %{state | precision: 12}, emit: false}

    def handle_set_property(_interface, "Precision", precision, state),
      do: {:ok, %{state | precision: precision}}

    def handle_set_property(_interface, "Cache", cache, state), do: {:ok, %{state | cache: cache}}
    def handle_set_property(_interface, "Quiet", quiet, state), do: {:ok, %{state | quiet: quiet}}
    def handle_set_property(_interface, "Secret", _secret, state), do: {:ok, state}

    @impl true
    def handle_info(:stop, state), do: {:stop, :normal, state}

    def handle_info(msg, state) do
      send(state.parent, {:object_info, msg})
      {:noreply, state}
    end

    @impl true
    def terminate(reason, state), do: send(state.parent, {:object_terminated, reason})
  end

  # An object with properties but neither property callback.
  defmodule Bare do
    use Rebus.Object, restart: :transient, id: :bare

    @impl true
    def interfaces do
      [
        Rebus.Interface.new!("org.example.Bare",
          methods: [{"Ping", in: [], out: []}],
          properties: [{"Level", "u", :readwrite}]
        )
      ]
    end

    @impl true
    def init(_arg), do: {:ok, nil}

    @impl true
    def handle_method(%Call{member: "Ping"}, [], state), do: {:reply, [], state}
  end

  # A GenServer of its own, which exports itself and maps calls with dispatch.
  defmodule Counter do
    use GenServer

    @counter Rebus.Interface.new!("org.example.Counter",
               methods: [{"Increment", in: [by: "u"], out: [count: "u"]}],
               properties: [{"Count", "u", :read}]
             )

    def start_link(conn), do: GenServer.start_link(__MODULE__, conn)
    def count(pid), do: GenServer.call(pid, :count)

    def interfaces, do: [@counter]

    @impl true
    def init(conn) do
      {:ok, _ref} = Service.export(conn, "/org/example/Counter", interfaces())
      {:ok, 0}
    end

    @impl true
    def handle_call(:count, _from, count), do: {:reply, count, count}

    @impl true
    def handle_info({_ref, %Call{}} = call, count),
      do: Rebus.Object.dispatch(call, __MODULE__, count)

    def handle_method(%Call{member: "Increment"}, [by], count),
      do: {:reply, [count + by], count + by}

    def handle_get_property("org.example.Counter", "Count", count), do: {:ok, count, count}
  end

  setup [:server_setup, :client_setup]

  describe "start_link/3" do
    test "exports a minimal object that answers its methods", %{cli: cli, svr: svr} do
      {:ok, pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)
      assert_receive {:object_started, ^pid}

      {:ok, serial} = TestServer.push_call(svr, calc_call("Add", [2, 3]))
      assert_reply(svr, serial, [5])

      {:ok, serial} = TestServer.push_call(svr, calc_call("Divide", [7, 0]))
      assert_error(svr, serial, "org.example.Error.DivideByZero", "Division by zero")

      {:ok, serial} = TestServer.push_call(svr, calc_call("Bare", []))
      assert_reply(svr, serial, [])
    end

    test "answers later from another process for {:noreply, state}", %{cli: cli, svr: svr} do
      {:ok, _pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)

      {:ok, serial} = TestServer.push_call(svr, calc_call("SlowAdd", [2, 3]))
      assert_receive {:slow_reply, :ok}, 1_000
      assert_reply(svr, serial, [5])
    end

    test "exports the interfaces given instead of the module's", %{cli: cli, svr: svr} do
      only_add =
        Rebus.Interface.new!("org.example.Calculator",
          methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}]
        )

      {:ok, _pid} =
        Rebus.Object.start_link(Calc, self(), conn: cli, path: @path, interfaces: [only_add])

      {:ok, serial} = TestServer.push_call(svr, calc_call("Add", [2, 3]))
      assert_reply(svr, serial, [5])

      {:ok, serial} = TestServer.push_call(svr, calc_call("Divide", [6, 3]))
      assert_error(svr, serial, "org.freedesktop.DBus.Error.UnknownMethod")
    end

    test "returns an export error without exiting the caller", %{cli: cli} do
      {:ok, _ref} = Service.export(cli, @path, [Calc.calculator()])

      assert {:error, {:already_exported, @path, "org.example.Calculator"}} =
               Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)

      refute_received {:object_started, _pid}
    end

    test "withdraws the export when init stops or ignores", %{cli: cli} do
      Process.flag(:trap_exit, true)

      assert {:error, :refused} =
               Rebus.Object.start_link(Calc, {:stop, :refused}, conn: cli, path: @path)

      assert :ignore = Rebus.Object.start_link(Calc, :ignore, conn: cli, path: @path)

      assert {:ok, _ref} = Service.export(cli, @path, [Calc.calculator()])
    end

    test "withdraws the export when init raises", %{cli: cli} do
      Process.flag(:trap_exit, true)

      capture_log(fn ->
        assert {:error, {%RuntimeError{message: "init failed"}, _stack}} =
                 Rebus.Object.start_link(Calc, :raise, conn: cli, path: @path)
      end)

      assert {:ok, _ref} = Service.export(cli, @path, [Calc.calculator()])
    end

    test "child specs are permanent, as for GenServer, unless use says otherwise" do
      spec = Calc.child_spec({2, []})
      assert %{id: Calc, start: {Rebus.Object, :start_link, [Calc, 2, []]}} = spec
      # No :restart key, so the supervisor applies its :permanent default.
      refute Map.has_key?(spec, :restart)

      assert %{id: :bare, restart: :transient} = Bare.child_spec({nil, []})
    end

    test "registers a name and refuses unknown or missing options", %{cli: cli} do
      {:ok, pid} =
        Rebus.Object.start_link(Calc, self(), conn: cli, path: @path, name: :rebus_test_calc)

      assert Process.whereis(:rebus_test_calc) == pid

      assert_raise ArgumentError, fn ->
        Rebus.Object.start_link(Calc, self(), conn: cli, path: "/other", bogus: true)
      end

      assert_raise KeyError, fn -> Rebus.Object.start_link(Calc, self(), conn: cli) end
    end

    test "passes other messages to handle_info and stops through terminate", %{cli: cli, svr: svr} do
      {:ok, pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)

      send(pid, :hello)
      assert_receive {:object_info, :hello}, 1_000

      send(pid, :stop)
      assert_receive {:object_terminated, :normal}, 1_000
      {:ok, pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)
      assert Process.alive?(pid)

      {:ok, serial} = TestServer.push_call(svr, calc_call("Stop", []))
      assert_receive {:object_terminated, :normal}, 1_000

      assert_error(svr, serial, "org.freedesktop.DBus.Error.Failed")
      assert {:ok, _ref} = Service.export(cli, @path, [Calc.calculator()])
    end

    test "logs other messages when the module has no handle_info", %{cli: cli} do
      {:ok, pid} = Rebus.Object.start_link(Bare, nil, conn: cli, path: @path)

      log =
        capture_log(fn ->
          send(pid, :stray)
          :sys.get_state(pid)
        end)

      assert log =~ "received unexpected message in handle_info/2: :stray"
      assert Process.alive?(pid)
    end
  end

  describe "properties" do
    setup %{cli: cli} do
      {:ok, pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)
      %{obj: pid}
    end

    test "answers Get and GetAll from handle_get_property", %{svr: svr} do
      {:ok, serial} = TestServer.push_call(svr, properties_call("Get", "ss", ["Precision"]))
      assert_reply(svr, serial, [{"u", 2}])

      {:ok, serial} = TestServer.push_call(svr, properties_call("GetAll", "s", []))

      assert_reply(svr, serial, [
        [
          {"Cache", {"as", []}},
          {"Precision", {"u", 2}},
          {"Quiet", {"u", 0}},
          {"Version", {"s", "1.0"}}
        ]
      ])
    end

    test "answers Get and GetAll with the callback's error", %{svr: svr} do
      for {member, signature, args} <- [{"Get", "ss", ["Unreadable"]}, {"GetAll", "s", []}] do
        {:ok, serial} =
          TestServer.push_call(
            svr,
            properties_call(member, signature, args, "org.example.Faulty")
          )

        assert_error(svr, serial, "org.example.Error.Unreadable", "Cannot read")
      end
    end

    test "emits PropertiesChanged with the value set", %{svr: svr, obj: obj} do
      {:ok, serial} = TestServer.push_call(svr, set_call("Precision", {"u", 7}))
      assert_reply(svr, serial, [])

      assert_properties_changed(svr, [{"Precision", {"u", 7}}], [])
      assert :sys.get_state(obj).state.precision == 7
    end

    test "emits only the name of a property that invalidates", %{svr: svr} do
      {:ok, serial} = TestServer.push_call(svr, set_call("Cache", {"as", ["a"]}))
      assert_reply(svr, serial, [])

      assert_properties_changed(svr, [], ["Cache"])
    end

    test "emits nothing for a property that does not signal changes or is write-only", %{
      svr: svr,
      obj: obj
    } do
      {:ok, serial} = TestServer.push_call(svr, set_call("Quiet", {"u", 4}))
      assert_reply(svr, serial, [])

      {:ok, serial} = TestServer.push_call(svr, set_call("Secret", {"s", "hush"}))
      assert_reply(svr, serial, [])

      refute_receive {^svr, %Message{type: :signal}}, 100
      assert :sys.get_state(obj).state.quiet == 4
    end

    test "emits nothing when the callback returns emit: false", %{svr: svr, obj: obj} do
      {:ok, serial} = TestServer.push_call(svr, set_call("Precision", {"u", 13}))
      assert_reply(svr, serial, [])

      refute_receive {^svr, %Message{type: :signal}}, 100
      assert :sys.get_state(obj).state.precision == 12
    end

    test "answers a refused Set with the callback's error and emits nothing", %{svr: svr} do
      {:ok, serial} = TestServer.push_call(svr, set_call("Precision", {"u", 17}))
      assert_error(svr, serial, "org.freedesktop.DBus.Error.InvalidArgs", "Precision above 16")

      refute_receive {^svr, %Message{type: :signal}}, 100
    end

    test "answers NotSupported without the property callbacks", %{cli: cli, svr: svr, obj: obj} do
      GenServer.stop(obj)
      {:ok, _pid} = Rebus.Object.start_link(Bare, nil, conn: cli, path: @path)

      for {member, signature, args} <- [
            {"Get", "ss", ["Level"]},
            {"GetAll", "s", []},
            {"Set", "ssv", ["Level", {"u", 1}]}
          ] do
        {:ok, serial} =
          TestServer.push_call(svr, properties_call(member, signature, args, "org.example.Bare"))

        assert_error(svr, serial, "org.freedesktop.DBus.Error.NotSupported", "Not supported")
      end
    end
  end

  describe "failures" do
    test "a crash fails the call and a supervisor restart re-exports", %{cli: cli, svr: svr} do
      {:ok, sup} =
        Supervisor.start_link([{Calc, {self(), conn: cli, path: @path}}], strategy: :one_for_one)

      assert_receive {:object_started, first}, 1_000

      log =
        capture_log(fn ->
          {:ok, serial} = TestServer.push_call(svr, calc_call("Crash", []))
          assert_error(svr, serial, "org.freedesktop.DBus.Error.Failed")
          assert_receive {:object_started, second}, 1_000
          assert second != first
        end)

      assert log =~ "boom"

      {:ok, serial} = TestServer.push_call(svr, calc_call("Add", [2, 3]))
      assert_reply(svr, serial, [5])

      Supervisor.stop(sup)
    end

    test "a reply that does not fit stops the object", %{cli: cli, svr: svr} do
      Process.flag(:trap_exit, true)
      {:ok, pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)

      capture_log(fn ->
        {:ok, serial} = TestServer.push_call(svr, calc_call("Garble", []))
        assert_receive {:EXIT, ^pid, {:invalid_reply, :signature_mismatch}}, 1_000
        assert_error(svr, serial, "org.freedesktop.DBus.Error.Failed")
      end)
    end

    test "stops with the connection", %{cli: cli} do
      Process.flag(:trap_exit, true)
      {:ok, pid} = Rebus.Object.start_link(Calc, self(), conn: cli, path: @path)

      :ok = Rebus.close(cli)

      assert_receive {:object_terminated, {:shutdown, :disconnected}}, 1_000
      assert_receive {:EXIT, ^pid, {:shutdown, :disconnected}}, 1_000
    end
  end

  describe "dispatch/4" do
    test "maps the calls of a GenServer that keeps its own loop", %{cli: cli, svr: svr} do
      {:ok, pid} = Counter.start_link(cli)

      {:ok, serial} =
        TestServer.push_call(
          svr,
          Message.new!(:method_call,
            path: "/org/example/Counter",
            interface: "org.example.Counter",
            member: "Increment",
            sender: ":1.99",
            signature: "u",
            body: [3]
          )
        )

      assert_reply(svr, serial, [3])
      assert Counter.count(pid) == 3

      {:ok, serial} =
        TestServer.push_call(
          svr,
          properties_call("Get", "ss", ["Count"], "org.example.Counter", "/org/example/Counter")
        )

      assert_reply(svr, serial, [{"u", 3}])
    end
  end

  defp server_setup(_) do
    {:ok, svr} = start_supervised({TestServer, tap: self()})
    %{svr: svr}
  end

  defp client_setup(%{svr: svr}) do
    {:ok, addr} = TestServer.get_listen_addr(svr)
    {:ok, cli} = Rebus.connect(addr)
    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}, 1_000
    on_exit(fn -> Rebus.close(cli) end)
    %{cli: cli}
  end

  defp calc_call(member, body) do
    Message.new!(:method_call,
      path: @path,
      interface: "org.example.Calculator",
      member: member,
      sender: ":1.99",
      signature: String.duplicate("i", length(body)),
      body: body
    )
  end

  defp properties_call(
         member,
         signature,
         args,
         interface \\ "org.example.Calculator",
         path \\ @path
       ) do
    Message.new!(:method_call,
      path: path,
      interface: "org.freedesktop.DBus.Properties",
      member: member,
      sender: ":1.99",
      signature: signature,
      body: [interface | args]
    )
  end

  defp set_call(property, value), do: properties_call("Set", "ssv", [property, value])

  defp assert_reply(svr, serial, body) do
    assert_receive {^svr,
                    %Message{type: :method_return, header_fields: %{reply_serial: ^serial}} =
                      reply},
                   1_000

    assert reply.body == body
  end

  defp assert_error(svr, serial, name, text \\ nil) do
    assert_receive {^svr,
                    %Message{
                      type: :error,
                      header_fields: %{reply_serial: ^serial, error_name: ^name}
                    } = error},
                   1_000

    if text, do: assert(error.body == [text])
  end

  defp assert_properties_changed(svr, changed, invalidated) do
    assert_receive {^svr,
                    %Message{
                      type: :signal,
                      header_fields: %{member: "PropertiesChanged"} = fields
                    } =
                      signal},
                   1_000

    assert fields.path == @path
    assert fields.interface == "org.freedesktop.DBus.Properties"
    assert signal.body == ["org.example.Calculator", changed, invalidated]
  end
end
