defmodule Rebus.Connection.DispatchTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog, only: [capture_log: 1]

  require Record

  alias Rebus.Connection
  alias Rebus.Connection.Dispatch
  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.InboundCalls
  alias Rebus.Connection.Objects
  alias Rebus.Connection.Pending
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Service
  alias Rebus.Connection.Writer
  alias Rebus.Interface
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.ScriptedTransport
  alias Rebus.Service.Call
  alias Rebus.TestFD

  # xmerl only checks the documents Introspect answers with; it is not a
  # runtime dependency of Rebus.
  Record.defrecordp(
    :xml_attribute,
    :xmlAttribute,
    Record.extract(:xmlAttribute, from_lib: "xmerl/include/xmerl.hrl")
  )

  @machine_id "0123456789abcdef0123456789abcdef"
  @unknown_object_error "org.freedesktop.DBus.Error.UnknownObject"
  @unknown_interface_error "org.freedesktop.DBus.Error.UnknownInterface"
  @unknown_method_error "org.freedesktop.DBus.Error.UnknownMethod"
  @invalid_args_error "org.freedesktop.DBus.Error.InvalidArgs"
  @limits_exceeded_error "org.freedesktop.DBus.Error.LimitsExceeded"
  @no_reply_error "org.freedesktop.DBus.Error.NoReply"
  @failed_error "org.freedesktop.DBus.Error.Failed"
  @unknown_property_error "org.freedesktop.DBus.Error.UnknownProperty"
  @property_read_only_error "org.freedesktop.DBus.Error.PropertyReadOnly"
  @introspectable "org.freedesktop.DBus.Introspectable"
  @properties "org.freedesktop.DBus.Properties"

  @path "/org/example/Calc"
  @calculator Interface.new!("org.example.Calculator",
                methods: [
                  {"Add", in: [a: "i", b: "i"], out: [sum: "i"]},
                  {"Reset", []}
                ]
              )
  @display Interface.new!("org.example.Display",
             methods: [{"Show", in: [text: "s"]}, {"Reset", []}]
           )
  @settings Interface.new!("org.example.Settings",
              properties: [
                {"Precision", "u", :readwrite},
                {"Version", "s", :read},
                {"Secret", "s", :write}
              ]
            )

  describe "reply correlation" do
    test "answers the caller of a correlated reply and clears its entry" do
      %{tag: tag, request_ref: request_ref, monitor_ref: monitor_ref} = entry = caller()
      state = pending(connection(), 11, entry)

      reply = Message.new!(:method_return, reply_serial: 11, signature: "s", body: ["ok"])

      assert {:continue, :recv, %Connection{} = state} = dispatch(reply, state)
      assert state.pending == Pending.new()
      refute Map.has_key?(state.pending.request_index, request_ref)
      refute Map.has_key?(state.pending.monitor_index, monitor_ref)
      assert_received {^tag, %Message{type: :method_return, body: ["ok"]}}
    end

    test "logs and drops a reply that correlates to nothing" do
      reply = Message.new!(:method_return, reply_serial: 9)

      log =
        capture_log(fn ->
          assert {:continue, :recv, %Connection{pending: pending}} = dispatch(reply, connection())
          assert pending == Pending.new()
        end)

      assert log =~ "Ignoring late or orphaned D-Bus reply for serial 9"
    end

    @tag skip: TestFD.skip_reason()
    test "opens a file-descriptor claim for a reply that carries descriptors" do
      fd = descriptor()
      %{tag: tag, request_ref: request_ref} = entry = caller()

      state =
        %{
          pending(connection(), 11, entry)
          | unix_fd_negotiated?: true,
            inbound_fds: Rights.retain(Rights.new(), [fd])
        }

      reply =
        Message.new!(:method_return,
          reply_serial: 11,
          signature: "h",
          body: [0],
          fds: [fd]
        )

      assert {:continue, :recv, %Connection{} = state} = dispatch(reply, state)
      assert state.pending == Pending.new()
      assert_received {^tag, {:fd_claim, claim_ref}}
      assert {:ok, ^claim_ref} = FDClaims.fetch_by_request(state.fd_claims, request_ref)

      # The claim still owns the descriptor, so this test closes it rather than
      # leaving it open for the life of the run.
      FDClaims.close_all(state.fd_claims)
    end

    test "fails a pending call whose reply hit a resource limit" do
      %{tag: tag} = entry = caller()
      state = pending(connection(), 11, entry)

      log =
        capture_log(fn ->
          assert {:continue, :recv, %Connection{pending: pending}} =
                   Dispatch.process_inbound(
                     %{state | inbound: Inbound.new(resource_limited_reply(11))},
                     :recv
                   )

          assert pending == Pending.new()
        end)

      assert log =~ "D-Bus frame dropped: :resource_limit"
      refute log =~ "resource-limit-body-sentinel"
      assert_received {^tag, {:error, {:reply_dropped, :method_return}}}
    end
  end

  describe "request_timeout/3" do
    test "answers a timed-out caller and clears its entry" do
      %{tag: tag, request_ref: request_ref} = entry = caller()
      state = pending(connection(), 11, entry)

      assert {:ok, %Connection{pending: pending}} =
               Dispatch.request_timeout(11, request_ref, state)

      assert pending == Pending.new()
      assert_received {^tag, {:error, :timeout}}
    end

    test "ignores a timeout for a request that already completed" do
      state = connection()

      assert {:ok, ^state} = Dispatch.request_timeout(11, make_ref(), state)
    end
  end

  describe "signals" do
    test "reaches only the handlers whose rule matches" do
      matching = make_ref()
      other = make_ref()

      state = %{
        connection()
        | name: ":1.42",
          handlers: %{
            matching => handler(MatchRule.new!(interface: "org.example.Wanted")),
            other => handler(MatchRule.new!(interface: "org.example.Ignored"))
          }
      }

      signal =
        Message.new!(:signal, path: "/test", interface: "org.example.Wanted", member: "Fired")

      assert {:continue, :recv, %Connection{}} = dispatch(signal, state)
      assert_received {^matching, %Message{header_fields: %{member: "Fired"}}}
      refute_received {^other, _msg}
    end

    test "delivers to a handler that registered no rule" do
      handler_ref = make_ref()
      state = %{connection() | handlers: %{handler_ref => handler(nil)}}

      signal =
        Message.new!(:signal, path: "/test", interface: "org.example.Any", member: "Fired")

      assert {:continue, :recv, %Connection{}} = dispatch(signal, state)
      assert_received {^handler_ref, %Message{header_fields: %{member: "Fired"}}}
    end

    test "swallows this connection's own NameAcquired signal" do
      handler_ref = make_ref()
      state = %{connection() | name: ":1.42", handlers: %{handler_ref => handler(nil)}}

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "NameAcquired",
          destination: ":1.42",
          signature: "s",
          body: [":1.42"]
        )

      assert {:continue, :recv, %Connection{}} = dispatch(signal, state)
      refute_received {^handler_ref, _msg}
    end
  end

  describe "inbound method calls" do
    test "queues an empty reply for org.freedesktop.DBus.Peer.Ping" do
      assert [%{kind: :reply, from: nil, msg: reply}] = queued_replies(peer_call("Ping"))
      assert %Message{type: :method_return, header_fields: fields} = reply
      assert fields.reply_serial == 5
      assert fields.destination == ":1.7"
      assert reply.body == []
      assert_received :advance_writes
    end

    test "queues the cached machine id for GetMachineId" do
      assert [%{msg: reply}] = queued_replies(peer_call("GetMachineId"))
      assert %Message{type: :method_return, body: [@machine_id]} = reply
      assert reply.header_fields.signature == "s"
    end

    test "reports an unavailable machine id as an error reply" do
      state = %{connection() | machine_id: :unavailable}

      assert [%{msg: reply}] = queued_replies(peer_call("GetMachineId"), state)
      assert %Message{type: :error, body: ["Machine ID unavailable"]} = reply
      assert reply.header_fields.error_name == @failed_error
    end

    test "refuses a call to a path with nothing exported with UnknownObject" do
      call =
        %{
          Message.new!(:method_call,
            path: "/test",
            interface: "org.example.Test",
            member: "Nope",
            sender: ":1.7"
          )
          | serial: 5
        }

      assert [%{msg: reply}] = queued_replies(call)
      assert %Message{type: :error, body: [detail]} = reply
      assert reply.header_fields.error_name == @unknown_object_error
      assert reply.header_fields.reply_serial == 5
      # The reply body must never echo caller-supplied data.
      refute detail =~ "Nope"
      refute detail =~ "/test"
    end

    test "refuses an unknown Peer member with UnknownMethod" do
      assert [%{msg: reply}] = queued_replies(peer_call("Nope"))
      assert %Message{type: :error, body: ["Method not handled by this connection"]} = reply
      assert reply.header_fields.error_name == @unknown_method_error
    end

    test "answers a peer-to-peer caller with no destination" do
      call =
        Message.new!(:method_call,
          path: "/",
          interface: "org.freedesktop.DBus.Peer",
          member: "Ping"
        )

      assert [%{msg: reply}] = queued_replies(call)
      refute Map.has_key?(reply.header_fields, :destination)
    end

    test "drops a call that asked for no reply" do
      call =
        Message.new!(:method_call,
          path: "/",
          interface: "org.freedesktop.DBus.Peer",
          member: "Ping",
          sender: ":1.7",
          flags: [:no_reply_expected]
        )

      assert queued_replies(call) == []
      refute_received :advance_writes
    end
  end

  describe "routing to exported objects" do
    test "hands a declared call to its handler and records it" do
      {export_ref, state} = exported(connection())
      call = app_call("Add", signature: "ii", body: [2, 3])

      assert {:continue, :recv, %Connection{} = state} = dispatch(call, state)

      assert_received {^export_ref, %Call{} = delivered}
      assert delivered.conn == self()
      assert delivered.export == export_ref
      assert delivered.serial == 5
      assert delivered.sender == ":1.7"
      assert delivered.path == @path
      assert delivered.interface == "org.example.Calculator"
      assert delivered.member == "Add"
      assert delivered.signature == "ii"
      assert delivered.out_signature == "i"
      assert delivered.body == [2, 3]
      assert delivered.flags == []
      refute delivered.no_reply?
      assert delivered.unix_fds == []
      assert delivered.message.header_fields.member == "Add"

      assert {:ok, entry} = InboundCalls.fetch(state.inbound_calls, delivered.ref)
      assert entry.serial == 5
      assert entry.sender == ":1.7"
      assert entry.export == export_ref
      assert entry.deadline == delivered.deadline
      assert is_integer(delivered.deadline)
      assert :queue.is_empty(state.writer.queue)

      InboundCalls.close_all(state.inbound_calls)
    end

    test "refuses an interface the object does not export with UnknownInterface" do
      {_export_ref, state} = exported(connection())

      call = app_call("Add", interface: "org.example.Missing", signature: "ii", body: [2, 3])
      assert_refused(call, state, @unknown_interface_error)
    end

    test "refuses an undeclared member with UnknownMethod" do
      {_export_ref, state} = exported(connection())

      assert_refused(
        app_call("Subtract", signature: "ii", body: [2, 3]),
        state,
        @unknown_method_error
      )
    end

    test "refuses arguments that differ from the declaration with InvalidArgs" do
      {_export_ref, state} = exported(connection())

      for {signature, body} <- [{"", []}, {"i", [2]}, {"iu", [2, 3]}, {"iii", [1, 2, 3]}] do
        call = app_call("Add", signature: signature, body: body)
        assert_refused(call, state, @invalid_args_error)
      end
    end

    test "refuses everything but Introspect on an intermediate node with UnknownInterface" do
      {_export_ref, state} = exported(connection())

      for {path, interface, member} <- [
            {"/org/example", "org.example.Calculator", "Add"},
            {"/", "org.freedesktop.DBus.Introspectable", "Nope"},
            {"/org", "org.freedesktop.DBus.Properties", "GetAll"},
            {"/org", nil, "Anything"},
            {"/org", nil, "Introspect"}
          ] do
        call = app_call(member, path: path, interface: interface)
        assert_refused(call, state, @unknown_interface_error)
      end
    end

    test "resolves a call without an interface to the one interface that declares it" do
      {export_ref, state} = exported(connection(), interfaces: [@calculator, @display])

      call = app_call("Show", interface: nil, signature: "s", body: ["hi"])
      assert {:continue, :recv, %Connection{} = state} = dispatch(call, state)

      assert_received {^export_ref, %Call{interface: "org.example.Display", member: "Show"}}
      InboundCalls.close_all(state.inbound_calls)
    end

    test "refuses a call without an interface that no interface, or several, declare" do
      {_export_ref, state} = exported(connection(), interfaces: [@calculator, @display])

      assert_refused(app_call("Reset", interface: nil), state, @unknown_method_error)
      assert_refused(app_call("Nope", interface: nil), state, @unknown_method_error)
    end

    test "refuses a call without an interface for a Peer member several interfaces declare" do
      pinger = Interface.new!("org.example.Pinger", methods: [{"Ping", []}])
      other = Interface.new!("org.example.OtherPinger", methods: [{"Ping", []}])
      {_export_ref, state} = exported(connection(), interfaces: [pinger, other])

      assert_refused(app_call("Ping", interface: nil), state, @unknown_method_error)
    end

    test "answers Peer members without an interface when nothing exported declares them" do
      {_export_ref, state} = exported(connection())

      for path <- [@path, "/org", "/elsewhere"] do
        call = app_call("Ping", path: path, interface: nil)
        assert [%{msg: %Message{type: :method_return}}] = queued_replies(call, state)
      end
    end

    test "prefers an exported member to Peer for a call without an interface" do
      pinger = Interface.new!("org.example.Pinger", methods: [{"Ping", []}])
      {export_ref, state} = exported(connection(), interfaces: [pinger])

      assert {:continue, :recv, %Connection{} = state} =
               dispatch(app_call("Ping", interface: nil), state)

      assert_received {^export_ref, %Call{interface: "org.example.Pinger"}}
      assert :queue.is_empty(state.writer.queue)
      InboundCalls.close_all(state.inbound_calls)
    end

    test "answers Peer on an exported path" do
      {export_ref, state} = exported(connection())

      assert [%{msg: %Message{type: :method_return}}] =
               queued_replies(app_call("Ping", interface: "org.freedesktop.DBus.Peer"), state)

      refute_received {^export_ref, _call}
    end

    test "routes calls below a subtree export with the path called" do
      {export_ref, state} = exported(connection(), path: "/org/example", subtree?: true)
      call = app_call("Add", path: "/org/example/a/b", signature: "ii", body: [2, 3])

      assert {:continue, :recv, %Connection{} = state} = dispatch(call, state)

      assert_received {^export_ref, %Call{path: "/org/example/a/b"}}
      InboundCalls.close_all(state.inbound_calls)
    end

    test "refuses a call beyond max_pending_calls with LimitsExceeded" do
      {export_ref, state} = exported(%{connection() | max_pending_calls: 1})
      call = app_call("Add", signature: "ii", body: [2, 3])

      assert {:continue, :recv, %Connection{} = state} = dispatch(call, state)
      assert_received {^export_ref, %Call{}}

      assert [%{msg: reply}] = queued_replies(%{call | serial: 6}, state)
      assert reply.header_fields.error_name == @limits_exceeded_error
      assert reply.header_fields.reply_serial == 6
      refute_received {^export_ref, _call}

      InboundCalls.close_all(state.inbound_calls)
    end

    test "never answers a call that asked for no reply, on any path" do
      {export_ref, state} = exported(%{connection() | max_pending_calls: 1})
      {:continue, :recv, state} = dispatch(app_call("Add", signature: "ii", body: [2, 3]), state)
      assert_received {^export_ref, %Call{}}

      for call <- [
            app_call("Add", path: "/nowhere"),
            app_call("Add", path: "/org"),
            app_call("Add", interface: "org.example.Missing"),
            app_call("Nope"),
            app_call("Add", signature: "i", body: [2]),
            # Over the limit, but untracked, so delivered below instead.
            app_call("Ping", interface: "org.freedesktop.DBus.Peer")
          ] do
        call = %{call | flags: [:no_reply_expected]}
        assert queued_replies(call, state) == []
      end

      refute_received :advance_writes
      InboundCalls.close_all(state.inbound_calls)
    end

    test "delivers a call that asked for no reply without recording it, even at the limit" do
      {export_ref, state} = exported(%{connection() | max_pending_calls: 1})
      {:continue, :recv, state} = dispatch(app_call("Add", signature: "ii", body: [2, 3]), state)
      assert_received {^export_ref, %Call{}}

      call = %{app_call("Add", signature: "ii", body: [4, 5]) | flags: [:no_reply_expected]}
      assert {:continue, :recv, %Connection{} = after_call} = dispatch(call, state)

      assert_received {^export_ref,
                       %Call{no_reply?: true, deadline: nil, flags: [:no_reply_expected]}}

      assert after_call.inbound_calls == state.inbound_calls
      assert :queue.is_empty(after_call.writer.queue)
      InboundCalls.close_all(state.inbound_calls)
    end

    test "records no deadline for an export that never expires" do
      {export_ref, state} = exported(connection(), call_timeout: :infinity)

      {:continue, :recv, state} = dispatch(app_call("Add", signature: "ii", body: [2, 3]), state)

      assert_received {^export_ref, %Call{deadline: nil, ref: ref}}
      assert {:ok, %{timer_ref: nil}} = InboundCalls.fetch(state.inbound_calls, ref)
    end
  end

  describe "Introspect" do
    test "answers on an exported object with its interfaces, the standard ones and children" do
      {_export_ref, state} = exported(connection(), interfaces: [@display, @calculator])
      {_export_ref, state} = exported(state, path: @path <> "/History")

      assert %{interfaces: interfaces, children: ["History"]} = introspected(@path, state)

      assert interfaces == [
               "org.example.Calculator",
               "org.example.Display",
               "org.freedesktop.DBus.Peer",
               "org.freedesktop.DBus.Introspectable",
               "org.freedesktop.DBus.Properties"
             ]
    end

    test "answers on every intermediate node with Introspectable and its children" do
      {_export_ref, state} = exported(connection())
      {_export_ref, state} = exported(state, path: "/org/other", subtree?: true)

      introspectable = ["org.freedesktop.DBus.Introspectable"]
      assert %{interfaces: ^introspectable, children: ["org"]} = introspected("/", state)

      assert %{interfaces: ^introspectable, children: ["example", "other"]} =
               introspected("/org", state)

      assert %{interfaces: ^introspectable, children: ["Calc"]} =
               introspected("/org/example", state)
    end

    test "answers on a subtree with its interfaces and no children" do
      {_export_ref, state} = exported(connection(), path: "/org/example", subtree?: true)
      {_export_ref, state} = exported(state, path: "/org/example/a/b")

      for path <- ["/org/example", "/org/example/a"] do
        assert %{interfaces: ["org.example.Calculator" | _standard], children: []} =
                 introspected(path, state)
      end
    end

    test "answers the reply to the caller with the call's serial" do
      {_export_ref, state} = exported(connection())

      assert [%{kind: :reply, msg: reply}] = queued_replies(introspect_call("/"), state)
      assert %Message{type: :method_return, body: [xml]} = reply
      assert reply.header_fields.reply_serial == 5
      assert reply.header_fields.destination == ":1.7"
      assert Message.signature(reply) == "s"
      assert xml =~ ~r/\A<!DOCTYPE node /
    end

    test "refuses a path with nothing exported at it or below it with UnknownObject" do
      assert_refused(introspect_call("/"), connection(), @unknown_object_error)

      {_export_ref, state} = exported(connection())
      assert_refused(introspect_call("/elsewhere"), state, @unknown_object_error)
      assert_refused(introspect_call(@path <> "/below"), state, @unknown_object_error)
    end

    test "refuses arguments with InvalidArgs" do
      {_export_ref, state} = exported(connection())

      for path <- [@path, "/org"] do
        call =
          app_call("Introspect",
            path: path,
            interface: @introspectable,
            signature: "s",
            body: ["x"]
          )

        assert_refused(call, state, @invalid_args_error)
      end
    end

    test "refuses another Introspectable member on an exported object with UnknownMethod" do
      {_export_ref, state} = exported(connection())

      assert_refused(
        app_call("Nope", interface: @introspectable),
        state,
        @unknown_method_error
      )
    end

    test "never answers a call that asked for no reply" do
      {_export_ref, state} = exported(connection())
      call = %{introspect_call(@path) | flags: [:no_reply_expected]}

      assert queued_replies(call, state) == []
    end

    test "caches each document until the export table changes" do
      {export_ref, state} = exported(connection())

      {:continue, :recv, introspected} = dispatch(introspect_call("/org"), state)
      assert Map.keys(introspected.objects.introspection) == [{:path, "/org"}]

      # A connection with nothing queued yet, and the cache just filled.
      state = %{state | objects: introspected.objects}
      {_ref, state} = exported(state, path: "/org/other")
      assert state.objects.introspection == %{}
      assert %{children: ["example", "other"]} = introspected("/org", state)

      state = Service.unexport(state, export_ref)
      assert %{children: ["other"]} = introspected("/org", state)
    end
  end

  describe "Properties" do
    test "routes each call to the handler that exported the target interface" do
      {_calc_ref, state} = exported(connection())
      handler = forwarder()
      {settings_ref, state} = exported(state, interfaces: [@settings], handler: handler)

      for {member, signature, body, out_signature} <- [
            {"Get", "ss", ["org.example.Settings", "Version"], "v"},
            {"Set", "ssv", ["org.example.Settings", "Precision", {"u", 3}], ""},
            {"GetAll", "s", ["org.example.Settings"], "a{sv}"}
          ] do
        call = properties_call(member, signature: signature, body: body)
        assert {:continue, :recv, %Connection{} = routed} = dispatch(call, state)

        assert_receive {:forwarded, {^settings_ref, %Call{} = delivered}}, 1_000
        assert delivered.export == settings_ref
        assert delivered.interface == @properties
        assert delivered.member == member
        assert delivered.signature == signature
        assert delivered.out_signature == out_signature
        assert delivered.body == body

        assert {:ok, entry} = InboundCalls.fetch(routed.inbound_calls, delivered.ref)
        assert entry.handler == handler
        assert :queue.is_empty(routed.writer.queue)
        InboundCalls.close_all(routed.inbound_calls)
      end

      refute_received {_export_ref, %Call{}}
    end

    test "answers GetAll itself with an empty dictionary when nothing is readable" do
      write_only = Interface.new!("org.example.Sink", properties: [{"Level", "u", :write}])
      {export_ref, state} = exported(connection(), interfaces: [@calculator, write_only])

      for target <- ["org.example.Calculator", "org.example.Sink"] do
        call = properties_call("GetAll", signature: "s", body: [target])

        assert [%{kind: :reply, msg: reply}] = queued_replies(call, state)
        assert %Message{type: :method_return, body: [[]]} = reply
        assert Message.signature(reply) == "a{sv}"
        assert reply.header_fields.reply_serial == 5
        assert reply.header_fields.destination == ":1.7"
      end

      refute_received {^export_ref, _call}
    end

    test "refuses a member Properties does not declare with UnknownMethod" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings])

      call = properties_call("Nope", signature: "s", body: ["org.example.Settings"])
      assert_refused(call, state, @unknown_method_error)
    end

    test "refuses arguments that differ from the Properties signatures with InvalidArgs" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings])

      for {member, signature, body} <- [
            {"Get", "s", ["org.example.Settings"]},
            {"Get", "ssv", ["org.example.Settings", "Version", {"s", "x"}]},
            {"Set", "ss", ["org.example.Settings", "Precision"]},
            {"Set", "ssu", ["org.example.Settings", "Precision", 3]},
            {"GetAll", "", []},
            {"GetAll", "ss", ["org.example.Settings", "Version"]}
          ] do
        call = properties_call(member, signature: signature, body: body)
        assert_refused(call, state, @invalid_args_error)
      end
    end

    test "refuses an interface the object does not export with UnknownInterface" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings])

      for target <- ["org.example.Missing", "", @properties, @introspectable] do
        for {member, signature, body} <- [
              {"Get", "ss", [target, "Version"]},
              {"Set", "ssv", [target, "Precision", {"u", 3}]},
              {"GetAll", "s", [target]}
            ] do
          call = properties_call(member, signature: signature, body: body)
          assert_refused(call, state, @unknown_interface_error)
        end
      end
    end

    test "refuses a property the interface does not declare with UnknownProperty" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings, @calculator])

      for {member, signature, body} <- [
            {"Get", "ss", ["org.example.Settings", "Missing"]},
            {"Set", "ssv", ["org.example.Settings", "Missing", {"u", 3}]},
            {"Get", "ss", ["org.example.Calculator", "Precision"]}
          ] do
        call = properties_call(member, signature: signature, body: body)
        assert_refused(call, state, @unknown_property_error)
      end
    end

    test "refuses reading a write-only or writing a read-only property with PropertyReadOnly" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings])

      for {member, signature, body} <- [
            {"Get", "ss", ["org.example.Settings", "Secret"]},
            {"Set", "ssv", ["org.example.Settings", "Version", {"s", "2.0"}]}
          ] do
        call = properties_call(member, signature: signature, body: body)
        assert_refused(call, state, @property_read_only_error)
      end
    end

    test "refuses a Set whose value differs from the declared type with InvalidArgs" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings])

      for value <- [{"i", 3}, {"s", "3"}, {"au", [3]}, {"v", {"u", 3}}] do
        call =
          properties_call("Set",
            signature: "ssv",
            body: ["org.example.Settings", "Precision", value]
          )

        assert [%{msg: reply}] = queued_replies(call, state)
        assert reply.header_fields.error_name == @invalid_args_error
        assert reply.body == ["Value does not match the property signature"]
      end
    end

    test "refuses Properties on an intermediate node and on a path with nothing exported" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings])
      body = ["org.example.Settings"]

      assert_refused(
        properties_call("GetAll", path: "/org", signature: "s", body: body),
        state,
        @unknown_interface_error
      )

      assert_refused(
        properties_call("GetAll", path: "/elsewhere", signature: "s", body: body),
        state,
        @unknown_object_error
      )
    end

    test "never answers a Properties call that asked for no reply" do
      {_export_ref, state} = exported(connection(), interfaces: [@settings, @calculator])

      for {member, signature, body} <- [
            {"GetAll", "s", ["org.example.Calculator"]},
            {"Get", "ss", ["org.example.Settings", "Secret"]},
            {"Get", "ss", ["org.example.Missing", "Version"]},
            {"Set", "ssv", ["org.example.Settings", "Precision", {"s", "3"}]}
          ] do
        call = %{
          properties_call(member, signature: signature, body: body)
          | flags: [:no_reply_expected]
        }

        assert queued_replies(call, state) == []
      end

      refute_received :advance_writes
    end
  end

  describe "completing inbound calls" do
    test "addresses the reply from the recorded call, not from the handler" do
      {_call, state} = routed()
      [ref] = Map.keys(state.inbound_calls.entries)

      forged =
        Message.new!(:method_return,
          reply_serial: 99,
          destination: ":1.666",
          signature: "i",
          body: [5]
        )

      assert {:ok, reply, state} = Service.complete(state, ref, forged)
      assert reply.header_fields.reply_serial == 5
      assert reply.header_fields.destination == ":1.7"
      assert state.inbound_calls == InboundCalls.new()
    end

    test "sends a peer-to-peer reply without a destination" do
      {export_ref, state} = exported(connection())
      call = app_call("Add", sender: nil, signature: "ii", body: [2, 3])
      {:continue, :recv, state} = dispatch(call, state)
      assert_received {^export_ref, %Call{ref: ref, sender: nil}}

      forged = Message.new!(:method_return, reply_serial: 99, destination: ":1.666")

      assert {:ok, reply, _state} = Service.complete(state, ref, forged)
      refute Map.has_key?(reply.header_fields, :destination)
    end

    test "answers a second reply with unknown_call" do
      {_call, state} = routed()
      [ref] = Map.keys(state.inbound_calls.entries)
      reply = Message.new!(:method_return, reply_serial: 5, signature: "i", body: [5])

      {:ok, _reply, state} = Service.complete(state, ref, reply)

      assert {:error, :unknown_call} = Service.complete(state, ref, reply)
    end

    test "answers an expired call with NoReply and forgets it" do
      {%Call{ref: ref}, state} = routed(call_timeout: 10)
      assert_receive {:inbound_call_timeout, ^ref}, 1_000

      assert {:noreply, state} = Connection.handle_info({:inbound_call_timeout, ref}, state)

      assert [%{kind: :reply, msg: reply}] = :queue.to_list(state.writer.queue)
      assert reply.header_fields.error_name == @no_reply_error
      assert reply.header_fields.reply_serial == 5
      assert reply.header_fields.destination == ":1.7"
      assert_received :advance_writes

      reply = Message.new!(:method_return, signature: "i", body: [5], reply_serial: 5)
      assert {:error, :unknown_call} = Service.complete(state, ref, reply)
    end

    test "ignores an expiry timer that fires after its call completed" do
      {%Call{ref: ref}, state} = routed(call_timeout: 10)
      reply = Message.new!(:method_return, signature: "i", body: [5], reply_serial: 5)
      {:ok, _reply, state} = Service.complete(state, ref, reply)

      assert {:noreply, ^state} = Connection.handle_info({:inbound_call_timeout, ref}, state)
    end

    test "fails pending calls with Failed and unexports when the handler exits" do
      handler = spawn(fn -> Process.sleep(:infinity) end)
      {export_ref, state} = exported(connection(), handler: handler)
      {:continue, :recv, state} = dispatch(app_call("Add", signature: "ii", body: [2, 3]), state)

      Process.exit(handler, :kill)
      {:monitors, [{:process, ^handler}]} = Process.info(self(), :monitors)
      monitor_ref = Map.fetch!(state.objects.exports, export_ref).monitor_ref

      assert {:noreply, state} =
               Connection.handle_info({:DOWN, monitor_ref, :process, handler, :killed}, state)

      assert [%{msg: reply}] = :queue.to_list(state.writer.queue)
      assert reply.header_fields.error_name == @failed_error
      assert reply.body == ["Handler exited"]
      assert reply.header_fields.reply_serial == 5
      assert state.inbound_calls == InboundCalls.new()
      assert state.objects == Objects.new()
    end

    test "fails pending calls with Failed when their object is unexported" do
      {export_ref, state} = exported(connection())
      {:continue, :recv, state} = dispatch(app_call("Add", signature: "ii", body: [2, 3]), state)

      state = Service.unexport(state, export_ref)

      assert [%{msg: reply}] = :queue.to_list(state.writer.queue)
      assert reply.header_fields.error_name == @failed_error
      assert reply.body == ["Object unexported"]
      assert state.inbound_calls == InboundCalls.new()
      assert {:monitors, []} = Process.info(self(), :monitors)
    end
  end

  describe "receive_result/2" do
    test "parks on a select notification without touching the buffer" do
      handle = make_ref()

      assert {:ok, %Connection{rref: ^handle, inbound: %Inbound{size: 0}}} =
               Dispatch.receive_result(
                 {:select, {:select_info, :recv, handle}},
                 connection()
               )
    end

    test "stops the connection when the transport fails" do
      assert {:transport_error, :closed, %Connection{}} =
               Dispatch.receive_result({:error, :closed}, connection())
    end

    test "stops the connection on an unrecognised receive result" do
      assert {:transport_error, :receive_failed, %Connection{}} =
               Dispatch.receive_result(:nonsense, connection())
    end
  end

  # An established connection whose socket is a `ScriptedTransport` agent. No
  # test here writes to it: queued replies are asserted on the writer's queue.
  defp connection do
    %Connection{
      sock: ScriptedTransport.start([]),
      impl: Rebus.Impl.build(transport: ScriptedTransport),
      established?: true,
      machine_id: @machine_id
    }
  end

  defp dispatch(%Message{} = msg, %Connection{} = state) do
    {:ok, encoded} = Message.encode(serialised(msg))

    Dispatch.process_inbound(
      %{state | inbound: Inbound.new(IO.iodata_to_binary(encoded))},
      :recv
    )
  end

  defp serialised(%Message{serial: serial} = msg) when is_integer(serial) and serial > 0, do: msg
  defp serialised(%Message{} = msg), do: %{msg | serial: 7}

  defp queued_replies(%Message{} = call, state \\ nil) do
    state = state || connection()

    assert {:continue, :recv, %Connection{writer: %Writer{} = writer}} = dispatch(call, state)

    :queue.to_list(writer.queue)
  end

  defp peer_call(member) do
    %{
      Message.new!(:method_call,
        path: "/",
        interface: "org.freedesktop.DBus.Peer",
        member: member,
        sender: ":1.7"
      )
      | serial: 5
    }
  end

  # Exports `@calculator` at `@path` to this process, or what `opts` says.
  defp exported(%Connection{} = state, opts \\ []) do
    ref = make_ref()

    {{:ok, ^ref}, state} =
      Service.export(state, %{
        ref: ref,
        path: Keyword.get(opts, :path, @path),
        subtree?: Keyword.get(opts, :subtree?, false),
        interfaces: Keyword.get(opts, :interfaces, [@calculator]),
        handler: Keyword.get(opts, :handler, self()),
        call_timeout: Keyword.get(opts, :call_timeout, 60_000)
      })

    {ref, state}
  end

  # A call to `Add` routed to this process and recorded.
  defp routed(opts \\ []) do
    {export_ref, state} = exported(connection(), opts)
    {:continue, :recv, state} = dispatch(app_call("Add", signature: "ii", body: [2, 3]), state)
    assert_received {^export_ref, %Call{} = call}
    {call, state}
  end

  defp app_call(member, opts \\ []) do
    fields =
      [
        path: Keyword.get(opts, :path, @path),
        interface: Keyword.get(opts, :interface, "org.example.Calculator"),
        member: member,
        sender: Keyword.get(opts, :sender, ":1.7"),
        signature: Keyword.get(opts, :signature, ""),
        body: Keyword.get(opts, :body, [])
      ]
      |> Enum.reject(fn {_key, value} -> is_nil(value) end)

    %{Message.new!(:method_call, fields) | serial: 5}
  end

  defp introspect_call(path), do: app_call("Introspect", path: path, interface: @introspectable)

  defp properties_call(member, opts), do: app_call(member, [interface: @properties] ++ opts)

  # A handler other than this process, which hands every message it receives
  # back here, so a test can tell which handler a call was routed to.
  defp forwarder do
    test = self()
    spawn_link(fn -> forward(test) end)
  end

  defp forward(test) do
    receive do
      message -> send(test, {:forwarded, message})
    end

    forward(test)
  end

  # The interface names and children of the document `Introspect` answers on
  # `path`, after checking that it parses.
  defp introspected(path, %Connection{} = state) do
    assert [%{msg: %Message{type: :method_return, body: [xml]}}] =
             queued_replies(introspect_call(path), state)

    {document, []} = xml |> :binary.bin_to_list() |> :xmerl_scan.string(quiet: true)

    %{
      interfaces: attribute_values(document, "/node/interface/@name"),
      children: attribute_values(document, "/node/node/@name")
    }
  end

  defp attribute_values(document, xpath) do
    xpath
    |> String.to_charlist()
    |> :xmerl_xpath.string(document)
    |> Enum.map(&(&1 |> xml_attribute(:value) |> :unicode.characters_to_binary()))
  end

  defp assert_refused(%Message{} = call, %Connection{} = state, error_name) do
    assert [%{kind: :reply, msg: reply}] = queued_replies(call, state)
    assert %Message{type: :error, body: [_detail]} = reply
    assert reply.header_fields.error_name == error_name
    assert reply.header_fields.reply_serial == call.serial
    refute_received {_export_ref, %Call{}}
  end

  # A pending-reply entry answered by this process: `GenServer.reply/2` on a
  # `{pid, ref}` sends `{ref, reply}`, so the assertions read the mailbox.
  defp caller do
    tag = make_ref()

    %{
      tag: tag,
      from: {self(), tag},
      timer_ref: Process.send_after(self(), :unused_request_timer, 60_000),
      request_ref: make_ref(),
      monitor_ref: Process.monitor(self()),
      deadline: System.monotonic_time(:millisecond) + 60_000
    }
  end

  defp pending(%Connection{} = state, serial, entry) do
    %{
      state
      | pending:
          Pending.put(state.pending, %Pending.Entry{
            serial: serial,
            from: entry.from,
            timer_ref: entry.timer_ref,
            request_ref: entry.request_ref,
            monitor_ref: entry.monitor_ref,
            deadline: entry.deadline
          })
    }
  end

  defp handler(rule),
    do: %{pid: self(), monitor_ref: Process.monitor(self()), rule: rule}

  # A descriptor this test owns outright, so the close-or-deliver path under
  # test never closes a number another socket still holds. `{:otp, :fd}` on a
  # socket the test keeps would do exactly that; see `Rebus.TestFD`.
  defp descriptor, do: TestFD.dup!()

  # A well-formed `method_return` frame whose body exceeds a local scalar cap,
  # so parsing yields the reply envelope and `{:error, :resource_limit, ...}`.
  defp resource_limited_reply(reply_serial) do
    sentinel = "resource-limit-body-sentinel"

    body =
      <<1_000_001::little-32, sentinel::binary>> <>
        :binary.copy(<<1>>, 1_000_001 - byte_size(sentinel))

    fields = [[5, {"u", reply_serial}], [8, {"g", "ay"}]]

    header =
      "a(yv)"
      |> Rebus.Encoder.encode_at_position([fields], :little, 12)
      |> IO.iodata_to_binary()

    padding = :binary.copy(<<0>>, rem(8 - rem(12 + byte_size(header), 8), 8))

    <<?l, 2, 0, 1, byte_size(body)::little-32, 1::little-32, header::binary, padding::binary,
      body::binary>>
  end
end
