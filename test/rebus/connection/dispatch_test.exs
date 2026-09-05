defmodule Rebus.Connection.DispatchTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog, only: [capture_log: 1]

  alias Rebus.Connection
  alias Rebus.Connection.Dispatch
  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Writer
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.ScriptedTransport
  alias Rebus.TestFD

  @machine_id "0123456789abcdef0123456789abcdef"
  @unknown_method_error "org.freedesktop.DBus.Error.UnknownMethod"
  @failed_error "org.freedesktop.DBus.Error.Failed"

  describe "reply correlation" do
    test "answers the caller of a correlated reply and clears its entry" do
      %{tag: tag, request_ref: request_ref, monitor_ref: monitor_ref} = entry = caller()
      state = pending(connection(), 11, entry)

      reply = Message.new!(:method_return, reply_serial: 11, signature: "s", body: ["ok"])

      assert {:continue, :recv, %Connection{} = state} = dispatch(reply, state)
      assert state.pending == %{}
      refute Map.has_key?(state.request_index, request_ref)
      refute Map.has_key?(state.monitor_index, monitor_ref)
      assert_received {^tag, %Message{type: :method_return, body: ["ok"]}}
    end

    test "logs and drops a reply that correlates to nothing" do
      reply = Message.new!(:method_return, reply_serial: 9)

      log =
        capture_log(fn ->
          assert {:continue, :recv, %Connection{pending: %{}}} = dispatch(reply, connection())
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
      assert state.pending == %{}
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
          assert {:continue, :recv, %Connection{pending: %{}}} =
                   Dispatch.process_inbound(
                     %{state | inbound: Inbound.new(resource_limited_reply(11))},
                     :recv
                   )
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

      assert {:ok, %Connection{pending: %{}, request_index: index}} =
               Dispatch.request_timeout(11, request_ref, state)

      assert index == %{}
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

    test "refuses any other method with UnknownMethod" do
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
          Map.put(
            state.pending,
            serial,
            {entry.from, entry.timer_ref, entry.request_ref, entry.monitor_ref, entry.deadline}
          ),
        request_index: Map.put(state.request_index, entry.request_ref, serial),
        monitor_index: Map.put(state.monitor_index, entry.monitor_ref, serial)
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
