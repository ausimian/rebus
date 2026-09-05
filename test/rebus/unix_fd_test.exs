defmodule Rebus.UnixFDTest do
  use ExUnit.Case, async: false

  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Inbound
  alias Rebus.{Message, TestImpl, TestServer, UnixFD}

  @connection_name :rebus_unix_fd_test_connection

  @moduletag skip:
               if(
                 :os.type() in [{:unix, :linux}, {:unix, :darwin}],
                 do: false,
                 else: "SCM_RIGHTS coverage is supported on Linux and macOS"
               )

  setup do
    path = Path.join("/tmp", "rebus-unix-fd-#{System.system_time(:nanosecond)}")

    {:ok, server} =
      start_supervised(%{
        id: {:unix_fd_server, path},
        start: {TestServer, :start_link, [[tap: self(), family: :local, path: path]]}
      })

    {:ok, address} = TestServer.get_listen_addr(server)

    # No test in this module drives the identity lookup, so the connection
    # replays a cached `id -u` instead of spawning a port of its own.
    {:ok, connection} =
      Rebus.connect(address,
        name: @connection_name,
        __impl__: %{identity: TestImpl.CachedIdentity}
      )

    on_exit(fn ->
      if Process.alive?(connection), do: Rebus.close(connection)
    end)

    %{server: server, connection: connection, connection_name: @connection_name}
  end

  test "keeps ancillary descriptors separate from h indexes" do
    assert {:error, :invalid_unix_fds} =
             Message.new(:signal,
               path: "/test",
               interface: "test.interface",
               member: "FD",
               signature: "h",
               body: [0]
             )

    assert {:ok, message} =
             Message.new(:signal,
               path: "/test",
               interface: "test.interface",
               member: "FD",
               signature: "hh",
               body: [0, 0],
               fds: [0]
             )

    assert message.header_fields.unix_fds == 1
    assert message.unix_fds == [0]
    assert {:error, :invalid_unix_fds} = Message.attach_unix_fds(message, [])
  end

  test "reports invalid raw descriptors without closing an unrelated descriptor" do
    assert {:error, :invalid_descriptor} = UnixFD.close(-1)
    assert {:error, :invalid_descriptor} = UnixFD.close(:not_a_descriptor)
    assert {:error, :ebadf} = UnixFD.close(1_000_000)
    assert :ok = UnixFD.close_all([1_000_000, -1])
  end

  test "sends one descriptor for reused h indexes", %{server: server, connection: connection} do
    {:ok, fd} = :socket.getopt(:sys.get_state(connection).sock, {:otp, :fd})

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "FD",
        signature: "hh",
        body: [0, 0],
        fds: [fd]
      )

    assert :ok = Rebus.send(connection, message)

    assert_receive {^server, %Message{header_fields: %{member: "FD"}, unix_fds: [received]}},
                   1_000

    assert received != fd
    assert :ok = UnixFD.close(received)
  end

  test "sends rights once and switches a partial sendmsg tail to send", %{connection: connection} do
    parent = self()
    continuation = {:select_info, :sendmsg, make_ref()}
    {:select_info, :sendmsg, _handle} = continuation
    calls = :atomics.new(1, [])
    {:ok, fd} = :socket.getopt(:sys.get_state(connection).sock, {:otp, :fd})

    :ok =
      TestImpl.install(connection,
        sendmsg: fn
          _sock, %{iov: [rest], ctrl: ctrl}, [], :nowait ->
            tail = binary_part(rest, 1, byte_size(rest) - 1)
            send(parent, {:sendmsg_initial, rest, ctrl})
            :atomics.add_get(calls, 1, 1)
            {:select, {continuation, [tail]}}
        end,
        send: fn _sock, rest, [], :nowait ->
          send(parent, {:send_tail, rest})
          :atomics.add_get(calls, 1, 1)
          :ok
        end,
        cancel: fn _sock, ^continuation ->
          send(parent, :sendmsg_continuation_cancelled)
          :ok
        end
      )

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "PartialFD",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    task = Task.async(fn -> Rebus.send(connection, message) end)

    assert_receive {:sendmsg_initial, initial, [%{level: :socket, type: :rights, data: rights}]},
                   1_000

    assert <<^fd::native-signed-32>> = rights
    assert_receive :sendmsg_continuation_cancelled, 1_000
    assert_receive {:send_tail, continuation_rest}, 1_000
    assert continuation_rest != initial
    assert 2 = :atomics.get(calls, 1)
    assert :ok = Task.await(task, 1_000)
  end

  test "uses a sendmsg continuation only when a select accepted no bytes", %{
    connection: connection
  } do
    parent = self()
    continuation = {:select_info, :sendmsg, make_ref()}
    {:select_info, :sendmsg, handle} = continuation
    {:ok, fd} = :socket.getopt(:sys.get_state(connection).sock, {:otp, :fd})

    :ok =
      TestImpl.install(connection,
        sendmsg: fn
          _sock, %{iov: [rest], ctrl: ctrl}, [], :nowait ->
            send(parent, {:sendmsg_initial_no_progress, rest, ctrl})
            {:select, continuation}

          _sock, [rest], ^continuation, :nowait ->
            send(parent, {:sendmsg_no_progress_continuation, rest})
            :ok
        end
      )

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "NoProgressFD",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    task = Task.async(fn -> Rebus.send(connection, message) end)
    assert_receive {:sendmsg_initial_no_progress, initial, [_rights]}, 1_000
    send(connection, {:"$socket", :sys.get_state(connection).sock, :select, handle})
    assert_receive {:sendmsg_no_progress_continuation, ^initial}, 1_000
    assert :ok = Task.await(task, 1_000)
  end

  test "keeps accepted FD control sticky across a plain-tail select", %{connection: connection} do
    parent = self()
    send_continuation = {:select_info, :send, make_ref()}
    {:select_info, :send, handle} = send_continuation
    {:ok, fd} = :socket.getopt(:sys.get_state(connection).sock, {:otp, :fd})

    :ok =
      TestImpl.install(connection,
        sendmsg: fn _sock, %{iov: [rest], ctrl: ctrl}, [], :nowait ->
          tail = binary_part(rest, 1, byte_size(rest) - 1)
          send(parent, {:sticky_sendmsg_initial, rest, ctrl})
          {:ok, tail}
        end,
        send: fn
          _sock, rest, [], :nowait ->
            send(parent, {:sticky_plain_tail, rest})
            {:select, send_continuation}

          _sock, rest, ^send_continuation, :nowait ->
            send(parent, {:sticky_plain_continuation, rest})
            :ok
        end
      )

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "StickyFD",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    task = Task.async(fn -> Rebus.send(connection, message) end)

    assert_receive {:sticky_sendmsg_initial, initial, [%{type: :rights, data: rights}]}, 1_000
    assert <<^fd::native-signed-32>> = rights
    assert_receive {:sticky_plain_tail, tail}, 1_000
    assert tail == binary_part(initial, 1, byte_size(initial) - 1)

    send(connection, {:"$socket", :sys.get_state(connection).sock, :select, handle})
    assert_receive {:sticky_plain_continuation, ^tail}, 1_000
    assert :ok = Task.await(task, 1_000)
  end

  test "delivers a received descriptor only with a live method reply", %{
    server: server,
    connection: connection
  } do
    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "ReceiveFD"
      )

    task = Task.async(fn -> Rebus.call(connection, method, 1_000) end)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "ReceiveFD"}}},
                   1_000

    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert {:ok, %Message{type: :method_return, unix_fds: [received]}} = Task.await(task, 1_000)
    assert received != fd
    assert :ok = UnixFD.close(received)
  end

  test "delivers a received descriptor with an FD-bearing D-Bus error reply", %{
    server: server,
    connection: connection
  } do
    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "ReceiveErrorFD"
      )

    task = Task.async(fn -> Rebus.call(connection, method, 1_000) end)

    assert_receive {^server,
                    %Message{serial: serial, header_fields: %{member: "ReceiveErrorFD"}}},
                   1_000

    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    reply =
      Message.new!(:error,
        error_name: "org.example.FailedWithFD",
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])

    assert {:error,
            %Message{
              type: :error,
              header_fields: %{error_name: "org.example.FailedWithFD"},
              unix_fds: [received]
            }} = Task.await(task, 1_000)

    assert received != fd
    assert :ok = UnixFD.close(received)
  end

  test "closes an FD reply whose public call alias timed out while its PID lived", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    TestImpl.install(connection,
      fd_claim_handoff: fn ->
        send(parent, :fd_claim_handoff_waiting)

        receive do
          :continue_fd_claim_handoff -> :ok
        end
      end,
      request_timeout_slack: fn -> 1_000 end
    )

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "TimedOutAliasFD"
      )

    caller =
      spawn(fn ->
        result = Rebus.call(connection, method, 500)
        send(parent, {:timed_out_alias_result, result})

        receive do
          :stop_timed_out_alias_caller -> :ok
        end
      end)

    assert_receive {^server,
                    %Message{serial: serial, header_fields: %{member: "TimedOutAliasFD"}}},
                   1_000

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert_receive :fd_claim_handoff_waiting, 1_000
    assert_receive {:timed_out_alias_result, {:error, :timeout}}, 1_000
    assert Process.alive?(caller)

    # The task PID is still alive, but its timed-out GenServer.call alias is
    # gone. Releasing this hook exercises the exact late-reply race.
    send(connection, :continue_fd_claim_handoff)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
    assert Process.alive?(connection)
    send(caller, :stop_timed_out_alias_caller)
  end

  test "unaliases a delayed FD delivery before it can reach the caller mailbox", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    TestImpl.install(connection,
      fd_claim_delivery: fn ->
        send(parent, :fd_delivery_waiting)

        receive do
          :continue_fd_delivery -> :ok
        end
      end,
      # The caller's alias must time out while the connection still holds the
      # request, so the reply has to survive the socket round trip on a loaded
      # runner. Keep the connection-side reaper well behind the caller
      # deadline, as the timed-out alias test above does.
      request_timeout_slack: fn -> 1_000 end
    )

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "DelayedFDDelivery"
      )

    caller =
      spawn(fn ->
        result = Rebus.call(connection, method, 500)
        send(parent, {:delayed_fd_result, result})

        receive do
          :stop_delayed_fd_caller -> :ok
          message -> send(parent, {:unexpected_delayed_fd_message, message})
        end
      end)

    assert_receive {^server,
                    %Message{serial: serial, header_fields: %{member: "DelayedFDDelivery"}}},
                   1_000

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert_receive :fd_delivery_waiting, 1_000
    assert_receive {:delayed_fd_result, {:error, :timeout}}, 2_000

    send(connection, :continue_fd_delivery)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
    refute_receive {:unexpected_delayed_fd_message, {:rebus_fd_reply, _, _, _}}, 100
    assert Process.alive?(connection)
    send(caller, :stop_delayed_fd_caller)
  end

  test "closes a stalled FD acknowledgement after its claim deadline", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    TestImpl.install(connection,
      fd_claim_ack: fn _claim ->
        send(parent, :fd_ack_waiting)

        receive do
          :continue_fd_ack -> :ok
        end
      end
    )

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "DelayedFDAck"
      )

    # The reply has to reach the connection inside the caller deadline, so the
    # budget must cover a socket round trip on a loaded runner. Every deadline
    # below scales with it; only the wall-clock margin changes.
    task = Task.async(fn -> Rebus.call(connection, method, 200) end)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "DelayedFDAck"}}},
                   1_000

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert_receive :fd_ack_waiting, 1_000

    # The first acknowledgement call expires at 300ms. Keep the handler
    # blocked beyond the 450ms claim cleanup deadline: the FIFO resolver must
    # still wait for a definitive close rather than return while this queued
    # acknowledgement could later transfer ownership.
    Process.sleep(550)
    send(connection, :continue_fd_ack)

    assert {:error, :fd_claim_expired} = Task.await(task, 1_000)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
    assert Process.alive?(connection)
  end

  test "closes a claimed FD when its caller dies before acknowledgement", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    TestImpl.install(connection,
      fd_claim_delivery: fn ->
        send(parent, :claimed_fd_delivery_waiting)

        receive do
          :continue_claimed_fd_delivery -> :ok
        end
      end
    )

    caller =
      spawn(fn ->
        message =
          Message.new!(:method_call,
            path: "/test",
            interface: "test.interface",
            member: "ClaimedCallerDown"
          )

        send(parent, {:claimed_caller_result, Rebus.call(connection, message, 1_000)})
      end)

    assert_receive {^server,
                    %Message{serial: serial, header_fields: %{member: "ClaimedCallerDown"}}},
                   1_000

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert_receive :claimed_fd_delivery_waiting, 1_000

    caller_ref = Process.monitor(caller)
    Process.exit(caller, :kill)
    assert_receive {:DOWN, ^caller_ref, :process, ^caller, :killed}, 1_000

    send(connection, :continue_claimed_fd_delivery)
    refute_receive {:claimed_caller_result, _result}, 50
    assert eventually(fn -> :sys.get_state(connection).fd_claims.claims == %{} end)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
    assert Process.alive?(connection)
  end

  test "Rebus.close closes partial inbound descriptors and unregisters its child", %{
    server: server,
    connection: connection,
    connection_name: connection_name
  } do
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    partial =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "PartialCloseFD",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    {:ok, encoded} = Message.encode(%{partial | serial: 41})
    encoded = IO.iodata_to_binary(encoded)
    prefix = binary_part(encoded, 0, min(8, byte_size(encoded)))

    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [prefix],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    assert eventually(fn -> :sys.get_state(connection).inbound_fds.fds != [] end)
    [received] = :sys.get_state(connection).inbound_fds.fds

    ref = Process.monitor(connection)
    assert :ok = Rebus.close(connection)
    assert_receive {:DOWN, ^ref, :process, ^connection, :shutdown}, 1_000
    # Check the exact received descriptor before any other operation can cause
    # the OS to reuse its integer value. This is stronger than a process-wide
    # table comparison, which changes by one OTP fixture descriptor on close.
    assert {:error, :ebadf} = UnixFD.close(received)
    assert Process.whereis(connection_name) == nil

    refute Enum.any?(DynamicSupervisor.which_children(Rebus.ConnectionSupervisor), fn
             {_id, ^connection, _type, _modules} -> true
             _child -> false
           end)
  end

  test "Rebus.close resolves an unacknowledged claim as disconnected", %{
    server: server,
    connection: connection,
    connection_name: connection_name
  } do
    parent = self()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    TestImpl.install(connection,
      fd_claim_ack: fn %{msg: %Message{unix_fds: [received]}} ->
        send(parent, {:close_claim_ack_waiting, received})

        receive do
          :continue_close_claim_ack -> :ok
        end
      end
    )

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "CloseClaimFD"
      )

    # The reply has to reach the connection inside the caller deadline, so the
    # budget must cover a socket round trip on a loaded runner. The release
    # sleep below scales with it; only the wall-clock margin changes.
    call_task = Task.async(fn -> Rebus.call(connection, method, 200) end)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "CloseClaimFD"}}},
                   1_000

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert_receive {:close_claim_ack_waiting, received}, 1_000

    ref = Process.monitor(connection)
    close_task = Task.async(fn -> Rebus.close(connection) end)
    assert nil == Task.yield(close_task, 20)

    # The shutdown signal enters the connection mailbox before the client can
    # enqueue an acknowledgement. Release after the 450ms claim deadline so an
    # ack cannot transfer ownership if scheduling changes at the boundary.
    Process.sleep(550)
    send(connection, :continue_close_claim_ack)

    assert :ok = Task.await(close_task, 1_000)
    assert_receive {:DOWN, ^ref, :process, ^connection, :shutdown}, 1_000
    assert {:error, :disconnected} = Task.await(call_task, 1_000)
    assert {:error, :ebadf} = UnixFD.close(received)
    assert Process.whereis(connection_name) == nil

    refute Enum.any?(DynamicSupervisor.which_children(Rebus.ConnectionSupervisor), fn
             {_id, ^connection, _type, _modules} -> true
             _child -> false
           end)
  end

  test "drops claimed reply ownership on expiry, caller DOWN, and cancellation", %{
    connection: connection
  } do
    state = :sys.get_state(connection)

    expired_ref = make_ref()
    expired = claimed_state(state, expired_ref, make_ref(), Process.monitor(self()))

    assert {:noreply, expired_state} =
             Rebus.Connection.handle_info({:fd_claim_timeout, expired_ref}, expired)

    assert expired_state.fd_claims.claims == %{}

    assert {:noreply, ^expired_state} =
             Rebus.Connection.handle_info({:fd_claim_timeout, make_ref()}, expired_state)

    down_ref = make_ref()
    down_monitor = Process.monitor(self())
    down_state = claimed_state(state, down_ref, make_ref(), down_monitor)

    assert {:noreply, down_state} =
             Rebus.Connection.handle_info(
               {:DOWN, down_monitor, :process, self(), :normal},
               down_state
             )

    assert down_state.fd_claims.claims == %{}

    cancel_ref = make_ref()
    cancel_monitor = Process.monitor(self())

    :sys.replace_state(connection, fn current ->
      claimed_state(current, make_ref(), cancel_ref, cancel_monitor)
    end)

    GenServer.cast(connection, {:cancel, cancel_ref})
    assert eventually(fn -> :sys.get_state(connection).fd_claims.claims == %{} end)
  end

  test "rejects stale claim and acknowledgement tokens without changing connection state", %{
    connection: connection
  } do
    delivery_alias = :erlang.alias([:reply])

    assert {:error, :fd_claim_expired} =
             GenServer.call(connection, {:claim_fd_reply, make_ref(), make_ref(), delivery_alias})

    assert {:error, :fd_claim_expired} =
             GenServer.call(connection, {:ack_fd_reply, make_ref(), make_ref()})

    :erlang.unalias(delivery_alias)
    assert Process.alive?(connection)
  end

  test "resolves FD claims as acknowledged, closed, or expired", %{connection: connection} do
    acknowledged_ref = make_ref()
    acknowledged_request = make_ref()
    acknowledged_monitor = Process.monitor(self())

    acknowledged_state =
      claimed_state(
        :sys.get_state(connection),
        acknowledged_ref,
        acknowledged_request,
        acknowledged_monitor
      )

    :sys.replace_state(connection, fn _state -> acknowledged_state end)

    delivery_ref = make_ref()
    delivery_alias = :erlang.alias([:reply])

    assert :ok =
             GenServer.call(
               connection,
               {:claim_fd_reply, acknowledged_ref, delivery_ref, delivery_alias}
             )

    assert_receive {:rebus_fd_reply, ^acknowledged_ref, ^delivery_ref, %Message{}}, 1_000
    assert :ok = GenServer.call(connection, {:ack_fd_reply, acknowledged_ref, delivery_ref})

    assert :acknowledged =
             GenServer.call(connection, {:resolve_fd_claim, acknowledged_ref, delivery_ref})

    assert :fd_claim_expired =
             GenServer.call(connection, {:resolve_fd_claim, acknowledged_ref, delivery_ref})

    :erlang.unalias(delivery_alias)

    closed_ref = make_ref()
    closed_request = make_ref()
    closed_monitor = Process.monitor(self())

    closed_state =
      claimed_state(:sys.get_state(connection), closed_ref, closed_request, closed_monitor)

    :sys.replace_state(connection, fn _state -> closed_state end)

    closed_delivery_ref = make_ref()
    closed_alias = :erlang.alias([:reply])

    assert :ok =
             GenServer.call(
               connection,
               {:claim_fd_reply, closed_ref, closed_delivery_ref, closed_alias}
             )

    assert_receive {:rebus_fd_reply, ^closed_ref, ^closed_delivery_ref, %Message{}}, 1_000

    assert :closed =
             GenServer.call(connection, {:resolve_fd_claim, closed_ref, closed_delivery_ref})

    discard_ref = make_ref()
    discard_request = make_ref()
    discard_monitor = Process.monitor(self())

    :sys.replace_state(connection, fn state ->
      claimed_state(state, discard_ref, discard_request, discard_monitor)
    end)

    # A caller which abandons the delivery leg closes the retained claim. A
    # duplicate discard is harmless and must not revive ownership.
    assert :ok = GenServer.call(connection, {:discard_fd_claim, discard_ref})
    assert :ok = GenServer.call(connection, {:discard_fd_claim, discard_ref})

    outcome_ref = make_ref()
    outcome_timer = Process.send_after(self(), {:fd_claim_outcome_timeout, outcome_ref}, 60_000)

    :sys.replace_state(connection, fn state ->
      %{
        state
        | fd_claims: %{state.fd_claims | outcomes: %{outcome_ref => {:closed, outcome_timer}}}
      }
    end)

    send(connection, {:fd_claim_outcome_timeout, outcome_ref})
    assert eventually(fn -> :sys.get_state(connection).fd_claims.outcomes == %{} end)

    state = :sys.get_state(connection)

    assert {:noreply, ^state} =
             Rebus.Connection.handle_info({:fd_claim_outcome_timeout, make_ref()}, state)

    assert {:noreply, ^state} =
             Rebus.Connection.handle_info({:request_timeout, make_ref(), make_ref()}, state)

    :erlang.unalias(closed_alias)
  end

  test "acknowledges only before the absolute claim deadline", %{connection: connection} do
    delivery_ref = make_ref()
    live_ref = make_ref()
    live_request = make_ref()
    live_monitor = Process.monitor(self())

    live_state =
      claimed_state(
        :sys.get_state(connection),
        live_ref,
        live_request,
        live_monitor,
        delivery_ref: delivery_ref,
        deadline: System.monotonic_time(:millisecond) + 100
      )

    :sys.replace_state(connection, fn _state -> live_state end)

    assert :ok = GenServer.call(connection, {:ack_fd_reply, live_ref, delivery_ref})

    assert :acknowledged =
             GenServer.call(connection, {:resolve_fd_claim, live_ref, delivery_ref})

    expired_ref = make_ref()
    expired_request = make_ref()
    expired_monitor = Process.monitor(self())

    expired_state =
      claimed_state(
        :sys.get_state(connection),
        expired_ref,
        expired_request,
        expired_monitor,
        delivery_ref: make_ref(),
        deadline: System.monotonic_time(:millisecond) - 1
      )

    expired_delivery_ref = expired_state.fd_claims.claims[expired_ref].delivery_ref
    :sys.replace_state(connection, fn _state -> expired_state end)

    assert {:error, :fd_claim_expired} =
             GenServer.call(connection, {:ack_fd_reply, expired_ref, expired_delivery_ref})

    assert :closed =
             GenServer.call(connection, {:resolve_fd_claim, expired_ref, expired_delivery_ref})
  end

  test "connection termination closes retained claim state", %{connection: connection} do
    {:ok, sock} = :socket.open(:local, :stream, :default)
    monitor_ref = Process.monitor(self())
    state = claimed_state(:sys.get_state(connection), make_ref(), make_ref(), monitor_ref)

    assert :ok = Rebus.Connection.terminate(:shutdown, %{state | sock: sock})
  end

  test "closes received duplicates of inherited pipe descriptors on Linux", %{
    server: server,
    connection: connection
  } do
    if match?({:unix, :linux}, :os.type()) do
      # CI runs the BEAM with piped standard input and output. SCM_RIGHTS
      # duplicates the descriptors, so closing the inbound values never closes
      # the test runner's own read or write pipe endpoints.
      assert linux_pipe_fd?(0), "expected standard input to be a read pipe"
      assert linux_pipe_fd?(1), "expected standard output to be a write pipe"

      for {fd, member} <- [{0, "ReadPipe"}, {1, "WritePipe"}] do
        method =
          Message.new!(:method_call,
            path: "/test",
            interface: "test.interface",
            member: member
          )

        task = Task.async(fn -> Rebus.call(connection, method, 1_000) end)

        assert_receive {^server, %Message{serial: serial, header_fields: %{member: ^member}}},
                       1_000

        reply =
          Message.new!(:method_return,
            reply_serial: serial,
            signature: "h",
            body: [0],
            fds: [fd]
          )

        :ok = TestServer.push_with_fds(server, reply, [fd])
        assert {:ok, %Message{unix_fds: [received]}} = Task.await(task, 1_000)
        assert received != fd
        assert :ok = UnixFD.close(received)
      end
    end
  end

  test "drops an inbound FD-bearing signal without fan-out and keeps handlers live", %{
    server: server,
    connection: connection
  } do
    {:ok, handler_ref} = Rebus.add_signal_handler(connection)
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    signal =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "SharedFD",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    following =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "AfterSharedFD"
      )

    {:ok, signal_bin} = Message.encode(signal)
    {:ok, following_bin} = Message.encode(following)

    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [IO.iodata_to_binary(signal_bin), IO.iodata_to_binary(following_bin)],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    refute_receive {^handler_ref, %Message{header_fields: %{member: "SharedFD"}}}, 50
    assert_receive {^handler_ref, %Message{header_fields: %{member: "AfterSharedFD"}}}, 1_000
    assert Process.alive?(connection)

    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "answers an FD-bearing inbound method call and closes its descriptor", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    call =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "TakesFD",
        sender: ":1.99",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    {:ok, serial} = TestServer.push_call_with_fds(server, call, [fd])

    assert_receive {^server,
                    %Message{
                      type: :error,
                      header_fields: %{
                        reply_serial: ^serial,
                        destination: ":1.99",
                        error_name: "org.freedesktop.DBus.Error.UnknownMethod"
                      }
                    }},
                   1_000

    assert Process.alive?(connection)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "closes a reply whose original caller died before delivery", %{
    server: server,
    connection: connection
  } do
    parent = self()

    caller =
      spawn(fn ->
        message =
          Message.new!(:method_call,
            path: "/test",
            interface: "test.interface",
            member: "OrphanFD"
          )

        send(parent, {:orphan_result, Rebus.call(connection, message, 1_000)})
      end)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "OrphanFD"}}},
                   1_000

    caller_ref = Process.monitor(caller)
    Process.exit(caller, :kill)
    assert_receive {:DOWN, ^caller_ref, :process, ^caller, :killed}, 1_000

    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd])
    refute_receive {:orphan_result, _result}, 50
    assert Process.alive?(connection)

    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "rejects FD writes after an optional negotiation error and retains recv coalescing" do
    parent = self()
    path = Path.join("/tmp", "rebus-unix-fd-error-#{System.system_time(:nanosecond)}")

    {:ok, server} =
      start_supervised(%{
        id: {:unix_fd_error_server, path},
        start:
          {TestServer, :start_link,
           [[tap: self(), family: :local, path: path, unix_fd_response: "ERROR unsupported\r\n"]]}
      })

    {:ok, address} = TestServer.get_listen_addr(server)

    {:ok, connection} =
      Rebus.connect(address, __impl__: %{identity: TestImpl.CachedIdentity})

    on_exit(fn -> if Process.alive?(connection), do: Rebus.close(connection) end)

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "FD",
        signature: "h",
        body: [0],
        fds: [0]
      )

    assert {:error, :unix_fd_not_negotiated} = Rebus.send(connection, message)

    {:ok, handler_ref} = Rebus.add_signal_handler(connection)
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    following =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "AfterNegotiationError"
      )

    {:ok, fd_encoded} = Message.encode(message)
    {:ok, following_encoded} = Message.encode(following)

    # CtrlSz=0 selects OTP's default buffer. The declined path therefore keeps
    # a bounded control buffer, closes illicit rights immediately, quarantines
    # only their frame, and still parses the coalesced successor.
    :ok =
      TestImpl.install(connection,
        recvmsg: fn sock, length, control_size, flags, timeout ->
          send(parent, {:declined_recvmsg, length, control_size})
          :socket.recvmsg(sock, length, control_size, flags, timeout)
        end
      )

    fd_encoded = IO.iodata_to_binary(fd_encoded)
    <<prefix::binary-size(8), tail::binary>> = fd_encoded

    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [prefix],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    # The illicit descriptor is closed before the first D-Bus frame completes;
    # only a taint bit remains while the eight-byte prefix is buffered.
    assert eventually(fn ->
             state = :sys.get_state(connection)
             state.inbound_fds.tainted? and state.inbound.size == byte_size(prefix)
           end)

    :ok =
      :socket.send(
        :sys.get_state(server).cli_sock,
        [tail, IO.iodata_to_binary(following_encoded)],
        [],
        1_000
      )

    assert_receive {^handler_ref, %Message{header_fields: %{member: "AfterNegotiationError"}}},
                   1_000

    assert_receive {:declined_recvmsg, 0, 256}, 1_000

    assert Process.alive?(connection)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "rejects FD writes on TCP before writing an ambiguous frame" do
    {:ok, server} = start_supervised({TestServer, tap: self()}, id: :unix_fd_tcp_server)
    {:ok, address} = TestServer.get_listen_addr(server)

    {:ok, connection} =
      Rebus.connect(address, __impl__: %{identity: TestImpl.CachedIdentity})

    on_exit(fn -> if Process.alive?(connection), do: Rebus.close(connection) end)

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "FD",
        signature: "h",
        body: [0],
        fds: [0]
      )

    assert {:error, :unix_fd_unsupported} = Rebus.send(connection, message)
    refute_receive {^server, %Message{header_fields: %{member: "FD"}}}, 100
  end

  test "rejects an already-closed borrowed FD without stopping other calls", %{
    server: server,
    connection: connection
  } do
    {:ok, disposable} = :socket.open(:local, :stream, :default)
    {:ok, closed_fd} = :socket.getopt(disposable, {:otp, :fd})
    :ok = :socket.close(disposable)

    stale =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "StaleFD",
        signature: "h",
        body: [0],
        fds: [closed_fd]
      )

    assert {:error, :unix_fd_send_failed} = Rebus.send(connection, stale)
    assert Process.alive?(connection)

    healthy =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "AfterStaleFD"
      )

    assert :ok = Rebus.send(connection, healthy)
    assert_receive {^server, %Message{header_fields: %{member: "AfterStaleFD"}}}, 1_000
  end

  test "handles an unaccepted descriptor error with an explicit RestData", %{
    connection: connection
  } do
    :ok =
      TestImpl.install(connection,
        sendmsg: fn _sock, %{iov: [rest]}, [], :nowait -> {:error, {:ebadf, rest}} end
      )

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "TupleStaleFD",
        signature: "h",
        body: [0],
        fds: [0]
      )

    assert {:error, :unix_fd_send_failed} = Rebus.send(connection, message)
    assert Process.alive?(connection)
  end

  test "stops after a partial FD frame later fails", %{connection: connection} do
    :ok =
      TestImpl.install(connection,
        sendmsg: fn _sock, %{iov: [rest]}, [], :nowait ->
          {:ok, binary_part(rest, 1, byte_size(rest) - 1)}
        end,
        send: fn _sock, _rest, [], :nowait -> {:error, :closed} end
      )

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "PartialStaleFD",
        signature: "h",
        body: [0],
        fds: [0]
      )

    ref = Process.monitor(connection)
    assert {:error, :disconnected} = Rebus.send(connection, message)
    assert_receive {:DOWN, ^ref, :process, ^connection, {:shutdown, :closed}}, 1_000
  end

  test "drops a descriptor with no matching header count and delivers a coalesced reply", %{
    server: server,
    connection: connection
  } do
    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "AfterBadFDCount"
      )

    task = Task.async(fn -> Rebus.call(connection, method, 1_000) end)

    assert_receive {^server,
                    %Message{serial: serial, header_fields: %{member: "AfterBadFDCount"}}},
                   1_000

    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    message =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "UnexpectedFD"
      )

    {:ok, encoded} = Message.encode(message)
    reply = Message.new!(:method_return, reply_serial: serial)
    {:ok, reply_encoded} = Message.encode(reply)

    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [IO.iodata_to_binary(encoded), IO.iodata_to_binary(reply_encoded)],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    assert {:ok, %Message{header_fields: %{reply_serial: ^serial}}} = Task.await(task, 1_000)
    assert Process.alive?(connection)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "drops an h index beyond the received descriptor vector and keeps signals live", %{
    server: server,
    connection: connection
  } do
    {:ok, handler_ref} = Rebus.add_signal_handler(connection)
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    valid =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "BadIndex",
        signature: "h",
        body: [0],
        fds: [fd]
      )

    {:ok, encoded} = Message.encode(valid)
    encoded = IO.iodata_to_binary(encoded)
    <<prefix::binary-size(byte_size(encoded) - 4), _index::binary-size(4)>> = encoded

    following =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "AfterBadIndex"
      )

    {:ok, following_encoded} = Message.encode(following)

    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [prefix, <<1::little-32>>, IO.iodata_to_binary(following_encoded)],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    assert_receive {^handler_ref, %Message{header_fields: %{member: "AfterBadIndex"}}}, 1_000
    assert Process.alive?(connection)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "quarantines an over-limit ancillary frame and keeps its successor live", %{
    server: server,
    connection: connection
  } do
    {:ok, handler_ref} = Rebus.add_signal_handler(connection)
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    rejected =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "TooManyFDs"
      )

    following =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "AfterTooManyFDs"
      )

    {:ok, rejected_encoded} = Message.encode(rejected)
    {:ok, following_encoded} = Message.encode(following)
    rights = for _ <- 1..17, into: <<>>, do: <<fd::native-signed-32>>

    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [IO.iodata_to_binary(rejected_encoded), IO.iodata_to_binary(following_encoded)],
          ctrl: [%{level: :socket, type: :rights, data: rights}]
        },
        [],
        1_000
      )

    refute_receive {^handler_ref, %Message{header_fields: %{member: "TooManyFDs"}}}, 50

    assert_receive {^handler_ref, %Message{header_fields: %{member: "AfterTooManyFDs"}}},
                   1_000

    assert Process.alive?(connection)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "CTRUNC closes descriptors after malformed control data", %{connection: connection} do
    {sender, receiver, listener, path} = local_socket_pair!()

    on_exit(fn ->
      _ = :socket.close(sender)
      _ = :socket.close(receiver)
      _ = :socket.close(listener)
      _ = File.rm(path)
    end)

    {:ok, fd} = :socket.getopt(sender, {:otp, :fd})
    before_fds = fd_set!()

    assert {:ok,
            %{
              iov: ["ignored"],
              ctrl: [%{level: :socket, type: :rights, data: <<received::native-signed-32>>}]
            } = recvmsg} = receive_rights_message!(sender, receiver, fd)

    assert {:stop, {:shutdown, :unix_fd_truncated}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok,
                %{
                  recvmsg
                  | ctrl: [%{level: :socket, type: :rights, data: <<1>>} | recvmsg.ctrl],
                    flags: [:ctrunc]
                }},
               :sys.get_state(connection)
             )

    # The malformed first cmsg must not prevent the later, kernel-created
    # descriptor from being found and closed on the fail-closed path.
    assert {:error, :ebadf} = UnixFD.close(received)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "invalid recvmsg shapes close decoded descriptors", %{connection: connection} do
    {sender, receiver, listener, path} = local_socket_pair!()

    on_exit(fn ->
      _ = :socket.close(sender)
      _ = :socket.close(receiver)
      _ = :socket.close(listener)
      _ = File.rm(path)
    end)

    {:ok, fd} = :socket.getopt(sender, {:otp, :fd})
    before_fds = fd_set!()

    assert {:ok,
            %{
              ctrl: [%{level: :socket, type: :rights, data: <<invalid_iov_fd::native-signed-32>>}]
            } = recvmsg} = receive_rights_message!(sender, receiver, fd)

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok, %{recvmsg | iov: [:not_iodata]}},
               :sys.get_state(connection)
             )

    assert {:error, :ebadf} = UnixFD.close(invalid_iov_fd)

    assert {:ok,
            %{
              ctrl: [%{level: :socket, type: :rights, data: <<missing_iov_fd::native-signed-32>>}]
            } = recvmsg} = receive_rights_message!(sender, receiver, fd)

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok, Map.drop(recvmsg, [:iov])},
               :sys.get_state(connection)
             )

    assert {:error, :ebadf} = UnixFD.close(missing_iov_fd)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "handles recvmsg select and malformed-control transitions", %{
    connection: connection
  } do
    state = :sys.get_state(connection)
    select_handle = make_ref()

    assert {:noreply, %{rref: ^select_handle}} =
             Rebus.Connection.handle_receive_result(
               {:select, {:select_info, :recvmsg, select_handle}},
               state
             )

    # The completion backend is unsupported, so its receive result is an
    # unknown shape that stops the connection instead of hanging.
    assert {:stop, {:shutdown, :receive_failed}, _state} =
             Rebus.Connection.handle_receive_result(
               {:completion, {:completion_info, :recvmsg, make_ref()}},
               state
             )

    assert {:noreply, %{rref: ^select_handle}} =
             Rebus.Connection.handle_receive_result(
               {:select,
                {{:select_info, :recvmsg, select_handle}, %{iov: [], ctrl: [], flags: []}}},
               state
             )

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok,
                %{
                  iov: [],
                  ctrl: [%{level: :socket, type: :rights, data: <<1>>}],
                  flags: []
                }},
               state
             )

    assert {:stop, {:shutdown, :unix_fd_truncated}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok, %{iov: ["ignored"], ctrl: [], flags: [:ctrunc]}},
               state
             )

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result({:ok, %{invalid: :control}}, state)

    assert {:stop, {:shutdown, :receive_failed}, _state} =
             Rebus.Connection.handle_receive_result(:invalid_result, state)

    assert {:stop, {:shutdown, :read_timeout}, _state} =
             Rebus.Connection.handle_continue(
               :hello_reply,
               TestImpl.stub(state, recvmsg: fn _, _, _, _, _ -> {:error, :timeout} end)
             )

    assert {:stop, {:shutdown, :receive_failed}, _state} =
             Rebus.Connection.handle_continue(
               :hello_reply,
               TestImpl.stub(state, recvmsg: fn _, _, _, _, _ -> :unexpected end)
             )

    assert {:noreply, _state} =
             Rebus.Connection.handle_continue(
               :hello_reply,
               TestImpl.stub(state,
                 recvmsg: fn _, _, _, _, _ ->
                   {:error, {:timeout, %{iov: [], ctrl: [], flags: []}}}
                 end
               )
             )

    assert {:stop, {:shutdown, :read_timeout}, _state} =
             Rebus.Connection.handle_continue(
               :hello_reply,
               TestImpl.stub(state,
                 recvmsg: fn _, _, _, _, _ -> {:error, {:timeout, :partial}} end
               )
             )

    assert {:stop, {:shutdown, :closed}, _state} =
             Rebus.Connection.handle_continue(
               :hello_reply,
               TestImpl.stub(state, recvmsg: fn _, _, _, _, _ -> {:error, :closed} end)
             )

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok,
                %{
                  iov: [],
                  ctrl: [%{level: :socket, type: :rights, data: <<1_000_000::native-signed-32>>}],
                  flags: []
                }},
               %{state | inbound: %Inbound{size: 1}}
             )

    # Even when SCM_RIGHTS decoding finds a malformed tail, CTRUNC wins: the
    # kernel may have installed descriptors absent from the reported control
    # payload, so the connection must fail closed rather than quarantine a
    # frame locally.
    assert {:stop, {:shutdown, :unix_fd_truncated}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok,
                %{
                  iov: ["ignored"],
                  ctrl: [
                    %{
                      level: :socket,
                      type: :rights,
                      data: <<1_000_000::native-signed-32, 1>>
                    }
                  ],
                  flags: [:ctrunc]
                }},
               state
             )

    assert {:stop, {:shutdown, :message_too_large}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok, %{iov: [:binary.copy(<<0>>, 65_537)], ctrl: [], flags: []}},
               state
             )

    too_many_rights =
      for _ <- 1..17, into: <<>>, do: <<1_000_000::native-signed-32>>

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok,
                %{
                  iov: [],
                  ctrl: [%{level: :socket, type: :rights, data: too_many_rights}],
                  flags: []
                }},
               state
             )

    # The same precedence applies when the complete decoded descriptor list
    # exceeds the local limit; the descriptors travel with the truncation
    # result and are closed before protocol shutdown.
    assert {:stop, {:shutdown, :unix_fd_truncated}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok,
                %{
                  iov: ["ignored"],
                  ctrl: [%{level: :socket, type: :rights, data: too_many_rights}],
                  flags: [:ctrunc]
                }},
               state
             )
  end

  # The set of descriptors this OS process holds open. Both `/proc/self/fd` and
  # `/dev/fd` answer for the reading process only, which is what the leak
  # assertions compare. `lsof` answers the same question but costs about 700ms
  # per call once the whole suite has run in this VM, against about 30µs for a
  # directory listing, so it is only a last resort on a Unix without `/dev/fd`.
  defp fd_set! do
    case :os.type() do
      {:unix, :linux} -> listed_fd_set!("/proc/self/fd")
      {:unix, _bsd} -> bsd_fd_set!()
      _other -> flunk("Unix FD leak test ran on an unsupported platform")
    end
  end

  defp bsd_fd_set! do
    if File.dir?("/dev/fd"), do: listed_fd_set!("/dev/fd"), else: lsof_fd_set!()
  end

  defp listed_fd_set!(directory) do
    directory
    |> File.ls!()
    |> MapSet.new()
  end

  defp lsof_fd_set! do
    lsof = System.find_executable("lsof") || flunk("lsof is required for Unix FD leak tests")

    lsof
    |> System.cmd(["-Fn", "-p", to_string(:os.getpid())])
    |> elem(0)
    |> String.split("\n", trim: true)
    |> Enum.flat_map(fn
      <<"f", descriptor::binary>> ->
        case Integer.parse(descriptor) do
          {fd, ""} -> [Integer.to_string(fd)]
          _not_a_descriptor -> []
        end

      _other_field ->
        []
    end)
    |> MapSet.new()
  end

  defp local_socket_pair! do
    path = Path.join("/tmp", "rebus-unix-fd-pair-#{System.unique_integer([:positive])}")
    {:ok, listener} = :socket.open(:local, :stream, :default)
    :ok = :socket.bind(listener, %{family: :local, path: path})
    :ok = :socket.listen(listener, 1)
    {:ok, sender} = :socket.open(:local, :stream, :default)
    {:ok, address} = :socket.sockname(listener)
    :ok = :socket.connect(sender, address)
    {:ok, receiver} = :socket.accept(listener, 1_000)
    {sender, receiver, listener, path}
  end

  defp receive_rights_message!(sender, receiver, fd) do
    :ok =
      :socket.sendmsg(
        sender,
        %{
          iov: ["ignored"],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    :socket.recvmsg(receiver, 0, 256, [], 1_000)
  end

  defp linux_pipe_fd?(fd) do
    case File.read_link("/proc/self/fd/#{fd}") do
      {:ok, "pipe:[" <> _id} -> true
      _ -> false
    end
  end

  defp claimed_state(state, claim_ref, request_ref, monitor_ref, opts \\ []) do
    timer_ref = Process.send_after(self(), {:fd_claim_timeout, claim_ref}, 60_000)

    claim = %{
      pid: self(),
      msg: Message.new!(:method_return, reply_serial: 1),
      request_ref: request_ref,
      monitor_ref: monitor_ref,
      timer_ref: timer_ref,
      delivery_ref: Keyword.get(opts, :delivery_ref),
      delivery_alias: Keyword.get(opts, :delivery_alias),
      deadline: Keyword.get(opts, :deadline, System.monotonic_time(:millisecond) + 60_000)
    }

    %{
      state
      | fd_claims: %FDClaims{
          claims: %{claim_ref => claim},
          request_index: %{request_ref => claim_ref},
          monitor_index: %{monitor_ref => claim_ref}
        }
    }
  end

  defp eventually(fun, attempts \\ 20)

  defp eventually(fun, attempts) when attempts > 0 do
    if fun.() do
      true
    else
      Process.sleep(10)
      eventually(fun, attempts - 1)
    end
  end

  defp eventually(_fun, 0), do: false
end
