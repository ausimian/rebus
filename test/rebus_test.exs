defmodule RebusTest do
  use ExUnit.Case
  doctest Rebus
  import ExUnit.CaptureLog

  alias Rebus.Connection
  alias Rebus.Message
  alias Rebus.SignalHandler
  alias Rebus.TestServer

  # GitHub run 33276531794 on Elixir 1.19.1/OTP 27.1 observed a referenced
  # byte size of 256 for copied GUID and mechanism binaries. This remains far
  # below the 64–270KB source buffers used by the retention regressions.
  @max_copied_referenced_bytes 256
  @test_bus_guid "30313233343536373839414243444546"

  describe "Connections" do
    setup [:server_setup]

    test "can be established with inet socket", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      {:ok, _cli} = Rebus.connect(addr)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test "connect! returns pid on success", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      pid = Rebus.connect!(addr)

      assert is_pid(pid)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test "drops resource-limited frames and continues coalesced signals", %{svr: svr} do
      cli = connect_until_ready(svr)
      ref = make_ref()

      :ok = :gen_event.add_sup_handler(SignalHandler, {SignalHandler, ref}, {cli, self(), ref})
      on_exit(fn -> :gen_event.delete_handler(SignalHandler, {SignalHandler, ref}, nil) end)

      payload_sentinel = "resource-limit-body-sentinel"
      limited_data = raw_resource_limited_signal()

      valid =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "AfterLimit"
        )

      assert {:error, :resource_limit} = Message.decode(limited_data)
      {:ok, valid_data} = Message.encode(valid)

      log =
        capture_log(fn ->
          :ok =
            TestServer.push_raw(
              svr,
              limited_data <> IO.iodata_to_binary(valid_data)
            )

          assert_receive {^ref, %Message{header_fields: %{member: "AfterLimit"}}}, 2_000
          assert Process.alive?(cli)
        end)

      assert log =~ "D-Bus frame dropped: :resource_limit"
      refute log =~ "D-Bus connection protocol stopped"
      refute log =~ payload_sentinel
    end

    test "drops a resource-limited reply, fails its caller, and keeps parsing", %{svr: svr} do
      cli = connect_until_ready(svr)
      signal_ref = make_ref()

      :ok =
        :gen_event.add_sup_handler(
          SignalHandler,
          {SignalHandler, signal_ref},
          {cli, self(), signal_ref}
        )

      on_exit(fn ->
        :gen_event.delete_handler(SignalHandler, {SignalHandler, signal_ref}, nil)
      end)

      method =
        Message.new!(:method_call,
          path: "/test",
          interface: "test.interface",
          member: "LimitedReply"
        )

      call_task = Task.async(fn -> Rebus.call(cli, method, 5_000) end)
      assert_receive {^svr, %Message{header_fields: %{member: "LimitedReply"}} = request}, 1_000

      [{_serial, {_from, timer_ref, _request_ref, _monitor_ref, _deadline}}] =
        :sys.get_state(cli).pending |> Map.to_list()

      limited_reply = raw_resource_limited_reply(request.serial)

      following_signal =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "AfterLimitedReply"
        )

      {:ok, following_signal_data} = Message.encode(following_signal)
      started_at = System.monotonic_time(:millisecond)

      log =
        capture_log(fn ->
          :ok =
            TestServer.push_raw(svr, limited_reply <> IO.iodata_to_binary(following_signal_data))

          assert {:error, {:reply_dropped, :method_return}} = Task.await(call_task, 2_000)
          assert System.monotonic_time(:millisecond) - started_at < 4_000

          assert_receive {^signal_ref, %Message{header_fields: %{member: "AfterLimitedReply"}}},
                         2_000
        end)

      assert log =~ "D-Bus frame dropped: :resource_limit"
      refute log =~ "resource-limit-body-sentinel"
      assert Process.read_timer(timer_ref) == false

      assert wait_until(fn ->
               state = :sys.get_state(cli)

               state.pending == %{} and state.request_index == %{} and state.monitor_index == %{} and
                 map_size(state.outbound_monitor_index) == 0
             end)

      assert Process.alive?(cli)

      follow_up =
        Message.new!(:method_call,
          path: "/test",
          interface: "test.interface",
          member: "AfterDroppedReply"
        )

      follow_up_task = Task.async(fn -> Rebus.call(cli, follow_up, 1_000) end)

      assert_receive {^svr,
                      %Message{header_fields: %{member: "AfterDroppedReply"}} = follow_up_request},
                     1_000

      :ok =
        TestServer.push(svr, Message.new!(:method_return, reply_serial: follow_up_request.serial))

      assert %Message{type: :method_return} = Task.await(follow_up_task, 1_000)
    end

    test "reports the validated error name when an error reply is resource-limited", %{svr: svr} do
      cli = connect_until_ready(svr)

      method =
        Message.new!(:method_call,
          path: "/test",
          interface: "test.interface",
          member: "LimitedErrorReply"
        )

      call_task = Task.async(fn -> Rebus.call(cli, method, 5_000) end)

      assert_receive {^svr, %Message{header_fields: %{member: "LimitedErrorReply"}} = request},
                     1_000

      [{_serial, {_from, timer_ref, _request_ref, _monitor_ref, _deadline}}] =
        :sys.get_state(cli).pending |> Map.to_list()

      error_name = "org.example.ResourceLimited"
      limited_reply = raw_resource_limited_error_reply(request.serial, error_name)

      log =
        capture_log(fn ->
          :ok = TestServer.push_raw(svr, limited_reply)

          assert {:error, {:reply_dropped, {:error, ^error_name}}} =
                   Task.await(call_task, 2_000)
        end)

      assert log =~ "D-Bus frame dropped: :resource_limit"
      refute log =~ "resource-limit-body-sentinel"
      assert Process.read_timer(timer_ref) == false

      assert wait_until(fn ->
               state = :sys.get_state(cli)

               state.pending == %{} and state.request_index == %{} and state.monitor_index == %{}
             end)

      assert Process.alive?(cli)
    end

    test "drops a header-limited frame without closing the connection", %{svr: svr} do
      cli = connect_until_ready(svr)
      ref = make_ref()

      :ok = :gen_event.add_sup_handler(SignalHandler, {SignalHandler, ref}, {cli, self(), ref})
      on_exit(fn -> :gen_event.delete_handler(SignalHandler, {SignalHandler, ref}, nil) end)

      limited_data = raw_header_resource_limited_signal()

      valid =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "AfterHeaderLimit"
        )

      {:ok, valid_data} = Message.encode(valid)
      assert {:error, :resource_limit} = Message.decode(limited_data)

      log =
        capture_log(fn ->
          :ok = TestServer.push_raw(svr, limited_data <> IO.iodata_to_binary(valid_data))
          assert_receive {^ref, %Message{header_fields: %{member: "AfterHeaderLimit"}}}, 2_000
        end)

      assert log =~ "D-Bus frame dropped: :resource_limit"
      assert Process.alive?(cli)
    end

    test "treats truncated over-declared arrays as protocol errors", %{svr: svr} do
      cli = connect_until_ready(svr)
      ref = Process.monitor(cli)

      log =
        capture_log(fn ->
          :ok = TestServer.push_raw(svr, raw_truncated_scalar_signal())

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :invalid_message}}, 1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: :invalid_message"
      refute log =~ "D-Bus frame dropped: :resource_limit"
    end

    test "rejects an invalid write timeout", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:error, :invalid_write_timeout} = Rebus.connect(addr, write_timeout: -1)
    end

    test "rejects an invalid read timeout", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:error, :invalid_read_timeout} = Rebus.connect(addr, read_timeout: 0)
    end

    test "rejects an invalid connection timeout", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:error, :invalid_timeout} = Rebus.connect(addr, timeout: 0)
    end

    test "returns the live PID for a local name collision", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_name_taken
      assert {:ok, cli} = Rebus.connect(addr, name: name)
      assert {:error, {:name_taken, ^cli}} = Rebus.connect(addr, name: name)

      on_exit(fn ->
        if pid = Process.whereis(name), do: Rebus.close(pid)
      end)

      assert cli == Process.whereis(name)
    end

    test "distinguishes an unrelated local name registration", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_unrelated_name
      owner = self()
      assert Process.register(owner, name)

      on_exit(fn ->
        if Process.whereis(name) == owner, do: Process.unregister(name)
      end)

      assert {:error, {:name_registered, ^owner}} = Rebus.connect(addr, name: name)
    end

    test "can recover a named connection after its owner exits", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_owner_exit_connection
      parent = self()

      owner =
        spawn(fn ->
          send(parent, {:owner_connection, Rebus.connect(addr, name: name)})
        end)

      owner_monitor = Process.monitor(owner)
      assert_receive {:owner_connection, {:ok, cli}}, 1_000
      assert_receive {:DOWN, ^owner_monitor, :process, ^owner, :normal}, 1_000
      assert Process.whereis(name) == cli
      assert {:error, {:name_taken, ^cli}} = Rebus.connect(addr, name: name)

      assert :ok = Rebus.close(cli)
      assert wait_until(fn -> Process.whereis(name) == nil end)

      {:ok, retry_svr} =
        start_supervised({Rebus.TestServer, tap: self()}, id: :owner_exit_retry_server)

      {:ok, retry_addr} = TestServer.get_listen_addr(retry_svr)
      assert {:ok, retry_cli} = Rebus.connect(retry_addr, name: name)
      assert Process.whereis(name) == retry_cli
      assert :ok = Rebus.close(retry_cli)
    end

    test "returns not found when closing a non-connection PID" do
      assert {:error, :not_found} = Rebus.close(self())
    end

    test "keeps signal-handler deletion scoped to its connection" do
      {:ok, svr_a} = start_supervised({Rebus.TestServer, tap: self()}, id: :signal_scope_a)
      {:ok, svr_b} = start_supervised({Rebus.TestServer, tap: self()}, id: :signal_scope_b)
      {:ok, addr_a} = TestServer.get_listen_addr(svr_a)
      {:ok, addr_b} = TestServer.get_listen_addr(svr_b)
      {:ok, conn_a} = Rebus.connect(addr_a)
      {:ok, conn_b} = Rebus.connect(addr_b)
      ref_b = Rebus.add_signal_handler(conn_b)

      assert :ok = Rebus.delete_signal_handler(conn_a, ref_b)

      :ok =
        TestServer.push(
          svr_b,
          Message.new!(:signal,
            path: "/",
            interface: "org.example.Test",
            member: "ForeignDelete",
            body: []
          )
        )

      assert_receive {^ref_b, %Message{header_fields: %{member: "ForeignDelete"}}}, 1_000

      assert :ok = Rebus.delete_signal_handler(conn_b, ref_b)

      :ok =
        TestServer.push(
          svr_b,
          Message.new!(:signal,
            path: "/",
            interface: "org.example.Test",
            member: "DeletedOnOwner",
            body: []
          )
        )

      refute_receive {^ref_b, %Message{header_fields: %{member: "DeletedOnOwner"}}}, 100
      assert :ok = Rebus.close(conn_a)
      assert :ok = Rebus.close(conn_b)
    end

    test "normalizes bounded auth identity lookup outcomes", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)

      assert {:ok, "353031"} = Connection.get_auth_id(100, fn _timeout -> {:ok, "501\n"} end)

      assert {:error, :auth_id_unavailable} =
               Connection.get_auth_id(100, fn _ -> {:error, :exit_status} end)

      assert {:error, :auth_id_unavailable} =
               Connection.get_auth_id(100, fn _ -> {:ok, "uid"} end)

      assert {:error, :auth_id_unavailable} =
               Connection.get_auth_id(100, fn _ -> {:ok, String.duplicate("1", 65)} end)

      assert {:error, :read_timeout} = Connection.get_auth_id(100, fn _ -> {:error, :timeout} end)

      assert {:error, :auth_id_unavailable} =
               Connection.get_auth_id(100, fn _ -> raise "auth identity runner failed" end)

      assert {:error, :auth_id_unavailable} =
               Connection.get_auth_id(100, fn _ -> throw(:auth_identity_runner_failed) end)

      assert {:error, :auth_id_unavailable} =
               Connection.get_auth_id(100, fn _ ->
                 Connection.run_auth_id(
                   100,
                   fn _ -> "/missing/id" end,
                   fn _, _ -> raise "port open failed" end
                 )
               end)

      assert {:error, :enoent} = Connection.run_auth_id(100, fn _ -> nil end)

      assert {:error, :enoent} =
               Connection.run_auth_id(100, fn _ -> raise "executable lookup failed" end)

      assert {:error, :enoent} =
               Connection.run_auth_id(100, fn _ -> throw(:executable_lookup_failed) end)

      assert {:error, :port_open_failed} =
               Connection.run_auth_id(
                 100,
                 fn _ -> "/missing/id" end,
                 fn _, _ -> throw(:port_open_failed) end
               )

      parent = self()

      assert {:ok, cli} =
               Rebus.connect(addr,
                 auth_id_fun: fn _timeout ->
                   send(parent, :malicious_auth_id_fun)
                   {:error, :exit_status}
                 end,
                 auth_username_fun: :invalid
               )

      refute_receive :malicious_auth_id_fun
      assert :ok = Rebus.close(cli)

      assert {:ok, precomputed_cli} =
               DynamicSupervisor.start_child(
                 Rebus.ConnectionSupervisor,
                 {Connection,
                  addr: addr,
                  precomputed_auth_id: "353031",
                  auth_id_fun: fn _timeout ->
                    send(parent, :precomputed_auth_id_runner)
                    {:error, :exit_status}
                  end}
               )

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
      refute_receive :precomputed_auth_id_runner
      assert Rebus.close(precomputed_cli) in [:ok, {:error, :not_found}]
    end

    test "normalizes internal username lookup outcomes" do
      assert {:ok, "rebus-user"} =
               Connection.get_auth_username(100, fn _timeout -> {:ok, "rebus-user\n"} end)

      assert {:error, :auth_cookie_unavailable} =
               Connection.get_auth_username(100, fn _timeout -> {:error, :exit_status} end)

      assert {:error, :auth_cookie_unavailable} =
               Connection.get_auth_username(100, fn _timeout ->
                 {:ok, String.duplicate("a", 65)}
               end)

      assert {:error, :auth_cookie_unavailable} =
               Connection.get_auth_username(100, fn _timeout -> {:ok, "invalid user"} end)

      assert {:error, :read_timeout} =
               Connection.get_auth_username(100, fn _timeout -> {:error, :timeout} end)

      assert {:error, :enoent} = Connection.run_auth_username(100, fn _ -> nil end)

      assert {:error, :port_open_failed} =
               Connection.run_auth_username(
                 100,
                 fn _ -> "/missing/id" end,
                 fn _, _ -> throw(:port_open_failed) end
               )
    end

    test "registers the connection under its optional name", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_named_connection

      assert {:ok, cli} = Rebus.connect(addr, name: name)
      assert Process.whereis(name) == cli
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}

      on_exit(fn ->
        if pid = Process.whereis(name), do: Rebus.close(pid)
      end)
    end

    test "rejects a non-atom connection name", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:error, :invalid_name} = Rebus.connect(addr, name: "rebus")
    end

    test "rejects an invalid internal authentication identity runner", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)

      assert {:error, :invalid_auth_id_fun} =
               DynamicSupervisor.start_child(
                 Rebus.ConnectionSupervisor,
                 {Connection, addr: addr, auth_id_fun: :invalid}
               )
    end

    test "connect! raises on failure" do
      # Try to connect to non-existent socket
      assert_raise RuntimeError, ~r/Failed to connect to D-Bus/, fn ->
        Rebus.connect!(%{family: :inet, addr: {{127, 0, 0, 1}, 9999}})
      end
    end

    test "returns a socket error when the Unix socket does not exist" do
      socket_path = socket_path()

      assert {:error, reason} = Rebus.connect(%{family: :local, path: socket_path})
      assert reason in [:econnrefused, :enoent]
    end

    test "returns an authentication error after authentication rejection" do
      {:ok, rejecting_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), auth_response: "REJECTED EXTERNAL\r\n"},
          id: :rejecting_server
        )

      {:ok, addr} = TestServer.get_listen_addr(rejecting_svr)

      assert {:error, {:auth_rejected, ["EXTERNAL"]}} = Rebus.connect(addr)
      assert_receive {^rejecting_svr, :client_closed}, 1_000
    end

    test "reports every mechanism advertised by a rejected authentication peer" do
      {:ok, rejecting_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), auth_response: "REJECTED ANONYMOUS EXTERNAL\r\n"},
          id: :multi_mechanism_rejecting_server
        )

      {:ok, addr} = TestServer.get_listen_addr(rejecting_svr)

      assert {:error, {:auth_rejected, ["ANONYMOUS", "EXTERNAL"]}} = Rebus.connect(addr)
      assert_receive {^rejecting_svr, :client_closed}, 1_000
    end

    test "copies rejected mechanisms from coalesced authentication input" do
      padding = String.duplicate("x", 65_536)

      {:ok, rejecting_svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(), auth_response: "REJECTED ANONYMOUS EXTERNAL\r\n" <> padding},
          id: :coalesced_rejecting_server
        )

      {:ok, addr} = TestServer.get_listen_addr(rejecting_svr)

      assert {:error, {:auth_rejected, mechanisms}} = Rebus.connect(addr)
      assert mechanisms == ["ANONYMOUS", "EXTERNAL"]

      for mechanism <- mechanisms do
        assert :binary.referenced_byte_size(mechanism) <= @max_copied_referenced_bytes
      end

      assert_receive {^rejecting_svr, outcome}, 1_000
      assert outcome in [:client_closed, {:client_close_outcome, {:error, :econnreset}}]
    end

    test "reports an empty mechanism list after a bare authentication rejection" do
      {:ok, rejecting_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), auth_response: "REJECTED\r\n"},
          id: :bare_rejecting_server
        )

      {:ok, addr} = TestServer.get_listen_addr(rejecting_svr)
      assert {:error, {:auth_rejected, []}} = Rebus.connect(addr)
      assert_receive {^rejecting_svr, :client_closed}, 1_000
    end

    test "returns a read timeout for a silent authentication peer", %{svr: svr} do
      {:ok, silent_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :silent_auth_server
        )

      {:ok, silent_addr} = TestServer.get_listen_addr(silent_svr)

      assert {:error, :read_timeout} = Rebus.connect(silent_addr, read_timeout: 300)
      assert_receive {^silent_svr, :auth_received}, 1_000

      {:ok, working_addr} = TestServer.get_listen_addr(svr)
      assert {:ok, _cli} = Rebus.connect(working_addr, read_timeout: 500)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test "does not let a silent setup block an independent connection" do
      {:ok, silent_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :concurrent_silent_auth_server
        )

      {:ok, healthy_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), notify_auth: true},
          id: :concurrent_healthy_auth_server
        )

      {:ok, silent_addr} = TestServer.get_listen_addr(silent_svr)
      silent_connect = Task.async(fn -> Rebus.connect(silent_addr, timeout: 1_000) end)
      assert_receive {^silent_svr, :auth_received}, 1_000

      {:ok, healthy_addr} = TestServer.get_listen_addr(healthy_svr)
      healthy_connect = Task.async(fn -> Rebus.connect(healthy_addr, timeout: 1_000) end)

      assert_receive {^healthy_svr, :auth_received}, 500
      assert {:ok, _cli} = Task.await(healthy_connect, 1_000)
      assert_receive {^healthy_svr, %Message{header_fields: %{member: "Hello"}}}
      assert {:error, :read_timeout} = Task.await(silent_connect, 1_500)
    end

    test "waits for a validated Hello reply before returning a usable connection", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      :ok = TestServer.set_auto_hello(svr, false)
      connect_task = Task.async(fn -> Rebus.connect(addr) end)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}
      assert nil == Task.yield(connect_task, 50)
      handle_hello(hello, svr)
      assert {:ok, cli} = Task.await(connect_task, 1_000)

      method =
        Message.new!(:method_call,
          path: "/",
          interface: "org.example.Test",
          member: "ImmediatelyUsable"
        )

      call_task = Task.async(fn -> Rebus.call(cli, method, 500) end)

      assert_receive {^svr, %Message{header_fields: %{member: "ImmediatelyUsable"}} = request},
                     500

      :ok = TestServer.push(svr, Message.new!(:method_return, reply_serial: request.serial))
      assert %Message{type: :method_return} = Task.await(call_task, 1_000)
    end

    test "fails a resource-limited Hello reply promptly instead of waiting for its timeout", %{
      svr: svr
    } do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      :ok = TestServer.set_auto_hello(svr, false)
      read_timeout = 2_000
      connect_task = Task.async(fn -> Rebus.connect(addr, read_timeout: read_timeout) end)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}
      started_at = System.monotonic_time(:millisecond)

      log =
        capture_log(fn ->
          :ok = TestServer.push_raw(svr, raw_resource_limited_reply(hello.serial))

          assert {:error, {:hello_failed, :resource_limit}} = Task.await(connect_task, 500)
        end)

      assert System.monotonic_time(:millisecond) - started_at < div(read_timeout, 2)
      assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :resource_limit}"
      refute log =~ "resource-limit-body-sentinel"
      refute log =~ "%Rebus.Connection"
    end

    test "makes pre-establishment discovery timeouts safe to retry", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_pre_establishment_timeout
      :ok = TestServer.set_auto_hello(svr, false)
      connect_task = Task.async(fn -> Rebus.connect(addr, name: name) end)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}
      cli = Process.whereis(name)
      assert is_pid(cli)

      initial_call =
        Message.new!(:method_call,
          path: "/",
          interface: "org.example.Test",
          member: "BeforeReadyCall"
        )

      initial_send =
        Message.new!(:signal,
          path: "/",
          interface: "org.example.Test",
          member: "BeforeReadySend"
        )

      call_task = Task.async(fn -> Rebus.call(cli, initial_call, 50) end)
      send_task = Task.async(fn -> Rebus.send(cli, initial_send, 50) end)

      assert {:error, :timeout} = Task.await(call_task, 1_000)
      assert {:error, :timeout} = Task.await(send_task, 1_000)
      refute_receive {^svr, %Message{header_fields: %{member: "BeforeReadyCall"}}}, 100
      refute_receive {^svr, %Message{header_fields: %{member: "BeforeReadySend"}}}, 100

      handle_hello(hello, svr)
      assert {:ok, ^cli} = Task.await(connect_task, 1_000)
      refute_receive {^svr, %Message{header_fields: %{member: "BeforeReadyCall"}}}, 100
      refute_receive {^svr, %Message{header_fields: %{member: "BeforeReadySend"}}}, 100

      retry_call = %{
        initial_call
        | header_fields: %{initial_call.header_fields | member: "RetryCall"}
      }

      retry_task = Task.async(fn -> Rebus.call(cli, retry_call, 500) end)

      assert_receive {^svr, %Message{header_fields: %{member: "RetryCall"}} = request}, 1_000
      :ok = TestServer.push(svr, Message.new!(:method_return, reply_serial: request.serial))
      assert %Message{type: :method_return} = Task.await(retry_task, 1_000)

      retry_send = %{
        initial_send
        | header_fields: %{initial_send.header_fields | member: "RetrySend"}
      }

      assert :ok = Rebus.send(cli, retry_send, 500)
      assert_receive {^svr, %Message{header_fields: %{member: "RetrySend"}}}, 1_000
      assert :ok = Rebus.close(cli)
    end

    test "stops a named connection when its caller dies during a delayed Hello", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_caller_gone_during_hello
      :ok = TestServer.set_auto_hello(svr, false)
      connect_task = Task.async(fn -> Rebus.connect(addr, name: name) end)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}
      cli = Process.whereis(name)
      assert is_pid(cli)
      ref = Process.monitor(cli)

      _ = Task.shutdown(connect_task, :brutal_kill)
      handle_hello(hello, svr)

      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :caller_gone}}, 2_000
      assert nil == Process.whereis(name)
    end

    test "returns readiness errors from discovered signal-handler operations" do
      auth_response = "OK 30313233343536373839414243444546\r\n"

      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           notify_auth: true,
           auth_response_fragments: [
             binary_part(auth_response, 0, 8),
             binary_part(auth_response, 8, byte_size(auth_response) - 8)
           ],
           auth_fragment_delay: 300},
          id: :pre_established_signal_handler_server
        )

      name = :rebus_issue_15_signal_handler_connection
      {:ok, addr} = TestServer.get_listen_addr(svr)
      connect_task = Task.async(fn -> Rebus.connect(addr, name: name) end)
      assert_receive {^svr, :auth_received}, 1_000
      pid = Process.whereis(name)
      assert is_pid(pid)

      add_task = Task.async(fn -> Rebus.add_signal_handler(pid) end)
      delete_task = Task.async(fn -> Rebus.delete_signal_handler(pid, make_ref()) end)

      assert {:ok, _cli} = Task.await(connect_task, 1_000)
      assert {:error, :not_connected} = Task.await(add_task, 1_000)
      assert {:error, :not_connected} = Task.await(delete_task, 1_000)

      on_exit(fn ->
        if connection = Process.whereis(name), do: Rebus.close(connection)
      end)
    end

    test "returns a disconnected error for signal handlers on a dead process" do
      dead = spawn(fn -> :ok end)
      ref = Process.monitor(dead)
      assert_receive {:DOWN, ^ref, :process, ^dead, reason}
      assert reason in [:normal, :noproc]

      assert {:error, :disconnected} = Rebus.add_signal_handler(dead)
      assert {:error, :disconnected} = Rebus.delete_signal_handler(dead, make_ref())
    end

    test "cancels a signal handler registration that times out during Hello", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_timed_out_signal_handler
      :ok = TestServer.set_auto_hello(svr, false)
      connect_task = Task.async(fn -> Rebus.connect(addr, name: name, read_timeout: 6_000) end)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}
      cli = Process.whereis(name)
      assert is_pid(cli)

      registration_task = Task.async(fn -> Rebus.add_signal_handler(cli) end)
      assert {:error, :timeout} = Task.await(registration_task, 6_000)

      handle_hello(hello, svr)
      assert {:ok, ^cli} = Task.await(connect_task, 1_000)
      assert :sys.get_state(cli).signal_handler_monitor_index == %{}
      refute Enum.any?(:gen_event.which_handlers(SignalHandler), &match?({SignalHandler, _}, &1))

      :ok =
        TestServer.push(
          svr,
          Message.new!(:signal,
            path: "/",
            interface: "org.example.Test",
            member: "TimedOutRegistration",
            body: []
          )
        )

      refute_receive {_unknown_ref, %Message{header_fields: %{member: "TimedOutRegistration"}}},
                     100
    end

    test "rejects discovered traffic before accepting setup" do
      auth_response = "OK 30313233343536373839414243444546\r\n"

      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           notify_auth: true,
           auth_response_fragments: [
             binary_part(auth_response, 0, 8),
             binary_part(auth_response, 8, byte_size(auth_response) - 8)
           ],
           auth_fragment_delay: 300},
          id: :pre_accept_traffic_server
        )

      name = :rebus_issue_15_pre_accept_connection
      {:ok, addr} = TestServer.get_listen_addr(svr)
      connect_task = Task.async(fn -> Rebus.connect(addr, name: name) end)
      assert_receive {^svr, :auth_received}, 1_000

      pid = Process.whereis(name)
      assert is_pid(pid)

      method =
        Message.new!(:method_call,
          path: "/",
          interface: "org.example.Test",
          member: "Queued"
        )

      call_task = Task.async(fn -> Rebus.call(pid, method, 1_000) end)

      assert {:ok, _cli} = Task.await(connect_task, 1_000)
      assert {:error, :not_connected} = Task.await(call_task, 1_000)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}, serial: 1}}

      on_exit(fn ->
        if connection = Process.whereis(name), do: Rebus.close(connection)
      end)
    end

    test "uses timeout for authentication reads when read_timeout is omitted" do
      {:ok, silent_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :timeout_silent_auth_server
        )

      {:ok, addr} = TestServer.get_listen_addr(silent_svr)

      assert {:error, :read_timeout} =
               Rebus.connect(addr, timeout: 1_000)

      assert_receive {^silent_svr, :auth_received}, 1_000
    end

    test "lets read_timeout override timeout", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:ok, _cli} = Rebus.connect(addr, timeout: 1, read_timeout: 500)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test "keeps the default inbound read timeout after a short setup timeout", %{svr: svr} do
      cli =
        connect_until_ready_direct(svr,
          timeout: 100,
          auth_id_fun: fn _timeout -> {:ok, "501\n"} end
        )

      signal =
        Message.new!(:signal,
          path: "/",
          interface: "org.example.Test",
          member: "Fragmented",
          signature: "s",
          body: [String.duplicate("a", 128)]
        )

      {:ok, encoded} = Message.encode(signal)
      encoded = IO.iodata_to_binary(encoded)
      first = binary_part(encoded, 0, 20)
      rest = binary_part(encoded, 20, byte_size(encoded) - 20)

      assert :ok = TestServer.push_raw_delayed_fragments(svr, [first, rest], 250)
      assert Process.alive?(cli)
    end

    test "releases a failed connection name before an immediate retry", %{svr: svr} do
      {:ok, silent_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :named_retry_silent_auth_server
        )

      name = :rebus_issue_15_retry_connection
      {:ok, silent_addr} = TestServer.get_listen_addr(silent_svr)

      assert {:error, :read_timeout} =
               Rebus.connect(silent_addr,
                 name: name,
                 timeout: 1_000
               )

      assert Process.whereis(name) == nil

      {:ok, working_addr} = TestServer.get_listen_addr(svr)
      assert {:ok, cli} = Rebus.connect(working_addr, name: name)
      assert Process.whereis(name) == cli
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}

      on_exit(fn ->
        if pid = Process.whereis(name), do: Rebus.close(pid)
      end)
    end

    test "closes setup when its waiting caller exits before accepting it" do
      {:ok, svr} =
        start_supervised({Rebus.TestServer, tap: self()}, id: :abandoned_connect_server)

      name = :rebus_issue_15_abandoned_connection
      {:ok, addr} = TestServer.get_listen_addr(svr)
      parent = self()
      connect_ref = make_ref()

      waiter =
        spawn(fn ->
          receive do
            {^connect_ref, {:ok, pid}} ->
              send(parent, {:setup_ready, pid})
              Process.sleep(:infinity)
          end
        end)

      {:ok, cli} =
        DynamicSupervisor.start_child(
          Rebus.ConnectionSupervisor,
          {Rebus.Connection, addr: addr, name: name, connect_waiter: {waiter, connect_ref}}
        )

      ref = Process.monitor(cli)
      assert_receive {:setup_ready, ^cli}, 1_000
      assert Process.whereis(name) == cli
      Process.exit(waiter, :kill)

      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :caller_gone}}, 2_000
      assert Process.whereis(name) == nil
    end

    test "releases a name when its waiter died before setup starts", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      name = :rebus_issue_15_dead_before_setup
      parent = self()

      log =
        capture_log(fn ->
          waiter =
            spawn(fn ->
              receive do
              end
            end)

          Process.exit(waiter, :kill)
          connect_ref = make_ref()

          {:ok, cli} =
            DynamicSupervisor.start_child(
              Rebus.ConnectionSupervisor,
              {Rebus.Connection,
               addr: addr,
               name: name,
               connect_waiter: {waiter, connect_ref},
               auth_id_fun: fn _timeout ->
                 send(parent, :unexpected_auth_id_lookup)
                 {:ok, "501\n"}
               end}
            )

          ref = Process.monitor(cli)
          assert_receive {:DOWN, ^ref, :process, ^cli, reason}, 1_000
          assert reason in [:noproc, {:shutdown, :caller_gone}]
          refute_receive :unexpected_auth_id_lookup, 100

          assert {:ok, retry_cli} = Rebus.connect(addr, name: name)
          assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}, 1_000
          assert :ok = Rebus.close(retry_cli)
        end)

      refute log =~ ~r/GenServer \S+ terminating/
      refute log =~ "** (stop)"
      refute log =~ "D-Bus connection transport stopped:"
      refute log =~ "D-Bus connection protocol stopped:"
    end

    test "closes a stalled setup when its connecting caller exits" do
      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :caller_gone_during_setup_server
        )

      name = :rebus_issue_15_caller_gone_connection
      {:ok, addr} = TestServer.get_listen_addr(svr)

      task =
        Task.async(fn ->
          Rebus.connect(addr,
            name: name,
            timeout: 1_000
          )
        end)

      assert_receive {^svr, :auth_received}, 1_000

      cli = Process.whereis(name)
      assert is_pid(cli)
      ref = Process.monitor(cli)
      _ = Task.shutdown(task, :brutal_kill)

      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :caller_gone}}, 2_000
      assert Process.whereis(name) == nil
    end

    test "returns an error if the connection dies before accepting setup" do
      auth_response = "OK 30313233343536373839414243444546\r\n"

      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           notify_auth: true,
           auth_response_fragments: [
             binary_part(auth_response, 0, 8),
             binary_part(auth_response, 8, byte_size(auth_response) - 8)
           ],
           auth_fragment_delay: 300},
          id: :accepted_connection_death_server
        )

      name = :rebus_issue_15_acceptance_death_connection
      {:ok, addr} = TestServer.get_listen_addr(svr)
      task = Task.async(fn -> Rebus.connect(addr, name: name) end)
      assert_receive {^svr, :auth_received}, 1_000

      cli = Process.whereis(name)
      assert is_pid(cli)
      assert :ok = :sys.suspend(cli)
      Process.exit(cli, :kill)

      assert {:error, :killed} = Task.await(task, 1_000)
      assert Process.whereis(name) == nil
    end

    test "correlates Hello replies with the serial sent during setup" do
      {:ok, svr} =
        start_supervised({Rebus.TestServer, tap: self()}, id: :dynamic_hello_serial_server)

      {:ok, addr} = TestServer.get_listen_addr(svr)
      parent = self()
      connect_ref = make_ref()

      waiter =
        spawn(fn ->
          receive do
            {^connect_ref, {:ok, pid}} ->
              send(parent, {:setup_ready, pid})

              receive do
                {^connect_ref, :accepted} -> send(parent, :setup_accepted)
              end
          end
        end)

      {:ok, cli} =
        DynamicSupervisor.start_child(
          Rebus.ConnectionSupervisor,
          {Rebus.Connection, addr: addr, connect_waiter: {waiter, connect_ref}}
        )

      assert_receive {:setup_ready, ^cli}, 1_000
      :sys.replace_state(cli, fn state -> %{state | serial: 42} end)
      send(cli, {connect_ref, :accepted})
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}, serial: 42} = hello}
      handle_hello(hello, svr)
      assert_receive :setup_accepted, 1_000
      assert wait_until(fn -> :sys.get_state(cli).name == ":1.100" end)
    end

    test "stops after an unanswered Hello reply timeout", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      :ok = TestServer.set_auto_hello(svr, false)
      task = Task.async(fn -> Rebus.connect(addr, read_timeout: 300) end)

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
      assert {:error, :read_timeout} = Task.await(task, 1_000)
    end

    test "continues a fragmented authentication response" do
      auth_response = "OK 30313233343536373839414243444546\r\n"

      {:ok, auth_svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           auth_response: auth_response,
           auth_response_fragments: [
             binary_part(auth_response, 0, 8),
             binary_part(auth_response, 8, byte_size(auth_response) - 8)
           ],
           auth_fragment_delay: 50},
          id: :fragmented_auth_server
        )

      {:ok, addr} = TestServer.get_listen_addr(auth_svr)
      assert {:ok, _cli} = Rebus.connect(addr, read_timeout: 300)
      assert_receive {^auth_svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test "does not extend authentication setup past its total read timeout" do
      auth_response = "OK 30313233343536373839414243444546\r\n"

      {:ok, auth_svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           auth_response: auth_response,
           auth_response_fragments: [
             binary_part(auth_response, 0, 1),
             binary_part(auth_response, 1, 1),
             binary_part(auth_response, 2, byte_size(auth_response) - 2)
           ],
           auth_fragment_delay: 300},
          id: :dribbling_auth_server
        )

      {:ok, addr} = TestServer.get_listen_addr(auth_svr)
      assert {:error, :read_timeout} = Rebus.connect(addr, read_timeout: 500)
      assert_receive {^auth_svr, :client_closed}, 1_500
    end

    test "times out after a partial authentication response" do
      {:ok, auth_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), partial_auth: "OK 303132333435"},
          id: :partial_auth_server
        )

      {:ok, addr} = TestServer.get_listen_addr(auth_svr)
      assert {:error, :read_timeout} = Rebus.connect(addr, read_timeout: 150)
      assert_receive {^auth_svr, :auth_received}
    end

    test "rejects an overlong authentication response" do
      auth_response = "OK " <> String.duplicate("a", 1_024) <> "\r\n"

      {:ok, auth_svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), auth_response: auth_response},
          id: :overlong_auth_server
        )

      {:ok, addr} = TestServer.get_listen_addr(auth_svr)
      assert {:error, :auth_failed} = Rebus.connect(addr)
    end

    test "times out after partial Hello reply progress with a protocol reason", %{svr: svr} do
      log =
        capture_log(fn ->
          {cli, hello} =
            connect_until_hello(svr,
              read_timeout: 100,
              auth_id_fun: fn _ -> {:ok, "501\n"} end
            )

          reply =
            Message.new!(:method_return,
              reply_serial: hello.serial,
              signature: "s",
              body: [":1.100"]
            )

          {:ok, encoded} = Message.encode(reply)
          encoded = IO.iodata_to_binary(encoded)
          first = binary_part(encoded, 0, 20)
          ref = Process.monitor(cli)

          :ok = TestServer.push_raw(svr, first)

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :read_timeout}}, 500
        end)

      assert log =~ "D-Bus connection protocol stopped: :read_timeout"
      # Test-server teardown from an earlier test can asynchronously log an
      # unrelated transport close while this capture is active. The contract
      # under test is that this connection classifies its elapsed Hello
      # deadline as a protocol error, not a transport read-timeout.
      refute log =~ "D-Bus connection transport stopped: :read_timeout"
      refute log =~ "%Rebus.Connection"
    end

    test "accepts Hello reply fragments within the total read timeout", %{svr: svr} do
      {cli, hello} = connect_until_hello(svr, read_timeout: 500)

      reply =
        Message.new!(:method_return, reply_serial: hello.serial, signature: "s", body: [":1.100"])

      {:ok, encoded} = Message.encode(reply)
      encoded = IO.iodata_to_binary(encoded)
      first = binary_part(encoded, 0, 20)
      rest = binary_part(encoded, 20, byte_size(encoded) - 20)

      task = Task.async(fn -> TestServer.push_raw_delayed_fragments(svr, [first, rest], 200) end)

      assert wait_until(fn -> :sys.get_state(cli).name == ":1.100" end, 100)
      assert :ok = Task.await(task)
      assert :sys.get_state(cli).partial_frame_timer == nil
      assert Process.alive?(cli)
    end

    test "does not extend the Hello setup deadline when a peer dribbles bytes", %{svr: svr} do
      {cli, hello} = connect_until_hello(svr, read_timeout: 500)

      reply =
        Message.new!(:method_return, reply_serial: hello.serial, signature: "s", body: [":1.100"])

      {:ok, encoded} = Message.encode(reply)

      <<first::binary-size(20), second::binary-size(1), _rest::binary>> =
        IO.iodata_to_binary(encoded)

      ref = Process.monitor(cli)

      task =
        Task.async(fn -> TestServer.push_raw_delayed_fragments(svr, [first, second], 300) end)

      assert :ok = Task.await(task)
      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :read_timeout}}, 700
    end

    test "stops with the parse error reason", %{svr: svr} do
      log =
        capture_log(fn ->
          cli = connect_until_ready(svr)
          ref = Process.monitor(cli)
          :ok = TestServer.push_raw(svr, <<255, 0::size(15 * 8)>>)

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :invalid_endianness}}, 1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: :invalid_endianness"
      refute log =~ "%Rebus.Connection"
    end

    test "stops on a nonempty array of zero-width structs", %{svr: svr} do
      log =
        capture_log(fn ->
          cli = connect_until_ready(svr)
          ref = Process.monitor(cli)

          :ok = TestServer.push_raw(svr, malformed_empty_struct_array_message())

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :invalid_message}}, 1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: :invalid_message"
    end

    test "rejects a fragmented oversized inbound frame before buffering its body", %{svr: svr} do
      log =
        capture_log(fn ->
          cli = connect_until_ready(svr)
          ref = Process.monitor(cli)
          body_length = Message.max_message_size() - 16 + 1

          fixed_header =
            <<?l, 4, 0, 1, body_length::little-32, 1::little-32, 0::little-32>>

          assert {:error, :message_too_large} = Message.expected_size(fixed_header)

          # Only the complete fixed header is sent; no message body is ever
          # placed on the socket before the declared size is rejected.
          :ok = TestServer.push_raw(svr, fixed_header)

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :message_too_large}}, 1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: :message_too_large"
      refute log =~ "%Rebus.Connection"
    end

    test "bounds retained segments for many small incomplete fragments", %{svr: svr} do
      cli = connect_until_ready(svr, read_timeout: 10_000)
      body_length = 1_000_000
      fixed_header = <<?l, 4, 0, 1, body_length::little-32, 1::little-32, 0::little-32>>

      :ok = TestServer.push_raw(svr, fixed_header)
      assert wait_until(fn -> :sys.get_state(cli).inbound_expected_size != nil end)

      assert :ok = TestServer.push_raw_fragments(svr, :binary.copy(<<0>>, 256))

      assert wait_until(fn ->
               :sys.get_state(cli).inbound_size == byte_size(fixed_header) + 256
             end)

      state = :sys.get_state(cli)

      assert state.inbound_size <= byte_size(fixed_header) + 256
      assert state.inbound_expected_size == byte_size(fixed_header) + body_length
      assert length(state.inbound_segments) <= 10
      assert binary_part(inbound_data(state), 0, byte_size(fixed_header)) == fixed_header
    end

    test "caps each inbound receive allocation below the maximum frame size", %{svr: svr} do
      cli = connect_until_ready(svr)

      assert Connection.inbound_receive_buffer_size() == 65_536

      assert {:ok, receive_buffer} = :socket.getopt(:sys.get_state(cli).sock, {:otp, :rcvbuf})

      buffer_size =
        case receive_buffer do
          {1, size} -> size
          size when is_integer(size) -> size
        end

      assert buffer_size == Connection.inbound_receive_buffer_size()
      assert buffer_size < Message.max_message_size()
    end

    test "decodes a one-byte fragmented inbound frame", %{svr: svr} do
      cli = connect_until_ready(svr)
      ref = Rebus.add_signal_handler(cli)

      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Fragmented",
          signature: "s",
          body: ["delivered one byte at a time"]
        )

      {:ok, encoded} = Message.encode(message)
      assert :ok = TestServer.push_raw_fragments(svr, IO.iodata_to_binary(encoded))

      assert_receive {^ref,
                      %Message{
                        header_fields: %{member: "Fragmented"},
                        body: ["delivered one byte at a time"]
                      }},
                     1_000

      assert wait_until(fn -> :sys.get_state(cli).inbound_size == 0 end)
      assert :sys.get_state(cli).partial_frame_timer == nil
    end

    test "keeps an idle connection alive without an incomplete frame", %{svr: svr} do
      cli = connect_until_ready(svr, read_timeout: 500)

      Process.sleep(600)

      assert Process.alive?(cli)
      assert :sys.get_state(cli).partial_frame_timer == nil
    end

    test "stops a stalled fragmented inbound frame after the read timeout", %{svr: svr} do
      log =
        capture_log(fn ->
          cli = connect_until_ready(svr, read_timeout: 500)
          ref = Process.monitor(cli)
          partial_header = <<?l, 4, 0, 1, 0::little-32, 1::little-32>>

          :ok = TestServer.push_raw(svr, partial_header)

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :read_timeout}}, 1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: :read_timeout"
      refute log =~ "%Rebus.Connection"
    end

    test "resets the partial-frame deadline when a peer makes progress", %{svr: svr} do
      cli = connect_until_ready(svr, read_timeout: 1_000)

      message =
        Message.new!(:signal,
          path: "/test",
          interface: "test.interface",
          member: "Fragmented"
        )

      {:ok, encoded} = Message.encode(message)
      binary = IO.iodata_to_binary(encoded)
      <<first::binary-size(12), second::binary-size(1), rest::binary>> = binary

      assert :ok = TestServer.push_raw(svr, first)

      assert wait_until(fn ->
               %Connection{inbound_size: size, partial_frame_timer: timer} = :sys.get_state(cli)
               size == byte_size(first) and timer != nil
             end)

      first_timer = :sys.get_state(cli).partial_frame_timer

      assert :ok = TestServer.push_raw(svr, second)

      assert wait_until(fn ->
               %Connection{inbound_size: size, partial_frame_timer: timer} = :sys.get_state(cli)

               size == byte_size(first) + byte_size(second) and timer != nil and
                 timer != first_timer
             end)

      second_timer = :sys.get_state(cli).partial_frame_timer
      {_timer_ref, stale_token} = first_timer
      send(cli, {:partial_frame_timeout, stale_token})

      assert wait_until(fn -> :sys.get_state(cli).partial_frame_timer == second_timer end)
      assert Process.alive?(cli)

      assert :ok = TestServer.push_raw(svr, rest)
      assert wait_until(fn -> :sys.get_state(cli).inbound_size == 0 end)
      assert :sys.get_state(cli).partial_frame_timer == nil
    end
  end

  describe "signal handler callbacks" do
    test "ignores unrecognised events and replies to handler calls" do
      state = {self(), self(), make_ref()}

      assert {:ok, ^state} = SignalHandler.handle_event(:unrecognised, state)
      assert {:ok, ^state} = SignalHandler.handle_info(:unrecognised, state)
      assert {:ok, :ok, ^state} = SignalHandler.handle_call(:status, state)
    end
  end

  describe "Connection callbacks" do
    test "allocates serials within a bounded range" do
      assert {:ok, 2} = Connection.allocate_serial(1, %{1 => :pending}, 2)
      assert {:ok, 1} = Connection.allocate_serial(2, %{2 => :pending}, 2)

      assert {:error, :serial_exhausted} =
               Connection.allocate_serial(1, %{1 => :pending, 2 => :pending}, 2)
    end

    test "classifies socket send results without exposing payloads" do
      assert :ok = Connection.classify_send_result(:ok, 3)
      assert {:error, :timeout} = Connection.classify_send_result({:error, {:timeout, "abc"}}, 3)

      assert {:error, {:send_fatal, :timeout}} =
               Connection.classify_send_result({:error, {:timeout, "a"}}, 3)

      assert {:error, {:send_fatal, :closed}} =
               Connection.classify_send_result({:error, {:closed, "abc"}}, 3)

      assert {:error, {:send_fatal, :closed}} =
               Connection.classify_send_result({:error, :closed}, 3)

      assert {:error, {:send_fatal, :timeout}} =
               Connection.classify_send_result({:error, {:timeout, %{}}}, 3)

      assert {:error, {:send_fatal, :send_failed}} =
               Connection.classify_send_result({:error, {"weird", "abc"}}, 3)

      assert {:continue, "bc"} = Connection.classify_send_result({:ok, "bc"}, 3)

      assert {:error, {:send_fatal, :send_failed}} =
               Connection.classify_send_result({:ok, ["bc"]}, 3)

      assert {:error, {:send_fatal, :timeout}} =
               Connection.classify_send_result({:error, {:timeout, "bc"}}, 3)

      select_info = {:select_info, :send, make_ref()}
      completion_info = {:completion_info, :send, make_ref()}

      assert {:select, ^select_info, nil} =
               Connection.classify_send_result({:select, select_info}, 3)

      assert {:select, ^select_info, "bc"} =
               Connection.classify_send_result({:select, {select_info, "bc"}}, 3)

      assert {:completion, ^completion_info} =
               Connection.classify_send_result({:completion, completion_info}, 3)

      assert {:error, {:send_fatal, :send_failed}} =
               Connection.classify_send_result({:unexpected, :socket_shape}, 3)
    end

    test "builds nonblocking socket continuation arguments" do
      continuation = {:select_info, :send, make_ref()}
      assert {"rest", [], :nowait} = Connection.socket_send_args("rest", nil)

      assert {"rest", ^continuation, :nowait} =
               Connection.socket_send_args("rest", {:continue, continuation})
    end

    test "falls back safely when configuring the OTP receive buffer" do
      parent = self()
      tuple_value = {1, Connection.inbound_receive_buffer_size()}

      assert :tuple =
               Connection.configure_receive_buffer(
                 :test_socket,
                 fn _sock, _option, value ->
                   send(parent, {:setopt, value})
                   :ok
                 end,
                 fn warning -> send(parent, {:warning, warning}) end
               )

      assert_receive {:setopt, ^tuple_value}
      refute_receive {:warning, _}

      assert :scalar =
               Connection.configure_receive_buffer(
                 :test_socket,
                 fn _sock, _option, value ->
                   send(parent, {:setopt, value})
                   if is_tuple(value), do: {:error, :invalid}, else: :ok
                 end,
                 fn warning -> send(parent, {:warning, warning}) end
               )

      assert_receive {:setopt, ^tuple_value}
      assert_receive {:setopt, 65_536}
      refute_receive {:warning, _}

      assert :default =
               Connection.configure_receive_buffer(
                 :test_socket,
                 fn _sock, _option, value ->
                   send(parent, {:setopt, value})
                   {:error, :invalid}
                 end,
                 fn warning -> send(parent, {:warning, warning}) end
               )

      assert_receive {:setopt, ^tuple_value}
      assert_receive {:setopt, 65_536}
      assert_receive {:warning, "D-Bus connection is using OTP's default receive buffer"}

      assert :default =
               Connection.configure_receive_buffer(
                 :test_socket,
                 fn _sock, _option, value ->
                   send(parent, {:setopt, value})
                   :unexpected
                 end,
                 fn warning -> send(parent, {:warning, warning}) end
               )

      assert_receive {:setopt, ^tuple_value}
      refute_receive {:setopt, 65_536}
      assert_receive {:warning, "D-Bus connection is using OTP's default receive buffer"}

      assert :default =
               Connection.configure_receive_buffer(
                 :test_socket,
                 fn _sock, _option, value ->
                   send(parent, {:setopt, value})
                   if is_tuple(value), do: {:error, :invalid}, else: :unexpected
                 end,
                 fn warning -> send(parent, {:warning, warning}) end
               )

      assert_receive {:setopt, ^tuple_value}
      assert_receive {:setopt, 65_536}
      assert_receive {:warning, "D-Bus connection is using OTP's default receive buffer"}
    end

    test "uses the production warning callback for receive-buffer fallback" do
      log =
        capture_log(fn ->
          assert :default =
                   Connection.configure_receive_buffer(:test_socket, fn _sock, _option, _value ->
                     {:error, :invalid}
                   end)
        end)

      assert log =~ "D-Bus connection is using OTP's default receive buffer"
      refute log =~ "test_socket"
    end

    test "rejects a pathological inbound segment count without logging data" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)

      segments =
        Enum.map(64..1//-1, fn size ->
          {size, :binary.copy(<<0>>, size)}
        end)

      state = %Connection{
        sock: sock,
        inbound_segments: segments,
        inbound_size: Enum.sum(Enum.map(segments, &elem(&1, 0)))
      }

      log =
        capture_log(fn ->
          assert {:stop, {:shutdown, :message_too_large}, ^state} =
                   Connection.append_inbound_fragment("sensitive peer payload", state, :recv)
        end)

      assert log =~ "D-Bus connection protocol stopped: :message_too_large"
      refute log =~ "sensitive peer payload"
      refute log =~ "%Rebus.Connection"
      _ = :socket.close(sock)
    end

    test "waits for a select after handling its attached receive data" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)

      message =
        Message.new!(:method_call, path: "/", interface: "org.example.Test", member: "Select")

      {:ok, encoded} = Message.encode(message)
      data = IO.iodata_to_binary(encoded)
      select_info = {:select_info, :recv, make_ref()}
      {:select_info, :recv, handle} = select_info
      state = %Connection{sock: sock, name: ":1.100"}

      assert {:noreply,
              %Connection{rref: ^handle, inbound_size: 0, partial_frame_timer: nil} = state} =
               Connection.handle_receive_result({:select, {select_info, data}}, state)

      assert {:noreply, ^state} = Connection.handle_continue(:recv, state)
      Connection.terminate(:normal, state)
    end

    test "parses coalesced frames with one materialization per receive" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)

      message =
        Message.new!(:method_call, path: "/", interface: "org.example.Test", member: "Coalesced")

      {:ok, encoded} = Message.encode(message)
      data = :binary.copy(IO.iodata_to_binary(encoded), 1_000)
      handle = make_ref()

      state = %Connection{
        sock: sock,
        name: ":1.100",
        rref: {:completion, handle}
      }

      assert {:noreply, %Connection{inbound_size: 0, inbound_flatten_count: 1},
              {:continue, :recv}} =
               Connection.handle_info(
                 {:"$socket", sock, :completion, {handle, {:ok, data}}},
                 state
               )

      _ = :socket.close(sock)
    end

    test "ignores stale partial-frame timeout tokens" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      current_token = make_ref()
      state = %Connection{sock: sock, partial_frame_timer: {make_ref(), current_token}}

      assert {:noreply, ^state} =
               Connection.handle_info({:partial_frame_timeout, make_ref()}, state)

      _ = :socket.close(sock)
    end

    test "normalizes only partial socket errors" do
      assert :closed == Connection.normalize_socket_error({:closed, "partial peer data"})
      assert :closed == Connection.normalize_socket_error({:closed, ["partial", ?\n]})

      non_partial = {:closed, %{details: "not peer data"}}
      non_atom_reason = {"closed", "partial-looking data"}

      assert non_partial == Connection.normalize_socket_error(non_partial)
      assert non_atom_reason == Connection.normalize_socket_error(non_atom_reason)
      assert :closed == Connection.normalize_socket_error(:closed)
    end

    test "uses a data-free fallback for unrecognized protocol errors" do
      assert :protocol_error ==
               Connection.sanitize_protocol_reason({:unexpected_handshake_message, "sensitive"})

      assert :protocol_error == Connection.sanitize_protocol_reason(["sensitive", "body"])

      assert {:malformed_reply, :missing_reply_serial} ==
               Connection.sanitize_protocol_reason({:malformed_reply, :missing_reply_serial})
    end

    test "logs and stops for an owned socket abort" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      handle = make_ref()
      state = %Connection{sock: sock, rref: handle}

      log =
        capture_log(fn ->
          assert {:stop, {:shutdown, :closed}, ^state} =
                   Connection.handle_info(
                     {:"$socket", sock, :abort, {handle, {:closed, "sensitive partial data"}}},
                     state
                   )
        end)

      refute log =~ "sensitive partial data"
      assert log =~ "D-Bus connection transport stopped: :closed"
      _ = :socket.close(sock)
    end

    test "handles completion-based reads without leaking buffered state" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)

      message =
        Message.new!(:method_call, path: "/", interface: "org.example.Test", member: "Read")

      {:ok, encoded} = Message.encode(message)
      data = IO.iodata_to_binary(encoded)
      complete_handle = make_ref()

      complete_state = %Connection{
        sock: sock,
        name: ":1.100",
        rref: {:completion, complete_handle}
      }

      assert {:noreply, %Connection{rref: nil, inbound_size: 0}, {:continue, :recv}} =
               Connection.handle_info(
                 {:"$socket", sock, :completion, {complete_handle, {:ok, data}}},
                 complete_state
               )

      partial_handle = make_ref()
      partial_state = %Connection{sock: sock, rref: {:completion, partial_handle}}

      assert {:noreply, %Connection{rref: nil, inbound_size: 15} = buffered, {:continue, :recv}} =
               Connection.handle_info(
                 {:"$socket", sock, :completion,
                  {partial_handle, {:ok, binary_part(data, 0, 15)}}},
                 partial_state
               )

      assert buffered.partial_frame_timer != nil
      Connection.terminate(:normal, buffered)
    end

    test "stops cleanly for completion read errors and aborts" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      error_handle = make_ref()
      error_state = %Connection{sock: sock, rref: {:completion, error_handle}}

      error_log =
        capture_log(fn ->
          assert {:stop, {:shutdown, :closed}, %Connection{rref: nil}} =
                   Connection.handle_info(
                     {:"$socket", sock, :completion,
                      {error_handle, {:error, {:closed, "sensitive partial data"}}}},
                     error_state
                   )
        end)

      refute error_log =~ "sensitive partial data"
      refute error_log =~ "%Rebus.Connection"

      abort_handle = make_ref()
      abort_state = %Connection{sock: sock, rref: {:completion, abort_handle}}

      abort_log =
        capture_log(fn ->
          assert {:stop, {:shutdown, :closed}, ^abort_state} =
                   Connection.handle_info(
                     {:"$socket", sock, :abort,
                      {abort_handle, {:closed, "sensitive partial data"}}},
                     abort_state
                   )
        end)

      refute abort_log =~ "sensitive partial data"
      refute abort_log =~ "%Rebus.Connection"

      _ = :socket.close(sock)
    end

    test "ignores arbitrary info messages" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      state = %Connection{sock: sock}

      assert {:noreply, ^state} = Connection.handle_info({:unexpected, "message"}, state)
      _ = :socket.close(sock)
    end

    test "ignores a stray process down message" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      state = %Connection{sock: sock}

      assert {:noreply, ^state} =
               Connection.handle_info({:DOWN, make_ref(), :process, self(), :normal}, state)

      _ = :socket.close(sock)
    end

    test "Hello callbacks stop cleanly when the socket is closed" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      :ok = :socket.close(sock)
      state = %Connection{sock: sock}

      assert {:stop, {:shutdown, :closed}, ^state} = Connection.handle_continue(:hello, state)

      assert {:stop, {:shutdown, :closed}, ^state} =
               Connection.handle_continue(:hello_reply, state)
    end

    test "logs a transport stop when the peer closes after authentication" do
      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer, tap: self(), close_after_begin: true},
          id: :server_closes_after_begin
        )

      {:ok, addr} = TestServer.get_listen_addr(svr)

      log =
        capture_log(fn ->
          assert {:error, reason} = Rebus.connect(addr)
          assert reason in [:closed, :econnreset]
        end)

      assert log =~ ~r/D-Bus connection transport stopped: :(closed|econnreset)/
    end

    test "sanitizes a fragmented Hello error without logging buffered data" do
      error_reply =
        Message.new!(:error,
          error_name: "org.example.SecretError",
          reply_serial: 1,
          signature: "s",
          body: ["sensitive prefix and body"]
        )

      {:ok, encoded} = Message.encode(error_reply)
      encoded = IO.iodata_to_binary(encoded)
      sensitive_prefix = "sensitive prefix"
      {prefix_start, _length} = :binary.match(encoded, sensitive_prefix)
      split_size = prefix_start + byte_size(sensitive_prefix)
      first = binary_part(encoded, 0, split_size)
      second = binary_part(encoded, split_size, byte_size(encoded) - split_size)

      {sock, server} = start_fragmented_socket_server(first)
      state = %Connection{sock: sock, hello_serial: 1}

      log =
        capture_log(fn ->
          task = Task.async(fn -> Connection.handle_continue(:hello_reply, state) end)
          send(server.pid, {:send_remainder, second})

          assert {:stop, {:shutdown, {:hello_failed, "org.example.SecretError"}},
                  %Connection{inbound_size: 0, inbound_segments: [], partial_frame_timer: nil}} =
                   Task.await(task)
        end)

      refute log =~ sensitive_prefix
      refute log =~ "sensitive prefix and body"
      refute log =~ "%Rebus.Connection"

      assert log =~
               "D-Bus connection protocol stopped: {:hello_failed, \"org.example.SecretError\"}"

      assert protocol_stop_log_count(log) == 1

      _ = :socket.close(sock)
      send(server.pid, :close)
      Task.await(server)
    end

    test "parses a buffered post-auth Hello reply before receiving" do
      hello_reply =
        Message.new!(:method_return, reply_serial: 1, signature: "s", body: [":1.100"])

      {:ok, encoded} = Message.encode(hello_reply)
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      :ok = :socket.close(sock)
      data = IO.iodata_to_binary(encoded)

      state = %Connection{
        sock: sock,
        hello_serial: 1,
        inbound_segments: [{byte_size(data), data}],
        inbound_size: byte_size(data)
      }

      assert {:noreply, %Connection{name: ":1.100", inbound_size: 0}, {:continue, :recv}} =
               Connection.handle_continue(:hello_reply_buffer, state)
    end
  end

  describe "Hello replies" do
    setup [:server_setup]

    test "returns a data-free reason for an error reply", %{svr: svr} do
      log =
        capture_log(fn ->
          {cli, hello} = connect_until_hello(svr)

          error_reply =
            Message.new!(:error,
              error_name: "org.example.SecretError",
              reply_serial: hello.serial,
              signature: "s",
              body: ["sensitive error body"]
            )

          {:ok, encoded} = Message.encode(error_reply)
          ref = Process.monitor(cli)

          :ok = TestServer.push_raw(svr, IO.iodata_to_binary(encoded) <> "sensitive remainder")

          assert_receive {:DOWN, ^ref, :process, ^cli,
                          {:shutdown, {:hello_failed, "org.example.SecretError"}}},
                         1_000
        end)

      refute log =~ "sensitive error body"
      refute log =~ "sensitive remainder"
      refute log =~ "%Rebus.Connection"

      assert log =~
               "D-Bus connection protocol stopped: {:hello_failed, \"org.example.SecretError\"}"
    end

    test "returns a copied valid Hello error name from a large frame", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      :ok = TestServer.set_auto_hello(svr, false)
      connect_task = Task.async(fn -> Rebus.connect(addr) end)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}

      error_name = "org.example.LargeReply"
      payload = String.duplicate("sensitive hello error body ", 10_000)

      error_reply =
        Message.new!(:error,
          error_name: error_name,
          reply_serial: hello.serial,
          signature: "s",
          body: [payload]
        )

      {:ok, encoded} = Message.encode(error_reply)

      log =
        capture_log(fn ->
          :ok = TestServer.push_raw(svr, IO.iodata_to_binary(encoded))

          assert {:error, {:hello_failed, returned_name}} = Task.await(connect_task, 1_000)
          assert returned_name == error_name
          assert :binary.referenced_byte_size(returned_name) <= @max_copied_referenced_bytes
        end)

      refute log =~ payload
    end

    test "classifies a missing error name", %{svr: svr} do
      log =
        assert_hello_error_reason(svr, :invalid_message, fn reply_serial ->
          raw_error_reply_binary(reply_serial, %{})
        end)

      assert log =~ "D-Bus connection protocol stopped: :invalid_message"
    end

    test "classifies an invalid error name without logging it", %{svr: svr} do
      invalid_name = "invalid error name"

      log =
        assert_hello_error_reason(svr, :invalid_message, fn reply_serial ->
          raw_error_reply_binary(reply_serial, %{error_name: invalid_name})
        end)

      refute log =~ invalid_name
      assert log =~ "D-Bus connection protocol stopped: :invalid_message"
    end

    test "classifies an oversized error name without logging it", %{svr: svr} do
      oversized_name = "org." <> String.duplicate("a", 252)

      log =
        assert_hello_error_reason(svr, :invalid_error_name, fn reply_serial ->
          raw_error_reply(reply_serial, %{error_name: oversized_name})
        end)

      refute log =~ oversized_name
      assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :invalid_error_name}"
    end

    test "returns a clean error for an empty Hello reply", %{svr: svr} do
      {cli, hello} = connect_until_hello(svr)
      empty_reply = Message.new!(:method_return, reply_serial: hello.serial)
      ref = Process.monitor(cli)

      log =
        capture_log(fn ->
          :ok = TestServer.push(svr, empty_reply)

          assert_receive {:DOWN, ^ref, :process, ^cli,
                          {:shutdown, {:hello_failed, :missing_unique_name}}},
                         1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :missing_unique_name}"
      refute log =~ "%Rebus.Connection"
    end

    test "rejects invalid unique names without retaining peer input" do
      invalid_names = [
        {:missing_prefix, "1.100"},
        {:empty_element, ":1..100"},
        {:invalid_character, ":1.10/0"},
        {:oversized, ":" <> String.duplicate("a", 253) <> ".a"}
      ]

      Enum.each(invalid_names, fn {kind, invalid_name} ->
        {:ok, invalid_svr} =
          start_supervised(
            {Rebus.TestServer, tap: self()},
            id: {:invalid_unique_name_server, kind}
          )

        log =
          capture_log(fn ->
            {cli, hello} = connect_until_hello(invalid_svr)
            ref = Process.monitor(cli)

            :ok =
              TestServer.push(
                invalid_svr,
                Message.new!(:method_return,
                  reply_serial: hello.serial,
                  signature: "s",
                  body: [invalid_name]
                )
              )

            assert_receive {:DOWN, ^ref, :process, ^cli,
                            {:shutdown, {:hello_failed, :invalid_unique_name}}},
                           1_000
          end)

        refute log =~ invalid_name
        assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :invalid_unique_name}"
      end)
    end

    test "accepts the longest valid unique name", %{svr: svr} do
      valid_name = ":" <> String.duplicate("a", 252) <> ".a"
      assert byte_size(valid_name) == 255

      {cli, hello} = connect_until_hello(svr)

      :ok =
        TestServer.push(
          svr,
          Message.new!(:method_return,
            reply_serial: hello.serial,
            signature: "s",
            body: [valid_name]
          )
        )

      assert wait_until(fn -> :sys.get_state(cli).name == valid_name end)
      assert :sys.get_state(cli).established?
      assert :ok = Rebus.close(cli)
    end

    test "copies retained auth and Hello identifiers from large peer input" do
      guid = "30313233343536373839414243444546"
      unique_name = ":1.100"

      hello_reply =
        Message.new!(:method_return,
          reply_serial: 1,
          signature: "ss",
          body: [unique_name, String.duplicate("x", 262_144)]
        )

      {:ok, encoded_hello_reply} = Message.encode(hello_reply)

      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           auto_hello: false,
           auth_response: "OK " <> guid <> "\r\n" <> IO.iodata_to_binary(encoded_hello_reply)},
          id: :retained_peer_identifier_server
        )

      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:ok, cli} = Rebus.connect(addr)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}

      state = :sys.get_state(cli)
      assert state.guid == guid
      assert :binary.referenced_byte_size(state.guid) <= @max_copied_referenced_bytes
      assert state.name == unique_name
      assert :binary.referenced_byte_size(state.name) <= @max_copied_referenced_bytes
      assert :ok = Rebus.close(cli)
    end

    test "returns a stable setup error for an invalid unique name", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      :ok = TestServer.set_auto_hello(svr, false)
      connect_task = Task.async(fn -> Rebus.connect(addr) end)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}

      log =
        capture_log(fn ->
          :ok =
            TestServer.push(
              svr,
              Message.new!(:method_return,
                reply_serial: hello.serial,
                signature: "s",
                body: [":1..100"]
              )
            )

          assert {:error, {:hello_failed, :invalid_unique_name}} = Task.await(connect_task, 1_000)
        end)

      refute log =~ ":1..100"
    end

    test "returns the unexpected message type without its payload", %{svr: svr} do
      {cli, hello} = connect_until_hello(svr)

      unexpected_reply =
        Message.new!(:method_return,
          reply_serial: hello.serial + 1,
          signature: "s",
          body: ["sensitive unexpected body"]
        )

      ref = Process.monitor(cli)
      :ok = TestServer.push(svr, unexpected_reply)

      assert_receive {:DOWN, ^ref, :process, ^cli,
                      {:shutdown, {:unexpected_handshake_message, :method_return}}},
                     1_000
    end

    test "rejects a signal before the Hello reply", %{svr: svr} do
      {cli, _hello} = connect_until_hello(svr)

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "BeforeHello"
        )

      assert_unexpected_handshake_message(svr, cli, signal, :signal)
    end

    test "rejects a method call before the Hello reply", %{svr: svr} do
      {cli, _hello} = connect_until_hello(svr)

      method_call =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "BeforeHello"
        )

      assert_unexpected_handshake_message(svr, cli, method_call, :method_call)
    end

    test "drains a coalesced method call and signal after a Hello reply", %{svr: svr} do
      {cli, hello} = connect_until_hello(svr)
      ref = make_ref()

      :ok =
        :gen_event.add_sup_handler(
          SignalHandler,
          {SignalHandler, ref},
          {cli, self(), ref}
        )

      on_exit(fn -> :gen_event.delete_handler(SignalHandler, {SignalHandler, ref}, nil) end)

      hello_reply =
        Message.new!(:method_return,
          reply_serial: hello.serial,
          signature: "s",
          body: [":1.100"]
        )

      method_call =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "UnsupportedInboundCall"
        )

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "CoalescedSignal",
          signature: "s",
          body: ["delivered without another write"]
        )

      {:ok, hello_data} = Message.encode(hello_reply)
      {:ok, method_call_data} = Message.encode(method_call)
      {:ok, signal_data} = Message.encode(signal)

      :ok =
        TestServer.push_raw(
          svr,
          IO.iodata_to_binary([hello_data, method_call_data, signal_data])
        )

      assert_receive {^ref, %Message{type: :signal, body: ["delivered without another write"]}},
                     1_000

      assert wait_until(fn -> :sys.get_state(cli).name == ":1.100" end)
      assert 0 == :sys.get_state(cli).inbound_size
    end

    test "drains a large coalesced signal burst before a partial tail", %{svr: svr} do
      cli = connect_until_ready(svr)
      ref = Rebus.add_signal_handler(cli)

      messages =
        for index <- 1..101 do
          Message.new!(:signal,
            path: "/test",
            interface: "test.interface",
            member: "Burst",
            signature: "u",
            body: [index]
          )
        end

      encoded =
        Enum.map(messages, fn message ->
          {:ok, data} = Message.encode(message)
          IO.iodata_to_binary(data)
        end)

      {complete, [tail]} = Enum.split(encoded, 100)
      split_at = byte_size(tail) - 3
      partial_tail = binary_part(tail, 0, split_at)
      remainder = binary_part(tail, split_at, byte_size(tail) - split_at)

      :ok = TestServer.push_raw(svr, IO.iodata_to_binary([complete, partial_tail]))

      for index <- 1..100 do
        assert_receive {^ref, %Message{header_fields: %{member: "Burst"}, body: [^index]}},
                       1_000
      end

      state = :sys.get_state(cli)
      assert state.inbound_size < state.inbound_expected_size
      assert length(state.inbound_segments) <= 10

      :ok = TestServer.push_raw(svr, remainder)

      assert_receive {^ref, %Message{header_fields: %{member: "Burst"}, body: [101]}},
                     1_000
    end
  end

  describe "Connection receive callbacks" do
    setup [:server_setup]

    test "survives an inbound method call", %{svr: svr} do
      cli = connect_until_ready(svr)
      ref = Rebus.add_signal_handler(cli)

      method_call =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "UnsupportedInboundCall"
        )

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "MethodCallSurvived",
          signature: "s",
          body: ["connection survived"]
        )

      :ok = TestServer.push(svr, method_call)
      :ok = TestServer.push(svr, signal)

      assert_receive {^ref, %Message{type: :signal, body: ["connection survived"]}}, 1_000
      assert Process.alive?(cli)
    end

    test "stops cleanly for a method return without reply serial", %{svr: svr} do
      assert_missing_reply_serial(svr, :method_return)
    end

    test "stops cleanly for an error without reply serial", %{svr: svr} do
      assert_missing_reply_serial(svr, :error)
    end

    test "does not set a name from an empty steady reply", %{svr: svr} do
      cli = connect_until_ready(svr, send_name_acquired?: false)
      :ok = :sys.suspend(cli)
      _ = :sys.replace_state(cli, fn state -> %{state | name: nil, hello_serial: 1} end)
      :ok = :sys.resume(cli)

      empty_reply = Message.new!(:method_return, reply_serial: 1)
      ref = Process.monitor(cli)

      log =
        capture_log(fn ->
          :ok = TestServer.push(svr, empty_reply)

          assert_receive {:DOWN, ^ref, :process, ^cli,
                          {:shutdown, {:hello_failed, :missing_unique_name}}},
                         1_000
        end)

      assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :missing_unique_name}"
      refute log =~ "%Rebus.Connection"
    end
  end

  describe "Unix socket connections" do
    test "can be established with unix socket" do
      # Use a short path to avoid Unix socket path length limit (108 bytes)
      socket_path = socket_path()

      {:ok, svr} =
        start_supervised({Rebus.TestServer, tap: self(), family: :local, path: socket_path})

      {:ok, _cli} = Rebus.connect(%{family: :local, path: socket_path})

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end
  end

  describe "Connection address parsing" do
    test ":system connects through a configured Unix pathname address" do
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})
      put_system_bus_address("unix:path=#{path},guid=#{@test_bus_guid}")

      assert {:ok, _cli} = Rebus.connect(:system)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":system returns error when address is nil" do
      put_system_bus_address(nil)

      assert {:error, :no_system_bus_address} = Rebus.connect(:system)
    end

    test ":session connects through a Unix pathname with a guid suffix" do
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})
      put_session_bus_address("unix:path=#{path},guid=#{@test_bus_guid}")

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session ignores caller-supplied address-list setup internals" do
      parent = self()
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})
      put_session_bus_address("unix:path=#{path}")

      assert {:ok, _cli} =
               Rebus.connect(:session,
                 auth_id: "not-an-auth-id",
                 auth_id_fun: fn _timeout ->
                   send(parent, :malicious_list_auth_id_fun)
                   {:error, :exit_status}
                 end,
                 address_list_auth_id: "not-an-auth-id",
                 address_list_setup_timeout: 1,
                 expected_guid: String.duplicate("f", 32),
                 precomputed_auth_id: "not-an-auth-id"
               )

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
      refute_receive :malicious_list_auth_id_fun
    end

    test ":session verifies a Unix socket guid without adding it to the socket address" do
      expected_guid = "ABCDEF0123456789ABCDEF0123456789"
      path = socket_path()

      {:ok, svr} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(),
           family: :local,
           path: path,
           auth_response: "OK abcdef0123456789abcdef0123456789\r\n"}
        )

      put_session_bus_address("unix:path=#{path},guid=#{expected_guid}")

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session fails closed on a guid mismatch without logging the configured value" do
      expected_guid = String.duplicate("f", 32)
      mismatched_path = socket_path()
      fallback_path = socket_path()

      {:ok, mismatched} =
        start_supervised(
          {Rebus.TestServer,
           tap: self(), family: :local, path: mismatched_path, notify_auth: true},
          id: :issue_14_guid_mismatch
        )

      {:ok, fallback} =
        start_supervised(
          {Rebus.TestServer, tap: self(), family: :local, path: fallback_path},
          id: :issue_14_guid_mismatch_fallback
        )

      put_session_bus_address(
        "unix:path=#{mismatched_path},guid=#{expected_guid};unix:path=#{fallback_path}"
      )

      log =
        capture_log(fn ->
          assert {:error, :guid_mismatch} = Rebus.connect(:session)
        end)

      assert_receive {^mismatched, :auth_received}
      refute_receive {^fallback, %Message{header_fields: %{member: "Hello"}}}, 100
      refute log =~ expected_guid
    end

    test ":session skips a parameterless unsupported transport but rejects known empty forms" do
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})

      put_session_bus_address("autolaunch:;unix:path=#{path}")

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}

      put_session_bus_address("unix:;unix:path=#{path}")
      assert {:error, {:invalid_bus_address, :missing_path}} = Rebus.connect(:session)

      put_session_bus_address("unix:guid=#{@test_bus_guid};unix:path=#{path}")
      assert {:error, {:invalid_bus_address, :missing_path}} = Rebus.connect(:session)

      put_session_bus_address("tcp:family=ipv4;unix:path=#{path}")
      assert {:error, {:invalid_bus_address, :missing_host}} = Rebus.connect(:session)
    end

    test ":session connects through a TCP address" do
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self()})
      {:ok, %{port: port}} = TestServer.get_listen_addr(svr)
      put_session_bus_address("tcp:host=127.0.0.1,port=#{port}")

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session falls back to the next supported address in order" do
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self()})
      {:ok, %{port: port}} = TestServer.get_listen_addr(svr)

      put_session_bus_address(
        "nonce-tcp:noncefile=/tmp/nonce;unix:path=/definitely/not/a/bus;tcp:host=127.0.0.1,port=#{port}"
      )

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session falls back from unimplemented Unix forms" do
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})

      put_session_bus_address(
        "unix:runtime=/run/user/1000;unix:tmpdir=/tmp;unix:dir=/tmp;unix:path=#{path}"
      )

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session ignores a forward-compatible Unix parameter" do
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})
      put_session_bus_address("unix:path=#{path},future=option")

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session falls back after a TCP connection failure" do
      path = socket_path()
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})
      put_session_bus_address("tcp:host=127.0.0.1,port=1;unix:path=#{path}")

      assert {:ok, _cli} = Rebus.connect(:session)
      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end

    test ":session returns the final supported connection failure" do
      put_session_bus_address("unix:path=/definitely/not/a/bus;nonce-tcp:noncefile=/tmp/nonce")

      assert {:error, :enoent} = Rebus.connect(:session)
    end

    test ":session does not fall back for invalid connection options" do
      put_session_bus_address("unix:path=/definitely/not/a/bus;unix:path=/also/not/a/bus")

      assert {:error, :invalid_timeout} = Rebus.connect(:session, timeout: 0)
    end

    if :os.type() == {:unix, :linux} do
      test ":session connects through a Unix abstract address" do
        assert :os.type() == {:unix, :linux}
        abstract = "rebus_#{System.unique_integer([:positive])}"
        path = <<0, abstract::binary>>
        {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: path})
        put_session_bus_address("unix:abstract=#{abstract},guid=#{@test_bus_guid}")

        assert {:ok, _cli} = Rebus.connect(:session)
        assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
      end

      test ":session connects through an IPv6 TCP address" do
        assert :os.type() == {:unix, :linux}
        {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :inet6})
        {:ok, %{port: port}} = TestServer.get_listen_addr(svr)
        put_session_bus_address("tcp:host=%3A%3A1,port=#{port},family=ipv6")

        assert {:ok, _cli} = Rebus.connect(:session)
        assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
      end
    end

    test ":session returns bounded errors without logging the address payload" do
      sentinel = "session-address-payload-sentinel"
      put_session_bus_address("unix:path=/tmp/%00#{sentinel}")

      log =
        capture_log(fn ->
          assert {:error, {:invalid_bus_address, :nul_byte}} = Rebus.connect(:session)
        end)

      refute log =~ sentinel
    end

    test ":session returns an unsupported transport error when none are supported" do
      put_session_bus_address("unix:runtime=/run/user/1000,tmpdir=/tmp,dir=/tmp")

      assert {:error, :unsupported_bus_transport} = Rebus.connect(:session)
    end

    test ":session keeps malformed entries fatal even with a valid fallback" do
      put_session_bus_address("unix:path=/tmp/%;unix:path=/definitely/not/a/bus")

      assert {:error, {:invalid_bus_address, :invalid_escape}} = Rebus.connect(:session)
    end

    test ":session does not log a valid percent-escaped socket path" do
      sentinel = "session-address-log-sentinel"

      escaped_sentinel =
        sentinel
        |> :binary.bin_to_list()
        |> Enum.map_join(fn byte -> "%#{Integer.to_string(byte, 16)}" end)

      put_session_bus_address("unix:path=/tmp/rebus-%0A#{escaped_sentinel}")

      log =
        capture_log([level: :debug], fn ->
          assert {:error, :enoent} = Rebus.connect(:session)
        end)

      assert log =~ "D-Bus address attempt candidate=1 ip=0 transport=unix"
      assert log =~ "reason=enoent"
      refute log =~ sentinel
      refute log =~ "%0A"
    end

    test ":system returns a bounded error for a non-binary configured address" do
      put_system_bus_address(:not_an_address)

      assert {:error, {:invalid_bus_address, :not_binary}} = Rebus.connect(:system)
    end

    test ":session returns error when DBUS_SESSION_BUS_ADDRESS is not set" do
      put_session_bus_address(nil)

      assert {:error, :no_session_bus_address} = Rebus.connect(:session)
    end

    test ":session cleans up a named child when its list setup budget expires" do
      name = :rebus_issue_14_address_list_deadline

      {:ok, first} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :issue_14_first_silent_server
        )

      {:ok, second} =
        start_supervised(
          {Rebus.TestServer, tap: self(), silent_auth: true},
          id: :issue_14_second_silent_server
        )

      {:ok, %{port: first_port}} = TestServer.get_listen_addr(first)
      {:ok, %{port: second_port}} = TestServer.get_listen_addr(second)

      put_session_bus_address(
        "tcp:host=127.0.0.1,port=#{first_port};tcp:host=127.0.0.1,port=#{second_port}"
      )

      started_at = System.monotonic_time(:millisecond)

      assert {:error, {:read_timeout, :read_timeout}} =
               Rebus.connect(:session, timeout: 400, name: name)

      assert System.monotonic_time(:millisecond) - started_at < 800
      assert_receive {^first, :auth_received}, 1_000
      assert_receive {^second, :auth_received}, 1_000
      assert Process.whereis(name) == nil
    end

    test "tries all TCP results before the next D-Bus address in stable order" do
      parent = self()

      resolver = fn _host, family, timeout ->
        send(parent, {:resolved, family, timeout})

        case family do
          :inet6 -> {:ok, [{0, 0, 0, 0, 0, 0, 0, 1}]}
          :inet -> {:ok, [{127, 0, 0, 1}, {127, 0, 0, 1}, {127, 0, 0, 2}]}
        end
      end

      connector = fn address, opts ->
        send(parent, {:connected, address, Keyword.fetch!(opts, :address_list_setup_timeout)})
        {:error, :econnrefused}
      end

      candidates = [
        {:tcp, "example", 12_345, :unspec, nil},
        {:local, "/tmp/fallback", nil}
      ]

      assert {:error, :econnrefused} =
               Rebus.connect_address_candidates(candidates, [timeout: 100],
                 resolver: resolver,
                 connector: connector
               )

      assert_receive {:resolved, :inet6, timeout6}
      assert timeout6 in 1..100
      assert_receive {:resolved, :inet, timeout4}
      assert timeout4 in 1..100
      assert_receive {:connected, %{family: :inet6, addr: {0, 0, 0, 0, 0, 0, 0, 1}}, _}
      assert_receive {:connected, %{family: :inet, addr: {127, 0, 0, 1}}, _}
      assert_receive {:connected, %{family: :inet, addr: {127, 0, 0, 1}}, _}
      assert_receive {:connected, %{family: :inet, addr: {127, 0, 0, 2}}, _}
      assert_receive {:connected, %{family: :local, path: "/tmp/fallback"}, _}
    end

    test "caps resolver results per family without changing their order or duplicates" do
      parent = self()
      Process.put(:bus_address_cap_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_cap_clock) end

      ipv6 = [
        {0, 0, 0, 0, 0, 0, 0, 1},
        {0, 0, 0, 0, 0, 0, 0, 1},
        {0, 0, 0, 0, 0, 0, 0, 2},
        {0, 0, 0, 0, 0, 0, 0, 3}
        | List.duplicate({0, 0, 0, 0, 0, 0, 0, 4}, 196)
      ]

      ipv4 = [
        {127, 0, 0, 1},
        {127, 0, 0, 1},
        {127, 0, 0, 2},
        {127, 0, 0, 3}
        | List.duplicate({127, 0, 0, 4}, 196)
      ]

      resolver = fn _host, family, timeout ->
        send(parent, {:resolved, family, timeout})
        {:ok, if(family == :inet6, do: ipv6, else: ipv4)}
      end

      connector = fn address, opts ->
        send(parent, {:attempted, address, Keyword.fetch!(opts, :address_list_setup_timeout)})
        {:error, :econnrefused}
      end

      assert {:error, :econnrefused} =
               Rebus.connect_address_candidates(
                 [{:tcp, "example", 12_345, :unspec, nil}],
                 [timeout: 5_000],
                 resolver: resolver,
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      assert_receive {:resolved, :inet6, _}
      assert_receive {:resolved, :inet, _}

      for {family, address} <- [
            {:inet6, {0, 0, 0, 0, 0, 0, 0, 1}},
            {:inet6, {0, 0, 0, 0, 0, 0, 0, 1}},
            {:inet6, {0, 0, 0, 0, 0, 0, 0, 2}},
            {:inet6, {0, 0, 0, 0, 0, 0, 0, 3}},
            {:inet, {127, 0, 0, 1}},
            {:inet, {127, 0, 0, 1}},
            {:inet, {127, 0, 0, 2}},
            {:inet, {127, 0, 0, 3}}
          ] do
        assert_receive {:attempted, %{family: ^family, addr: ^address}, timeout}
        assert timeout >= 50
      end

      refute_receive {:attempted, _, _}
    end

    test "resolves the auth ID once in the caller before all address attempts" do
      parent = self()

      resolver = fn _host, :inet, _timeout ->
        {:ok, [{127, 0, 0, 1}, {127, 0, 0, 1}, {127, 0, 0, 2}, {127, 0, 0, 3}]}
      end

      connector = fn address, opts ->
        send(
          parent,
          {:attempted, address, Keyword.fetch!(opts, :address_list_auth_id)}
        )

        if address.family == :local, do: {:ok, self()}, else: {:error, :econnrefused}
      end

      auth_id_runner = fn timeout ->
        send(parent, {:auth_id, self(), timeout})
        {:ok, "501\n"}
      end

      assert {:ok, _pid} =
               Rebus.connect_address_candidates(
                 [
                   {:tcp, "example", 12_345, :inet, nil},
                   {:local, "/tmp/fallback", nil}
                 ],
                 [timeout: 1_000],
                 resolver: resolver,
                 connector: connector,
                 auth_id_runner: auth_id_runner
               )

      assert_receive {:auth_id, auth_id_owner, auth_timeout}
      assert auth_id_owner == self()
      assert auth_timeout in 1..1_000

      for address <- [
            %{family: :inet, addr: {127, 0, 0, 1}, port: 12_345},
            %{family: :inet, addr: {127, 0, 0, 1}, port: 12_345},
            %{family: :inet, addr: {127, 0, 0, 2}, port: 12_345},
            %{family: :inet, addr: {127, 0, 0, 3}, port: 12_345},
            %{family: :local, path: "/tmp/fallback"}
          ] do
        assert_receive {:attempted, ^address, "353031"}
      end

      refute_receive {:auth_id, _, _}
    end

    test "stops before candidate setup when the list auth ID is unavailable" do
      parent = self()

      connector = fn address, _opts ->
        send(parent, {:attempted, address})
        {:ok, self()}
      end

      assert {:error, :auth_id_unavailable} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/never-attempted", nil}],
                 [timeout: 100],
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:error, :exit_status} end
               )

      refute_receive {:attempted, _}
    end

    test "owns the list auth-ID port in the calling process" do
      parent = self()
      executable = System.find_executable("cat")
      assert is_binary(executable)

      task =
        Task.async(fn ->
          Rebus.connect_address_candidates(
            [{:local, "/tmp/never-attempted", nil}],
            [timeout: 5_000],
            connector: fn _address, _opts -> {:ok, self()} end,
            auth_id_runner: fn _timeout ->
              port = Port.open({:spawn_executable, String.to_charlist(executable)}, [:binary])
              send(parent, {:auth_id_port, self(), port})

              receive do
                :continue -> {:ok, "501\n"}
              end
            end
          )
        end)

      assert_receive {:auth_id_port, owner, port}, 1_000
      assert owner == task.pid
      assert Port.info(port) != nil

      _ = Task.shutdown(task, :brutal_kill)
      assert wait_until(fn -> Port.info(port) == nil end)
    end

    test "counts auth lookup and candidate slices against one aggregate deadline" do
      parent = self()
      Process.put(:bus_address_auth_budget_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_auth_budget_clock) end

      auth_id_runner = fn timeout ->
        send(parent, {:auth_slice, timeout})

        Process.put(
          :bus_address_auth_budget_clock,
          Process.get(:bus_address_auth_budget_clock) + timeout
        )

        {:ok, "501\n"}
      end

      connector = fn address, opts ->
        timeout = Keyword.fetch!(opts, :address_list_setup_timeout)
        send(parent, {:candidate_slice, address.path, timeout})

        Process.put(
          :bus_address_auth_budget_clock,
          Process.get(:bus_address_auth_budget_clock) + timeout
        )

        if address.path == "/tmp/second", do: {:ok, self()}, else: {:error, :read_timeout}
      end

      assert {:ok, _pid} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/first", nil}, {:local, "/tmp/second", nil}],
                 [timeout: 60],
                 connector: connector,
                 auth_id_runner: auth_id_runner,
                 monotonic_time: monotonic_time
               )

      assert_receive {:auth_slice, 20}
      assert_receive {:candidate_slice, "/tmp/first", 20}
      assert_receive {:candidate_slice, "/tmp/second", 20}
      assert Process.get(:bus_address_auth_budget_clock) <= 60
    end

    test "keeps tiny aggregate budgets positive and bounded" do
      parent = self()
      Process.put(:bus_address_tiny_budget_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_tiny_budget_clock) end

      auth_id_runner = fn timeout ->
        send(parent, {:tiny_auth_slice, timeout})

        Process.put(
          :bus_address_tiny_budget_clock,
          Process.get(:bus_address_tiny_budget_clock) + timeout
        )

        {:ok, "501\n"}
      end

      connector = fn address, opts ->
        timeout = Keyword.fetch!(opts, :address_list_setup_timeout)
        send(parent, {:tiny_candidate_slice, address.path, timeout})
        {:ok, self()}
      end

      assert {:ok, _pid} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/one", nil}],
                 [timeout: 5],
                 connector: connector,
                 auth_id_runner: auth_id_runner,
                 monotonic_time: monotonic_time
               )

      assert_receive {:tiny_auth_slice, auth_timeout}
      assert auth_timeout > 0
      assert_receive {:tiny_candidate_slice, "/tmp/one", candidate_timeout}
      assert candidate_timeout > 0
      assert Process.get(:bus_address_tiny_budget_clock) <= 5
    end

    test "gives a stalled IPv6 result only its fair slice before trying IPv4" do
      parent = self()
      Process.put(:bus_address_slice_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_slice_clock) end

      resolver = fn _host, family, timeout ->
        send(parent, {:resolved, family, timeout})

        case family do
          :inet6 -> {:ok, [{0, 0, 0, 0, 0, 0, 0, 1}]}
          :inet -> {:ok, [{127, 0, 0, 1}]}
        end
      end

      connector = fn address, opts ->
        timeout = Keyword.fetch!(opts, :address_list_setup_timeout)
        send(parent, {:attempted, address, timeout})

        case address.family do
          :inet6 ->
            Process.put(:bus_address_slice_clock, Process.get(:bus_address_slice_clock) + timeout)
            {:error, :read_timeout}

          :inet ->
            {:ok, self()}
        end
      end

      assert {:ok, _pid} =
               Rebus.connect_address_candidates(
                 [
                   {:tcp, "example", 12_345, :unspec, nil},
                   {:local, "/tmp/fallback", nil}
                 ],
                 [timeout: 90],
                 resolver: resolver,
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      assert_receive {:resolved, :inet6, 30}
      assert_receive {:resolved, :inet, 45}
      assert_receive {:attempted, %{family: :inet6}, 30}
      assert_receive {:attempted, %{family: :inet}, 30}
      refute_receive {:attempted, %{family: :local}, _}
      assert Process.get(:bus_address_slice_clock) <= 90
    end

    test "gives a stalled Unix candidate a slice before a later entry" do
      parent = self()
      Process.put(:bus_address_later_candidate_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_later_candidate_clock) end

      connector = fn address, opts ->
        timeout = Keyword.fetch!(opts, :address_list_setup_timeout)
        send(parent, {:attempted, address, timeout})

        case address.path do
          "/tmp/first" ->
            Process.put(
              :bus_address_later_candidate_clock,
              Process.get(:bus_address_later_candidate_clock) + timeout
            )

            {:error, :read_timeout}

          "/tmp/second" ->
            {:ok, self()}
        end
      end

      assert {:ok, _pid} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/first", nil}, {:local, "/tmp/second", nil}],
                 [timeout: 90],
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      assert_receive {:attempted, %{path: "/tmp/first"}, 45}
      assert_receive {:attempted, %{path: "/tmp/second"}, 45}
      assert Process.get(:bus_address_later_candidate_clock) <= 90
    end

    test "stops TCP IP fallback immediately on a guid mismatch" do
      parent = self()
      expected_guid = String.duplicate("a", 32)

      resolver = fn _host, :inet, _timeout ->
        {:ok, [{127, 0, 0, 1}, {127, 0, 0, 2}]}
      end

      connector = fn address, opts ->
        send(parent, {:attempted, address, Keyword.fetch!(opts, :expected_guid)})
        {:error, :guid_mismatch}
      end

      assert {:error, :guid_mismatch} =
               Rebus.connect_address_candidates(
                 [{:tcp, "example", 12_345, :inet, expected_guid}],
                 [timeout: 100],
                 resolver: resolver,
                 connector: connector,
                 # This test controls both resolver and connector outcomes;
                 # keep its address-budget assertion independent of runner
                 # scheduling before the first deterministic attempt.
                 monotonic_time: fn -> 0 end
               )

      assert_receive {:attempted, %{family: :inet, addr: {127, 0, 0, 1}}, ^expected_guid}
      refute_receive {:attempted, %{family: :inet, addr: {127, 0, 0, 2}}, _}
    end

    test "keeps the safe last error when the TCP IP loop reaches its deadline" do
      parent = self()
      Process.put(:bus_address_ip_deadline_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_ip_deadline_clock) end

      resolver = fn _host, :inet, _timeout ->
        {:ok, [{127, 0, 0, 1}, {127, 0, 0, 2}]}
      end

      connector = fn address, opts ->
        timeout = Keyword.fetch!(opts, :address_list_setup_timeout)
        send(parent, {:attempted, address, timeout})

        Process.put(
          :bus_address_ip_deadline_clock,
          Process.get(:bus_address_ip_deadline_clock) + timeout * 2 + 1
        )

        {:error, :econnrefused}
      end

      assert {:error, {:read_timeout, :econnrefused}} =
               Rebus.connect_address_candidates(
                 [{:tcp, "example", 12_345, :inet, nil}],
                 [timeout: 50],
                 resolver: resolver,
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      assert_receive {:attempted, %{addr: {127, 0, 0, 1}}, 25}
      refute_receive {:attempted, %{addr: {127, 0, 0, 2}}, _}
    end

    test "honours explicit TCP family filters and skips empty resolutions" do
      parent = self()

      resolver = fn _host, family, _timeout ->
        send(parent, {:resolved, family})
        {:ok, []}
      end

      connector = fn address, _opts ->
        send(parent, {:connected, address})
        {:ok, self()}
      end

      assert {:ok, _pid} =
               Rebus.connect_address_candidates(
                 [
                   {:tcp, "example", 12_345, :inet, nil},
                   {:local, "/tmp/fallback", nil}
                 ],
                 [timeout: 100],
                 resolver: resolver,
                 connector: connector
               )

      assert_receive {:resolved, :inet}
      refute_receive {:resolved, :inet6}
      assert_receive {:connected, %{family: :local, path: "/tmp/fallback"}}
    end

    test "returns a safe TCP resolver reason without retaining the host" do
      sentinel = "resolver-host-sentinel"

      resolver = fn _host, family, _timeout ->
        case family do
          :inet6 -> {:error, :eafnosupport}
          :inet -> {:error, :nxdomain}
        end
      end

      assert {:error, {:tcp_resolution_failed, :nxdomain}} =
               Rebus.connect_address_candidates(
                 [{:tcp, sentinel, 12_345, :unspec, nil}],
                 [timeout: 100],
                 resolver: resolver
               )
    end

    test "does not invoke a resolver after the aggregate deadline expires" do
      parent = self()
      Process.put(:bus_address_resolver_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_resolver_clock) end

      resolver = fn _host, family, timeout ->
        send(parent, {:resolved, family, timeout})
        Process.put(:bus_address_resolver_clock, 10)
        {:error, {:untrusted, "resolver-payload-sentinel"}}
      end

      assert {:error, {:read_timeout, :tcp_resolution_failed}} =
               Rebus.connect_address_candidates(
                 [{:tcp, "example", 12_345, :unspec, nil}],
                 [timeout: 5],
                 resolver: resolver,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      assert_receive {:resolved, :inet6, 2}
      refute_receive {:resolved, :inet, _}
    end

    test "returns a bare aggregate timeout before any address attempt" do
      parent = self()
      Process.put(:bus_address_initial_deadline_clock, [0, 5])

      monotonic_time = fn ->
        [now | remaining] = Process.get(:bus_address_initial_deadline_clock)
        Process.put(:bus_address_initial_deadline_clock, remaining)
        now
      end

      connector = fn address, _opts ->
        send(parent, {:attempted, address})
        {:ok, self()}
      end

      assert {:error, :read_timeout} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/never-attempted", nil}],
                 [timeout: 5],
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      refute_receive {:attempted, _}
    end

    test "redacts non-atom failures from an aggregate timeout diagnostic" do
      sentinel = "deadline-last-error-sentinel"
      Process.put(:bus_address_safe_deadline_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_safe_deadline_clock) end

      connector = fn _address, _opts ->
        Process.put(:bus_address_safe_deadline_clock, 10)
        {:error, {:untrusted, sentinel}}
      end

      assert {:error, {:read_timeout, :connection_failed}} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/one", nil}, {:local, "/tmp/two", nil}],
                 [timeout: 5],
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )
    end

    test "rejects an invalid internal address-list implementation seam" do
      assert {:error, :invalid_bus_address_implementation} =
               Rebus.connect_address_candidates([], [timeout: 100], resolver: :not_a_function)
    end

    test "shares one setup budget across list attempts and aborts global failures" do
      parent = self()
      Process.put(:bus_address_clock, 0)

      monotonic_time = fn -> Process.get(:bus_address_clock) end

      connector = fn address, opts ->
        send(parent, {:attempted, address, Keyword.fetch!(opts, :address_list_setup_timeout)})
        Process.put(:bus_address_clock, Process.get(:bus_address_clock) + 40)
        {:error, :econnrefused}
      end

      assert {:error, {:read_timeout, :econnrefused}} =
               Rebus.connect_address_candidates(
                 [
                   {:local, "/tmp/one", nil},
                   {:local, "/tmp/two", nil},
                   {:local, "/tmp/three", nil}
                 ],
                 [timeout: 50],
                 connector: connector,
                 auth_id_runner: fn _timeout -> {:ok, "501\n"} end,
                 monotonic_time: monotonic_time
               )

      assert_receive {:attempted, %{path: "/tmp/one"}, 16}
      assert_receive {:attempted, %{path: "/tmp/two"}, 5}
      refute_receive {:attempted, %{path: "/tmp/three"}, _}

      aborting_connector = fn address, _opts ->
        send(parent, {:aborted, address})
        {:error, :auth_id_unavailable}
      end

      assert {:error, :auth_id_unavailable} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/one", nil}, {:local, "/tmp/two", nil}],
                 [timeout: 50],
                 connector: aborting_connector,
                 monotonic_time: monotonic_time
               )

      assert_receive {:aborted, %{path: "/tmp/one"}}
      refute_receive {:aborted, %{path: "/tmp/two"}}

      unavailable_cookie_connector = fn address, _opts ->
        send(parent, {:cookie_unavailable, address})
        {:error, :auth_cookie_unavailable}
      end

      assert {:error, :auth_cookie_unavailable} =
               Rebus.connect_address_candidates(
                 [{:local, "/tmp/one", nil}, {:local, "/tmp/two", nil}],
                 [timeout: 50],
                 connector: unavailable_cookie_connector,
                 monotonic_time: monotonic_time
               )

      assert_receive {:cookie_unavailable, %{path: "/tmp/one"}}
      refute_receive {:cookie_unavailable, %{path: "/tmp/two"}}
    end
  end

  describe "Public message API" do
    setup [:server_setup, :client_setup]

    test "returns the complete reply message when called", %{cli: cli, svr: svr} do
      method =
        Rebus.Message.new!(
          :method_call,
          path: "/org/freedesktop/DBus",
          member: "FakeMethod",
          signature: "s",
          flags: [],
          body: ["foobar"]
        )

      # Call the method (in a task to avoid blocking the test)
      task = Task.async(fn -> Rebus.call(cli, method) end)
      # Confirm the server received it
      assert_receive {^svr, %Message{} = rcvd}
      assert rcvd.body == ["foobar"]

      # Reply to the method call to unblock the caller
      reply =
        Rebus.Message.new!(
          :method_return,
          reply_serial: rcvd.serial,
          signature: "s",
          flags: [],
          body: ["response"]
        )

      TestServer.push(svr, reply)

      resp = Task.await(task)
      assert resp.body == ["response"]
    end

    test "returns D-Bus error replies as complete messages", %{cli: cli, svr: svr} do
      method =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          member: "FakeMethod"
        )

      task = Task.async(fn -> Rebus.call(cli, method) end)
      assert_receive {^svr, %Message{} = received}

      error =
        Message.new!(:error,
          error_name: "org.example.Failed",
          reply_serial: received.serial,
          signature: "s",
          body: ["failed"]
        )

      :ok = TestServer.push(svr, error)

      assert %Message{type: :error, header_fields: %{error_name: "org.example.Failed"}} =
               Task.await(task)
    end

    test "cleans pending calls that time out", %{cli: cli, svr: svr} do
      method =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          member: "NeverReplies"
        )

      task = Task.async(fn -> Rebus.call(cli, method, 500) end)
      assert_receive {^svr, %Message{header_fields: %{member: "NeverReplies"}}}

      [{serial, {_from, _timer_ref, request_ref, _monitor_ref, _deadline}}] =
        :sys.get_state(cli).pending |> Map.to_list()

      send(cli, {:request_timeout, serial, request_ref})

      assert {:error, :timeout} = Task.await(task)
      assert :sys.get_state(cli).pending == %{}
      assert :sys.get_state(cli).request_index == %{}
      assert :sys.get_state(cli).monitor_index == %{}
      assert Process.alive?(cli)
    end

    test "bounds the caller timeout while the connection is busy", %{cli: cli, svr: svr} do
      :ok = :sys.suspend(cli)
      method = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "Queued")
      started_at = System.monotonic_time(:millisecond)

      assert {:error, :timeout} = Rebus.call(cli, method, 20)
      assert System.monotonic_time(:millisecond) - started_at < 250

      :ok = :sys.resume(cli)
      refute_receive {^svr, %Message{header_fields: %{member: "Queued"}}}, 50
      assert :sys.get_state(cli).pending == %{}
    end

    test "does not stop a shared connection for a tiny call timeout", %{cli: cli, svr: svr} do
      method = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "Tiny")

      assert {:error, :timeout} = Rebus.call(cli, method, 0)
      assert Process.alive?(cli)

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "StillAvailable"
        )

      assert :ok = Rebus.send(cli, signal)
      assert_receive {^svr, %Message{header_fields: %{member: "StillAvailable"}}}
    end

    test "times out a busy send without delivering it later", %{cli: cli, svr: svr} do
      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "QueuedSend"
        )

      :ok = :sys.suspend(cli)
      task = Task.async(fn -> Rebus.send(cli, signal, 20) end)

      assert {:ok, {:error, :timeout}} = Task.yield(task, 500)
      :ok = :sys.resume(cli)

      refute_receive {^svr, %Message{header_fields: %{member: "QueuedSend"}}}, 50
      assert Process.alive?(cli)
    end

    test "removes a pending call when its caller exits", %{cli: cli, svr: svr} do
      method = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "CallerDies")
      task = Task.async(fn -> Rebus.call(cli, method, 5_000) end)

      assert_receive {^svr, %Message{header_fields: %{member: "CallerDies"}}}
      assert map_size(:sys.get_state(cli).pending) == 1
      _ = Task.shutdown(task, :brutal_kill)

      assert wait_until(fn -> :sys.get_state(cli).pending == %{} end)
      assert :sys.get_state(cli).request_index == %{}
      assert :sys.get_state(cli).monitor_index == %{}
      assert Process.alive?(cli)
    end

    test "fails pending calls when the transport closes", %{cli: cli, svr: svr} do
      method = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "Pending")
      task = Task.async(fn -> Rebus.call(cli, method, 5_000) end)
      assert_receive {^svr, %Message{header_fields: %{member: "Pending"}}}
      ref = Process.monitor(cli)
      :ok = :socket.close(:sys.get_state(cli).sock)

      assert {:error, :disconnected} = Task.await(task)
      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, _reason}}
    end

    test "correlates concurrent calls when replies arrive out of order", %{cli: cli, svr: svr} do
      first = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "First")
      second = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "Second")
      first_task = Task.async(fn -> Rebus.call(cli, first) end)
      second_task = Task.async(fn -> Rebus.call(cli, second) end)

      assert_receive {^svr, %Message{header_fields: %{member: first_member}} = first_received}
      assert_receive {^svr, %Message{header_fields: %{member: second_member}} = second_received}

      received = %{first_member => first_received, second_member => second_received}

      :ok =
        TestServer.push(
          svr,
          Message.new!(:method_return,
            reply_serial: received["Second"].serial,
            signature: "s",
            body: ["second reply"]
          )
        )

      :ok =
        TestServer.push(
          svr,
          Message.new!(:method_return,
            reply_serial: received["First"].serial,
            signature: "s",
            body: ["first reply"]
          )
        )

      assert %Message{body: ["first reply"]} = Task.await(first_task)
      assert %Message{body: ["second reply"]} = Task.await(second_task)
    end

    test "rejects operation and message combinations it cannot honour", %{cli: cli} do
      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "Changed"
        )

      no_reply_call =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          member: "NoReply",
          flags: [:no_reply_expected]
        )

      assert {:error, {:invalid_message_type, :signal}} = Rebus.call(cli, signal)
      assert {:error, :no_reply_expected} = Rebus.call(cli, no_reply_call)

      reply_expected =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          member: "ReplyExpected"
        )

      assert {:error, :reply_expected} = Rebus.send(cli, reply_expected)
      assert :ok = Rebus.send(cli, no_reply_call)
    end

    test "skips live pending serials when serial numbers wrap", %{cli: cli, svr: svr} do
      first = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "First")
      first_task = Task.async(fn -> Rebus.call(cli, first) end)
      assert_receive {^svr, %Message{} = first_received}

      :ok = :sys.suspend(cli)
      _ = :sys.replace_state(cli, fn state -> %{state | serial: 4_294_967_295} end)
      :ok = :sys.resume(cli)

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "Wrapped"
        )

      assert :ok = Rebus.send(cli, signal)
      assert_receive {^svr, %Message{serial: 4_294_967_295}}
      assert :ok = Rebus.send(cli, signal)
      assert_receive {^svr, %Message{serial: 1}}

      second = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "Second")
      second_task = Task.async(fn -> Rebus.call(cli, second) end)
      assert_receive {^svr, %Message{serial: 3} = second_received}

      :ok =
        TestServer.push(svr, Message.new!(:method_return, reply_serial: second_received.serial))

      :ok =
        TestServer.push(svr, Message.new!(:method_return, reply_serial: first_received.serial))

      assert %Message{serial: _} = Task.await(first_task)
      assert %Message{serial: _} = Task.await(second_task)
    end

    test "stops the connection when a send fails", %{cli: cli} do
      sock = :sys.get_state(cli).sock
      ref = Process.monitor(cli)
      :ok = :socket.close(sock)

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "AfterClose"
        )

      assert {:error, :disconnected} = Rebus.send(cli, signal)
      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, _reason}}
    end

    test "rejects remote connection PIDs" do
      node_name = "synthetic@remote"

      remote_connection =
        :erlang.binary_to_term(
          <<131, 103, 100, byte_size(node_name)::16, node_name::binary, 1::32, 0::32, 0>>
        )

      message = Message.new!(:method_call, path: "/org/freedesktop/DBus", member: "Remote")

      assert {:error, :remote_connection_unsupported} = Rebus.call(remote_connection, message)
      assert {:error, :remote_connection_unsupported} = Rebus.send(remote_connection, message)
    end

    test "keeps the connection alive when an outgoing message cannot be encoded", %{
      cli: cli,
      svr: svr
    } do
      invalid_message = %Message{
        type: :method_call,
        flags: [],
        version: 1,
        body_length: 0,
        serial: 0,
        header_fields: %{
          path: "/org/freedesktop/DBus",
          member: "BadBody",
          signature: "s"
        },
        body: [42]
      }

      assert {:error, :encode_failed} = Rebus.call(cli, invalid_message)
      assert Process.alive?(cli)

      signal =
        Message.new!(:signal,
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "StillConnected"
        )

      assert :ok = Rebus.send(cli, signal)
      assert_receive {^svr, %Message{type: :signal, header_fields: %{member: "StillConnected"}}}
      assert Process.alive?(cli)
    end

    test "resumes a selected outbound frame with its OTP continuation", %{cli: cli} do
      parent = self()
      continuation = {:select_info, :send, make_ref()}
      {:select_info, :send, handle} = continuation
      calls = :atomics.new(1, [])

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _sock, rest, flags_or_cont, timeout ->
              call = :atomics.add_get(calls, 1, 1)
              send(parent, {:outbound_send, call, rest, flags_or_cont, timeout})
              if call == 1, do: {:select, continuation}, else: :ok
            end
        }
      end)

      signal = Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Selected")
      task = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert_receive {:outbound_send, 1, _rest, [], :nowait}
      sock = :sys.get_state(cli).sock
      send(cli, {:"$socket", sock, :select, handle})
      assert_receive {:outbound_send, 2, _rest, ^continuation, :nowait}
      assert :ok = Task.await(task)
    end

    test "cancels a zero-byte selected frame after its caller times out", %{cli: cli} do
      parent = self()
      continuation = {:select_info, :send, make_ref()}

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _, _, _, _ -> {:select, continuation} end,
            cancel_fun: fn _sock, info ->
              send(parent, {:cancelled_write, info})
              :ok
            end
        }
      end)

      signal =
        Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Cancelled")

      assert {:error, :timeout} = Rebus.send(cli, signal, 20)
      assert_receive {:cancelled_write, ^continuation}, 500
      assert :sys.get_state(cli).active_write == nil
    end

    test "continues an immediate partial outbound write with its exact binary tail", %{cli: cli} do
      parent = self()
      calls = :atomics.new(1, [])

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _sock, rest, _flags, _timeout ->
              case :atomics.add_get(calls, 1, 1) do
                1 ->
                  tail = binary_part(rest, 1, byte_size(rest) - 1)
                  send(parent, {:first_tail, tail})
                  {:ok, tail}

                2 ->
                  send(parent, {:second_tail, rest})
                  :ok
              end
            end
        }
      end)

      signal = Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Partial")
      assert :ok = Rebus.send(cli, signal, 500)
      assert_receive {:first_tail, tail}
      assert_receive {:second_tail, ^tail}
    end

    test "handles select results that include a partial tail", %{cli: cli} do
      parent = self()
      continuation = {:select_info, :send, make_ref()}
      {:select_info, :send, handle} = continuation
      calls = :atomics.new(1, [])

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _sock, rest, flags, timeout ->
              case :atomics.add_get(calls, 1, 1) do
                1 ->
                  tail = binary_part(rest, 1, byte_size(rest) - 1)
                  send(parent, {:selected_tail, tail})
                  {:select, {continuation, tail}}

                2 ->
                  send(parent, {:resumed_tail, rest, flags, timeout})
                  :ok
              end
            end
        }
      end)

      signal =
        Message.new!(:signal, path: "/", interface: "org.example.Test", member: "SelectPartial")

      task = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert_receive {:selected_tail, tail}
      send(cli, {:"$socket", :sys.get_state(cli).sock, :select, handle})
      assert_receive {:resumed_tail, ^tail, ^continuation, :nowait}
      assert :ok = Task.await(task)
    end

    test "completes a Windows-style completion write", %{cli: cli} do
      completion = {:completion_info, :send, make_ref()}
      {:completion_info, :send, handle} = completion

      :sys.replace_state(cli, fn state ->
        %{state | send_fun: fn _, _, _, _ -> {:completion, completion} end}
      end)

      signal =
        Message.new!(:signal, path: "/", interface: "org.example.Test", member: "Completion")

      task = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert wait_until(fn -> :sys.get_state(cli).active_write != nil end)
      send(cli, {:"$socket", :sys.get_state(cli).sock, :completion, {handle, :ok}})
      assert :ok = Task.await(task)
    end

    test "continues a partial completion write", %{cli: cli} do
      parent = self()
      completion = {:completion_info, :send, make_ref()}
      {:completion_info, :send, handle} = completion
      calls = :atomics.new(1, [])

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _, rest, _, _ ->
              if :atomics.add_get(calls, 1, 1) == 1 do
                send(parent, {:completion_rest, rest})
                {:completion, completion}
              else
                send(parent, {:completion_tail, rest})
                :ok
              end
            end
        }
      end)

      signal =
        Message.new!(:signal,
          path: "/",
          interface: "org.example.Test",
          member: "PartialCompletion"
        )

      task = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert_receive {:completion_rest, rest}
      tail = binary_part(rest, 1, byte_size(rest) - 1)
      send(cli, {:"$socket", :sys.get_state(cli).sock, :completion, {handle, {:ok, 1}}})
      assert_receive {:completion_tail, ^tail}
      assert :ok = Task.await(task)
    end

    test "stops all queued callers when a selected partial frame times out", %{cli: cli} do
      parent = self()
      continuation = {:select_info, :send, make_ref()}

      :sys.replace_state(cli, fn state ->
        %{
          state
          | write_timeout: 20,
            send_fun: fn _, rest, _, _ ->
              {:select, {continuation, binary_part(rest, 1, byte_size(rest) - 1)}}
            end,
            cancel_fun: fn _, info ->
              send(parent, {:cancelled_write, info})
              :ok
            end
        }
      end)

      signal =
        Message.new!(:signal, path: "/", interface: "org.example.Test", member: "TimeoutPartial")

      ref = Process.monitor(cli)
      first = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      second = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert {:error, :disconnected} = Task.await(first, 1_000)
      assert {:error, :disconnected} = Task.await(second, 1_000)
      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :timeout}}, 1_000
    end

    test "does not transmit a queued frame after its caller exits", %{cli: cli} do
      parent = self()
      continuation = {:select_info, :send, make_ref()}
      {:select_info, :send, handle} = continuation
      calls = :atomics.new(1, [])

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _, _rest, _flags, _timeout ->
              call = :atomics.add_get(calls, 1, 1)
              send(parent, {:queued_down_send, call})
              if call == 1, do: {:select, continuation}, else: :ok
            end
        }
      end)

      signal =
        Message.new!(:signal, path: "/", interface: "org.example.Test", member: "QueuedDown")

      first = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert_receive {:queued_down_send, 1}
      second = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert wait_until(fn -> :queue.len(:sys.get_state(cli).write_queue) == 1 end)
      _ = Task.shutdown(second, :brutal_kill)
      assert wait_until(fn -> MapSet.size(:sys.get_state(cli).cancelled_requests) == 1 end)

      send(cli, {:"$socket", :sys.get_state(cli).sock, :select, handle})
      assert_receive {:queued_down_send, 2}
      assert :ok = Task.await(first)
      assert :atomics.get(calls, 1) == 2

      assert wait_until(fn ->
               state = :sys.get_state(cli)

               :queue.is_empty(state.write_queue) and MapSet.size(state.queued_requests) == 0 and
                 MapSet.size(state.cancelled_requests) == 0 and
                 map_size(state.outbound_monitor_index) == 0
             end)

      assert Process.alive?(cli)
    end

    test "finishes a partial frame after its caller exits without registering a reply", %{
      cli: cli
    } do
      parent = self()
      continuation = {:select_info, :send, make_ref()}
      {:select_info, :send, handle} = continuation
      calls = :atomics.new(1, [])

      :sys.replace_state(cli, fn state ->
        %{
          state
          | send_fun: fn _, rest, _flags, _timeout ->
              case :atomics.add_get(calls, 1, 1) do
                1 ->
                  tail = binary_part(rest, 1, byte_size(rest) - 1)
                  send(parent, {:partial_down_tail, tail})
                  {:select, {continuation, tail}}

                _ ->
                  send(parent, {:partial_down_resume, rest})
                  :ok
              end
            end
        }
      end)

      signal =
        Message.new!(:signal, path: "/", interface: "org.example.Test", member: "PartialDown")

      task = Task.async(fn -> Rebus.send(cli, signal, 500) end)
      assert_receive {:partial_down_tail, tail}
      _ = Task.shutdown(task, :brutal_kill)
      assert wait_until(fn -> MapSet.size(:sys.get_state(cli).cancelled_requests) == 1 end)

      send(cli, {:"$socket", :sys.get_state(cli).sock, :select, handle})
      assert_receive {:partial_down_resume, ^tail}

      assert wait_until(fn ->
               state = :sys.get_state(cli)

               state.active_write == nil and state.pending == %{} and state.request_index == %{} and
                 state.monitor_index == %{} and map_size(state.outbound_monitor_index) == 0 and
                 MapSet.size(state.cancelled_requests) == 0
             end)

      assert Process.alive?(cli)
    end
  end

  describe "Signals" do
    setup [:server_setup, :client_setup]

    test "are received", %{cli: cli, svr: svr} do
      # add a remove a signal handler to test that works
      ref = Rebus.add_signal_handler(cli)
      Rebus.delete_signal_handler(cli, ref)

      # Add one back
      ref = Rebus.add_signal_handler(cli)

      # Send the NameAcquired signal
      signal =
        Rebus.Message.new!(
          :signal,
          sender: "org.freedesktop.DBus",
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "FakeSignal",
          destination: ":1.100",
          signature: "s",
          flags: [],
          body: ["foobar"]
        )

      :ok = TestServer.push(svr, signal)

      assert_receive {^ref, %Message{body: ["foobar"]}}
    end
  end

  defp server_setup(_) do
    # The 'tap' process receives client frames. The server automatically replies
    # to Hello; tests that need to control the handshake disable that behaviour.
    {:ok, svr} = start_supervised({Rebus.TestServer, tap: self()})
    %{svr: svr}
  end

  defp client_setup(%{svr: svr}) do
    {:ok, addr} = TestServer.get_listen_addr(svr)
    {:ok, cli} = Rebus.connect(addr)

    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}

    %{cli: cli}
  end

  defp handle_hello(%Message{} = msg, svr, opts \\ []) do
    reply =
      Rebus.Message.new!(
        :method_return,
        reply_serial: msg.serial,
        signature: "s",
        flags: [],
        body: [":1.100"]
      )

    :ok = TestServer.push(svr, reply)

    if Keyword.get(opts, :send_name_acquired?, true) do
      signal =
        Rebus.Message.new!(
          :signal,
          sender: "org.freedesktop.DBus",
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "NameAcquired",
          destination: ":1.100",
          signature: "s",
          flags: [],
          body: [":1.100"]
        )

      :ok = TestServer.push(svr, signal)
    end
  end

  defp connect_until_hello(svr, opts \\ []) do
    {_fixture_opts, connect_opts} = split_fixture_options(opts)
    {:ok, addr} = TestServer.get_listen_addr(svr)
    :ok = TestServer.set_auto_hello(svr, false)
    args = Keyword.put(connect_opts, :addr, addr)

    {:ok, cli} =
      DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, {Rebus.Connection, args})

    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}, 1_000
    assert hello.serial == 1
    {cli, hello}
  end

  defp connect_until_ready(svr, opts \\ []) do
    {fixture_opts, connect_opts} = split_fixture_options(opts)

    :ok =
      TestServer.set_auto_hello(svr, true, Keyword.get(fixture_opts, :send_name_acquired?, true))

    {:ok, addr} = TestServer.get_listen_addr(svr)
    {:ok, cli} = Rebus.connect(addr, connect_opts)
    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}, serial: 1}}
    cli
  end

  defp connect_until_ready_direct(svr, opts) do
    {fixture_opts, connect_opts} = split_fixture_options(opts)

    :ok =
      TestServer.set_auto_hello(svr, true, Keyword.get(fixture_opts, :send_name_acquired?, true))

    {:ok, addr} = TestServer.get_listen_addr(svr)
    args = Keyword.put(connect_opts, :addr, addr)

    {:ok, cli} =
      DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, {Rebus.Connection, args})

    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}, serial: 1}}
    assert wait_until(fn -> :sys.get_state(cli).established? end)
    cli
  end

  defp split_fixture_options(opts) do
    # Test-only controls must never be passed to Rebus.connect/2.
    Keyword.split(opts, [:send_name_acquired?])
  end

  defp assert_hello_error_reason(svr, expected_reason, build_reply) do
    capture_log(fn ->
      {cli, hello} = connect_until_hello(svr)
      reply = build_reply.(hello.serial)

      encoded =
        if is_binary(reply) do
          reply
        else
          {:ok, iodata} = Message.encode(reply)
          IO.iodata_to_binary(iodata)
        end

      ref = Process.monitor(cli)

      :ok = TestServer.push_raw(svr, encoded)

      expected_shutdown =
        if expected_reason == :invalid_message,
          do: :invalid_message,
          else: {:hello_failed, expected_reason}

      assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, ^expected_shutdown}}, 1_000
    end)
  end

  defp assert_unexpected_handshake_message(svr, cli, message, type) do
    ref = Process.monitor(cli)
    :ok = TestServer.push(svr, message)

    assert_receive {:DOWN, ^ref, :process, ^cli,
                    {:shutdown, {:unexpected_handshake_message, ^type}}},
                   1_000
  end

  defp assert_missing_reply_serial(svr, type) do
    log =
      capture_log(fn ->
        cli = connect_until_ready(svr)
        ref = Process.monitor(cli)
        type_code = if type == :method_return, do: 2, else: 3
        encoded = <<?l, type_code, 0, 1, 0::little-32, 1::little-32, 0::little-32>>

        :ok = TestServer.push_raw(svr, encoded)

        assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, :invalid_message}}, 1_000
      end)

    assert log =~ "D-Bus connection protocol stopped: :invalid_message"
    refute log =~ "%Rebus.Connection"
  end

  defp raw_error_reply(reply_serial, header_fields) do
    raw_reply(:error, Map.put(header_fields, :reply_serial, reply_serial))
  end

  defp raw_error_reply_binary(reply_serial, header_fields) do
    fields =
      header_fields
      |> Map.put(:reply_serial, reply_serial)
      |> Enum.map(fn {field, value} ->
        type = if field == :reply_serial, do: "u", else: "s"
        code = if field == :reply_serial, do: 5, else: 4
        [code, {type, value}]
      end)

    header =
      Rebus.Encoder.encode_at_position("a(yv)", [fields], :little, 12) |> IO.iodata_to_binary()

    padding = :binary.copy(<<0>>, rem(8 - rem(12 + byte_size(header), 8), 8))

    <<?l, 3, 0, 1, 0::little-32, 1::little-32, header::binary, padding::binary>>
  end

  defp raw_resource_limited_signal do
    raw_wire_message(
      4,
      [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Limited"}],
        [8, {"g", "ay"}]
      ],
      scalar_limited_body()
    )
  end

  defp raw_resource_limited_reply(reply_serial) do
    raw_wire_message(2, [[5, {"u", reply_serial}], [8, {"g", "ay"}]], scalar_limited_body())
  end

  defp raw_resource_limited_error_reply(reply_serial, error_name) do
    raw_wire_message(
      3,
      [[4, {"s", error_name}], [5, {"u", reply_serial}], [8, {"g", "ay"}]],
      scalar_limited_body()
    )
  end

  defp raw_header_resource_limited_signal do
    fields =
      [[1, {"o", "/test"}], [2, {"s", "test.interface"}], [3, {"s", "HeaderLimited"}]] ++
        List.duplicate([10, {"ay", []}], 25_001)

    raw_wire_message(4, fields, <<>>)
  end

  defp raw_truncated_scalar_signal do
    raw_wire_message(
      4,
      [
        [1, {"o", "/test"}],
        [2, {"s", "test.interface"}],
        [3, {"s", "Truncated"}],
        [8, {"g", "ay"}]
      ],
      <<1_000_001::little-32, 1>>
    )
  end

  defp scalar_limited_body do
    sentinel = "resource-limit-body-sentinel"

    <<1_000_001::little-32, sentinel::binary>> <>
      :binary.copy(<<1>>, 1_000_001 - byte_size(sentinel))
  end

  defp raw_wire_message(type, fields, body) do
    header =
      Rebus.Encoder.encode_at_position("a(yv)", [fields], :little, 12)
      |> IO.iodata_to_binary()

    padding = :binary.copy(<<0>>, rem(8 - rem(12 + byte_size(header), 8), 8))

    <<?l, type, 0, 1, byte_size(body)::little-32, 1::little-32, header::binary, padding::binary,
      body::binary>>
  end

  defp malformed_empty_struct_array_message do
    valid_fields = [
      [1, {"o", "/test"}],
      [2, {"s", "test.interface"}],
      [3, {"s", "ZeroWidth"}]
    ]

    valid_header =
      Rebus.Encoder.encode_at_position("a(yv)", [valid_fields], :little, 12)
      |> IO.iodata_to_binary()

    valid_data = binary_part(valid_header, 4, byte_size(valid_header) - 4)
    signature_field = <<8, 1, "g", 0, 3, "a()", 0>>
    field_padding = :binary.copy(<<0>>, rem(8 - rem(16 + byte_size(valid_data), 8), 8))
    fields_data = valid_data <> field_padding <> signature_field
    header = <<byte_size(fields_data)::little-32, fields_data::binary>>
    padding = :binary.copy(<<0>>, rem(8 - rem(12 + byte_size(header), 8), 8))
    body = <<1::little-32, 0::size(4 * 8)>>

    <<?l, 4, 0, 1, byte_size(body)::little-32, 1::little-32, header::binary, padding::binary,
      body::binary>>
  end

  defp raw_reply(type, header_fields) do
    %Message{
      type: type,
      flags: [],
      version: 1,
      body_length: 0,
      serial: 1,
      header_fields: header_fields,
      body: []
    }
  end

  defp start_fragmented_socket_server(first) do
    parent = self()

    server =
      Task.async(fn ->
        {:ok, listener} = :socket.open(:inet, :stream, :default)
        :ok = :socket.bind(listener, %{family: :inet, addr: :loopback, port: 0})
        :ok = :socket.listen(listener, 1)
        {:ok, addr} = :socket.sockname(listener)
        send(parent, {:fragmented_socket_ready, self(), addr})

        {:ok, peer} = :socket.accept(listener)
        :ok = :socket.send(peer, first)

        receive do
          {:send_remainder, remainder} -> :ok = :socket.send(peer, remainder)
        end

        receive do
          :close -> :ok
        end

        :ok = :socket.close(peer)
        :ok = :socket.close(listener)
      end)

    server_pid = server.pid
    assert_receive {:fragmented_socket_ready, ^server_pid, addr}
    {:ok, sock} = :socket.open(:inet, :stream, :default)
    :ok = :socket.connect(sock, addr)
    {sock, server}
  end

  defp protocol_stop_log_count(log) do
    length(String.split(log, "D-Bus connection protocol stopped:")) - 1
  end

  defp inbound_data(%Connection{} = state) do
    state.inbound_segments
    |> Enum.reverse()
    |> Enum.map(&elem(&1, 1))
    |> IO.iodata_to_binary()
  end

  defp wait_until(predicate, attempts \\ 100)

  defp wait_until(predicate, 0), do: predicate.()

  defp wait_until(predicate, attempts) do
    if predicate.() do
      true
    else
      Process.sleep(10)
      wait_until(predicate, attempts - 1)
    end
  end

  defp socket_path do
    "/tmp/rebus_test_#{System.os_time(:nanosecond)}_#{:erlang.unique_integer([:positive])}.sock"
  end

  defp put_system_bus_address(address) do
    previous = Application.fetch_env(:rebus, :system_bus_address)

    on_exit(fn ->
      case previous do
        {:ok, value} -> Application.put_env(:rebus, :system_bus_address, value)
        :error -> Application.delete_env(:rebus, :system_bus_address)
      end
    end)

    Application.put_env(:rebus, :system_bus_address, address)
  end

  defp put_session_bus_address(address) do
    previous = System.get_env("DBUS_SESSION_BUS_ADDRESS")

    on_exit(fn ->
      if is_nil(previous) do
        System.delete_env("DBUS_SESSION_BUS_ADDRESS")
      else
        System.put_env("DBUS_SESSION_BUS_ADDRESS", previous)
      end
    end)

    if is_nil(address) do
      System.delete_env("DBUS_SESSION_BUS_ADDRESS")
    else
      System.put_env("DBUS_SESSION_BUS_ADDRESS", address)
    end
  end
end
