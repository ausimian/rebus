defmodule RebusTest do
  use ExUnit.Case
  doctest Rebus
  import ExUnit.CaptureLog

  alias Rebus.Connection
  alias Rebus.Message
  alias Rebus.SignalHandler
  alias Rebus.TestServer

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

    test "rejects an invalid write timeout", %{svr: svr} do
      {:ok, addr} = TestServer.get_listen_addr(svr)
      assert {:error, :invalid_write_timeout} = Rebus.connect(addr, write_timeout: -1)
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

      assert {:error, :auth_failed} = Rebus.connect(addr)
      assert_receive {^rejecting_svr, :client_closed}, 1_000
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

    test "ignores arbitrary info messages" do
      {:ok, sock} = :socket.open(:inet, :stream, :default)
      state = %Connection{sock: sock}

      assert {:noreply, ^state} = Connection.handle_info({:unexpected, "message"}, state)
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
          {:ok, cli} = Rebus.connect(addr)
          ref = Process.monitor(cli)

          assert_receive {:DOWN, ^ref, :process, ^cli, {:shutdown, reason}}, 1_000
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
      state = %Connection{sock: sock}

      assert {:noreply, %{prev: ^first} = state, {:continue, :hello_reply}} =
               Connection.handle_continue(:hello_reply, state)

      send(server.pid, {:send_remainder, second})

      log =
        capture_log(fn ->
          assert {:stop, {:shutdown, {:hello_failed, "org.example.SecretError"}}, ^state} =
                   Connection.handle_continue(:hello_reply, state)
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
      state = %Connection{sock: sock, prev: IO.iodata_to_binary(encoded)}

      assert {:noreply, %Connection{name: ":1.100", prev: <<>>}, {:continue, :recv}} =
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

    test "classifies a missing error name", %{svr: svr} do
      log =
        assert_hello_error_reason(svr, :missing_error_name, fn reply_serial ->
          raw_error_reply(reply_serial, %{})
        end)

      assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :missing_error_name}"
    end

    test "classifies an invalid error name without logging it", %{svr: svr} do
      invalid_name = "invalid error name"

      log =
        assert_hello_error_reason(svr, :invalid_error_name, fn reply_serial ->
          raw_error_reply(reply_serial, %{error_name: invalid_name})
        end)

      refute log =~ invalid_name
      assert log =~ "D-Bus connection protocol stopped: {:hello_failed, :invalid_error_name}"
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
      assert <<>> == :sys.get_state(cli).prev
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
      cli = connect_until_ready(svr)
      :ok = :sys.suspend(cli)
      _ = :sys.replace_state(cli, fn state -> %{state | name: nil} end)
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
    test ":system parses unix:path= format" do
      # Test with a non-existent path to verify parsing works
      Application.put_env(
        :rebus,
        :system_bus_address,
        "unix:path=/tmp/nonexistent-test-system-bus"
      )

      # This will fail to connect but tests address parsing
      result = Rebus.connect(:system)

      # Should get a connection error, not a parsing error
      assert {:error, reason} = result
      assert reason != :no_system_bus_address

      # Clean up
      Application.delete_env(:rebus, :system_bus_address)
    end

    test ":system returns error when address is nil" do
      # Temporarily set address to nil
      Application.put_env(:rebus, :system_bus_address, nil)

      assert {:error, :no_system_bus_address} = Rebus.connect(:system)

      # Clean up
      Application.delete_env(:rebus, :system_bus_address)
    end

    test ":session parses unix:path= format" do
      # Test with a non-existent path to verify parsing works
      System.put_env("DBUS_SESSION_BUS_ADDRESS", "unix:path=/tmp/nonexistent-test-session-bus")

      # This will fail to connect but tests address parsing
      result = Rebus.connect(:session)

      # Should get a connection error, not a parsing error
      assert {:error, reason} = result
      assert reason != :no_session_bus_address

      # Clean up
      System.delete_env("DBUS_SESSION_BUS_ADDRESS")
    end

    test ":session returns error when DBUS_SESSION_BUS_ADDRESS is not set" do
      # Ensure the environment variable is not set
      original_value = System.get_env("DBUS_SESSION_BUS_ADDRESS")
      System.delete_env("DBUS_SESSION_BUS_ADDRESS")

      assert {:error, :no_session_bus_address} = Rebus.connect(:session)

      # Restore original value if it existed
      if original_value do
        System.put_env("DBUS_SESSION_BUS_ADDRESS", original_value)
      end
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

      [{serial, {_from, _timer_ref, request_ref, _monitor_ref}}] =
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
      invalid_message =
        Message.new!(:method_call,
          path: "/org/freedesktop/DBus",
          member: "BadBody",
          signature: "s",
          body: [42]
        )

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
    # The 'tap' process will receive all messages received by the test server.
    # The server does not respond to any messages unless instructed to do so.
    {:ok, svr} = start_supervised({Rebus.TestServer, tap: self()})
    %{svr: svr}
  end

  defp client_setup(%{svr: svr}) do
    {:ok, addr} = TestServer.get_listen_addr(svr)
    {:ok, cli} = Rebus.connect(addr)

    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = msg}
    handle_hello(msg, svr)

    %{cli: cli}
  end

  defp handle_hello(%Message{} = msg, svr) do
    reply =
      Rebus.Message.new!(
        :method_return,
        reply_serial: msg.serial,
        signature: "s",
        flags: [],
        body: [":1.100"]
      )

    :ok = TestServer.push(svr, reply)

    signal =
      Rebus.Message.new!(
        :signal,
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

  defp connect_until_hello(svr) do
    {:ok, addr} = TestServer.get_listen_addr(svr)
    {:ok, cli} = Rebus.connect(addr)
    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}} = hello}
    {cli, hello}
  end

  defp connect_until_ready(svr) do
    {cli, hello} = connect_until_hello(svr)
    handle_hello(hello, svr)
    assert wait_until(fn -> :sys.get_state(cli).name == ":1.100" end)
    cli
  end

  defp assert_hello_error_reason(svr, expected_reason, build_reply) do
    capture_log(fn ->
      {cli, hello} = connect_until_hello(svr)
      reply = build_reply.(hello.serial)
      {:ok, encoded} = Message.encode(reply)
      ref = Process.monitor(cli)

      :ok = TestServer.push_raw(svr, IO.iodata_to_binary(encoded))

      assert_receive {:DOWN, ^ref, :process, ^cli,
                      {:shutdown, {:hello_failed, ^expected_reason}}},
                     1_000
    end)
  end

  defp assert_missing_reply_serial(svr, type) do
    log =
      capture_log(fn ->
        cli = connect_until_ready(svr)
        ref = Process.monitor(cli)
        {:ok, encoded} = Message.encode(raw_reply(type, %{}))

        :ok = TestServer.push_raw(svr, IO.iodata_to_binary(encoded))

        assert_receive {:DOWN, ^ref, :process, ^cli,
                        {:shutdown, {:malformed_reply, :missing_reply_serial}}},
                       1_000
      end)

    assert log =~ "D-Bus connection protocol stopped: {:malformed_reply, :missing_reply_serial}"
    refute log =~ "%Rebus.Connection"
  end

  defp raw_error_reply(reply_serial, header_fields) do
    raw_reply(:error, Map.put(header_fields, :reply_serial, reply_serial))
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
end
