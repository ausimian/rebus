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

  describe "Methods" do
    setup [:server_setup, :client_setup]

    test "block when called", %{cli: cli, svr: svr} do
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
      task = Task.async(fn -> Connection.send(cli, method) end)
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
