defmodule RebusTest do
  use ExUnit.Case
  doctest Rebus

  alias Rebus.Connection
  alias Rebus.Message
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
  end

  describe "Unix socket connections" do
    test "can be established with unix socket" do
      # Use a short path to avoid Unix socket path length limit (108 bytes)
      socket_path = "/tmp/rebus_test_#{:erlang.unique_integer([:positive])}.sock"
      {:ok, svr} = start_supervised({Rebus.TestServer, tap: self(), family: :local, path: socket_path})
      
      {:ok, _cli} = Rebus.connect(%{family: :local, path: socket_path})

      assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}
    end
  end

  describe "Connection address parsing" do
    test ":system parses unix:path= format" do
      # Test with a non-existent path to verify parsing works
      Application.put_env(:rebus, :system_bus_address, "unix:path=/tmp/nonexistent-test-system-bus")
      
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
end
