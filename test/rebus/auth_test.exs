defmodule Rebus.AuthTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Rebus.Auth
  alias Rebus.Connection
  alias Rebus.Message
  alias Rebus.TestImpl

  @guid "30313233343536373839414243444546"

  describe "REJECTED parser" do
    test "retains only bounded valid mechanism names" do
      assert {:ok, []} = Auth.parse_rejected("REJECTED")

      assert {:ok, ["ANONYMOUS", "DBUS_COOKIE_SHA1", "EXTERNAL"]} =
               Auth.parse_rejected("REJECTED ANONYMOUS DBUS_COOKIE_SHA1 EXTERNAL")

      assert {:ok, nine_mechanisms} =
               Auth.parse_rejected("REJECTED " <> Enum.join(List.duplicate("MECH", 9), " "))

      assert length(nine_mechanisms) == 9

      for malformed <- [
            "REJECTED ",
            "REJECTED DBUS_COOKIE_SHA1 ",
            "REJECTED DBUS_COOKIE_SHA1\tANONYMOUS",
            "REJECTED " <> Enum.join(List.duplicate("ANONYMOUS", 65), " "),
            "REJECTED " <> String.duplicate("A", 65)
          ] do
        assert {:error, :auth_failed} = Auth.parse_rejected(malformed)
      end

      assert {:error, :auth_failed} = Auth.parse_rejected("DATA not-a-rejection")
    end
  end

  describe "DBUS_COOKIE_SHA1" do
    test "uses a private temporary keyring and computes the exact wire response" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"
      server_challenge = "server-challenge"

      with_private_keyring(context, cookie, fn _home ->
        {server, addr} =
          start_auth_server(fn peer ->
            assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED ANONYMOUS DBUS_COOKIE_SHA1 EXTERNAL\r\n")

            assert "AUTH DBUS_COOKIE_SHA1 " <> encoded_username = receive_line(peer)
            assert {:ok, username} = Base.decode16(encoded_username, case: :mixed)
            assert username != <<>>

            challenge = Base.encode16("#{context} 1 #{server_challenge}", case: :lower)
            :ok = :socket.send(peer, "DATA " <> challenge <> "\r\n")

            assert "DATA " <> encoded_response = receive_line(peer)
            assert encoded_response == String.downcase(encoded_response)
            assert {:ok, response} = Base.decode16(encoded_response, case: :mixed)
            [client_challenge, received_digest] = :binary.split(response, " ", [:global])

            expected_digest =
              :crypto.hash(:sha, [server_challenge, ":", client_challenge, ":", cookie])
              |> Base.encode16(case: :lower)

            assert received_digest == expected_digest
            :ok = :socket.send(peer, "OK #{@guid}\r\n")
            assert "BEGIN " = receive_line(peer)
            reply_to_hello(peer)
            wait_for_finish()
          end)

        assert {:ok, connection} = Rebus.connect(addr, read_timeout: 1_000)
        assert :ok = Rebus.close(connection)
        send(server.pid, :finish)
        assert :ok = Task.await(server, 2_000)
      end)
    end

    test "uses a private keyring when HOME is a symlink to its final directory" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        symlinked_home = home <> "-symlink"

        try do
          :ok = File.ln_s(home, symlinked_home)
          System.put_env("HOME", symlinked_home)

          assert {:ok, _response} =
                   Auth.cookie_response(
                     "user",
                     File.stat!(home).uid,
                     Base.encode16("#{context} 1 server-challenge", case: :lower)
                   )
        after
          System.put_env("HOME", home)
          :ok = File.rm(symlinked_home)
        end
      end)
    end

    test "rejects malicious challenges and unsafe keyring paths without retaining details" do
      assert {:error, :auth_failed} =
               Auth.cookie_response("user", 0, Base.encode16("../ 1 challenge", case: :lower))

      assert {:error, :auth_failed} =
               Auth.cookie_response(
                 "user",
                 0,
                 Base.encode16("org.example 1 challenge", case: :lower)
               )

      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        keyring = Path.join(home, ".dbus-keyrings")
        :ok = File.chmod(keyring, 0o755)

        assert {:error, :auth_cookie_unavailable} =
                 Auth.cookie_response(
                   "user",
                   File.stat!(keyring).uid,
                   Base.encode16("#{context} 1 challenge", case: :lower)
                 )
      end)

      with_private_keyring(context, cookie, fn home ->
        path = Path.join([home, ".dbus-keyrings", context])
        target = Path.join(home, "cookie-target")
        :ok = File.write(target, "1 0 #{cookie}\n")
        :ok = File.rm(path)
        :ok = File.ln_s(target, path)

        assert {:error, :auth_cookie_unavailable} =
                 Auth.cookie_response(
                   "user",
                   File.stat!(home).uid,
                   Base.encode16("#{context} 1 challenge", case: :lower)
                 )
      end)
    end

    test "fails closed for malformed cookie fields and bounded keyring records" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      assert {:error, :auth_cookie_unavailable} = Auth.cookie_response(:user, 0, "00")
      assert {:error, :auth_failed} = Auth.cookie_response("", 0, "")
      assert {:error, :auth_failed} = Auth.cookie_response("user", 0, "")

      with_private_keyring(context, cookie, fn home ->
        keyring = Path.join(home, ".dbus-keyrings")
        path = Path.join(keyring, context)
        uid = File.stat!(keyring).uid

        response = fn cookie_id, challenge ->
          Auth.cookie_response(
            "user",
            uid,
            Base.encode16("#{context} #{cookie_id} #{challenge}", case: :lower)
          )
        end

        assert {:ok, _response} =
                 Auth.cookie_response(
                   "user",
                   uid,
                   Base.encode16("#{context} 1 challenge", case: :lower) |> String.upcase()
                 )

        :ok = File.write(path, "4242 0 #{cookie}\n")
        assert {:ok, _response} = response.("4242", "challenge")

        :ok =
          File.write(
            path,
            "unrelated malformed record\n" <>
              String.duplicate("x", 1_025) <>
              "\n" <>
              "1 0 #{cookie}\n"
          )

        assert {:ok, _response} = response.("1", "challenge")

        bounded_records =
          Enum.map_join(1..256, "\n", fn id -> "#{id} 0 #{cookie}" end) <> "\n"

        :ok = File.write(path, bounded_records)
        assert {:ok, _response} = response.("1", "challenge")

        too_many_records =
          Enum.map_join(1..257, "\n", fn id -> "#{id} 0 #{cookie}" end) <> "\n"

        :ok = File.write(path, too_many_records)
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        :ok = File.write(path, "2 0 #{cookie}\n")
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        :ok = File.write(path, "1 0 #{cookie}\n1 1 #{cookie}\n")
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        :ok = File.write(path, "not-a-cookie-record\n")
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        :ok = File.write(path, "1 0 zz\n")
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        :ok = File.write(path, "1 0 ABCD\n")
        assert {:ok, _response} = response.("1", "challenge")

        :ok = File.write(path, "1 0 f\n")
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        :ok = File.write(path, String.duplicate("A", 1_025))
        assert {:error, :auth_cookie_unavailable} = response.("1", "challenge")

        assert {:error, :auth_failed} =
                 Auth.cookie_response(
                   "user",
                   uid,
                   Base.encode16("#{String.duplicate("a", 129)} 1 challenge", case: :lower)
                 )

        assert {:error, :auth_failed} =
                 response.(String.duplicate("1", 21), "challenge")

        assert {:error, :auth_failed} = response.("1", String.duplicate("a", 513))
        assert {:error, :auth_failed} = response.("1", "")
      end)
    end

    test "does not downgrade after a cookie response is rejected" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn _home ->
        {server, addr} =
          start_auth_server(fn peer ->
            assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1\r\n")
            assert "AUTH DBUS_COOKIE_SHA1 " <> _username = receive_line(peer)

            challenge = Base.encode16("#{context} 1 server-challenge", case: :lower)
            :ok = :socket.send(peer, "DATA " <> challenge <> "\r\n")
            assert "DATA " <> _response = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1 ANONYMOUS\r\n")

            assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
            :ok
          end)

        assert {:error, :auth_failed} =
                 Rebus.connect(addr, allow_anonymous: true, read_timeout: 1_000)

        assert :ok = Task.await(server, 2_000)
      end)
    end

    test "uses the aggregate setup deadline for a dribbling cookie challenge" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn _home ->
        {server, addr} =
          start_auth_server(fn peer ->
            assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1\r\n")
            assert "AUTH DBUS_COOKIE_SHA1 " <> _username = receive_line(peer)
            :ok = :socket.send(peer, "DATA 726562")
            assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
            :ok
          end)

        assert {:error, :read_timeout} = Rebus.connect(addr, read_timeout: 100)
        assert :ok = Task.await(server, 2_000)
      end)
    end

    test "keeps cookie protocol failures terminal when anonymous is enabled" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      for {reply, after_cookie_data?, expected} <- [
            {"REJECTED DBUS_COOKIE_SHA1 ANONYMOUS\r\n", false,
             {:auth_rejected, ["DBUS_COOKIE_SHA1", "ANONYMOUS"]}},
            {"DATA 00\r\n", false, :auth_failed},
            {"ERROR peer-auth-payload-sentinel\r\n", true, :auth_failed},
            {"ERROR peer-auth-payload-sentinel\r\n", false, :auth_failed}
          ] do
        with_private_keyring(context, cookie, fn _home ->
          {server, addr} =
            start_auth_server(fn peer ->
              assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
              :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1 ANONYMOUS\r\n")
              assert "AUTH DBUS_COOKIE_SHA1 " <> _username = receive_line(peer)

              if after_cookie_data? do
                challenge = Base.encode16("#{context} 1 server-challenge", case: :lower)
                :ok = :socket.send(peer, "DATA " <> challenge <> "\r\n")
                assert "DATA " <> _response = receive_line(peer)
                :ok = :socket.send(peer, reply)
              else
                :ok = :socket.send(peer, reply)
              end

              assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
              :ok
            end)

          assert {:error, ^expected} = Rebus.connect(addr, allow_anonymous: true)
          assert :ok = Task.await(server, 2_000)
        end)
      end
    end

    test "uses anonymous directly only when username lookup fails before cookie AUTH" do
      parent = self()
      anonymous_name = :rebus_anonymous_username_connection
      rejecting_name = :rebus_rejecting_username_connection

      unavailable_username =
        TestImpl.identity(anonymous_name, username: fn _timeout -> {:error, :exit_status} end)

      {anonymous_server, anonymous_addr} =
        start_auth_server(fn peer ->
          assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
          :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1 ANONYMOUS\r\n")
          assert "AUTH ANONYMOUS" = receive_line(peer)
          :ok = :socket.send(peer, "OK #{@guid}\r\n")
          assert "BEGIN " = receive_line(peer)
          reply_to_hello(peer)
          send(parent, :anonymous_established)
          wait_for_finish()
        end)

      assert {:ok, connection} =
               DynamicSupervisor.start_child(
                 Rebus.ConnectionSupervisor,
                 {Connection,
                  addr: anonymous_addr,
                  name: anonymous_name,
                  allow_anonymous: true,
                  __impl__: %{identity: unavailable_username}}
               )

      assert_receive :anonymous_established, 1_000
      assert :ok = Rebus.close(connection)
      send(anonymous_server.pid, :finish)
      assert :ok = Task.await(anonymous_server, 2_000)

      {rejecting_server, rejecting_addr} =
        start_auth_server(fn peer ->
          assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
          :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1 ANONYMOUS\r\n")
          assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
          :ok
        end)

      blocked_username =
        TestImpl.identity(rejecting_name,
          username: fn _timeout ->
            send(parent, :rejecting_username_lookup)

            receive do
              :fail_username_lookup -> {:error, :exit_status}
            end
          end
        )

      assert {:ok, rejected_connection} =
               DynamicSupervisor.start_child(
                 Rebus.ConnectionSupervisor,
                 {Connection,
                  addr: rejecting_addr,
                  name: rejecting_name,
                  __impl__: %{identity: blocked_username}}
               )

      monitor_ref = Process.monitor(rejected_connection)
      assert_receive :rejecting_username_lookup, 1_000
      send(rejected_connection, :fail_username_lookup)

      assert_receive {:DOWN, ^monitor_ref, :process, ^rejected_connection,
                      {:shutdown, :auth_cookie_unavailable}},
                     1_000

      assert :ok = Task.await(rejecting_server, 2_000)
    end
  end

  describe "ANONYMOUS" do
    test "does not send CANCEL when cookie credentials are unavailable without opt-in" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        :ok = File.chmod(Path.join(home, ".dbus-keyrings"), 0o755)

        {server, addr} =
          start_auth_server(fn peer ->
            assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1\r\n")
            assert "AUTH DBUS_COOKIE_SHA1 " <> _username = receive_line(peer)

            challenge = Base.encode16("#{context} 1 server-challenge", case: :lower)
            :ok = :socket.send(peer, "DATA " <> challenge <> "\r\n")
            assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
            :ok
          end)

        assert {:error, :auth_cookie_unavailable} = Rebus.connect(addr)
        assert :ok = Task.await(server, 2_000)
      end)
    end

    test "keeps unavailable cookie credentials terminal after cookie AUTH" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        :ok = File.chmod(Path.join(home, ".dbus-keyrings"), 0o755)

        {server, addr} =
          start_auth_server(fn peer ->
            assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1\r\n")
            assert "AUTH DBUS_COOKIE_SHA1 " <> _username = receive_line(peer)

            challenge = Base.encode16("#{context} 1 server-challenge", case: :lower)
            :ok = :socket.send(peer, "DATA " <> challenge <> "\r\n")
            assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
            :ok
          end)

        assert {:error, :auth_cookie_unavailable} = Rebus.connect(addr, allow_anonymous: true)
        assert :ok = Task.await(server, 2_000)
      end)
    end

    test "does not downgrade for a peer-chosen unavailable cookie context or ID" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      for challenge <- [
            Base.encode16("bogus_peer_context 1 server-challenge", case: :lower),
            Base.encode16("#{context} 999999 server-challenge", case: :lower)
          ] do
        with_private_keyring(context, cookie, fn _home ->
          {server, addr} =
            start_auth_server(fn peer ->
              assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
              :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1 ANONYMOUS\r\n")
              assert "AUTH DBUS_COOKIE_SHA1 " <> _username = receive_line(peer)
              :ok = :socket.send(peer, "DATA " <> challenge <> "\r\n")
              assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
              :ok
            end)

          assert {:error, :auth_cookie_unavailable} = Rebus.connect(addr, allow_anonymous: true)
          assert :ok = Task.await(server, 2_000)
        end)
      end
    end

    test "uses a buffered anonymous response before the peer closes" do
      {server, addr} =
        start_auth_server(fn peer ->
          assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)

          :ok =
            :socket.send(
              peer,
              "REJECTED ANONYMOUS\r\n" <> "OK #{@guid}\r\n"
            )

          assert "AUTH ANONYMOUS" = receive_line(peer)
          assert "BEGIN " = receive_line(peer)
          :ok
        end)

      assert {:error, reason} = Rebus.connect(addr, allow_anonymous: true)
      assert reason in [:closed, :econnreset]
      assert :ok = Task.await(server, 2_000)
    end

    test "requires an explicit opt-in and works over TCP when enabled" do
      {rejecting_server, rejecting_addr} =
        start_auth_server(fn peer ->
          assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
          :ok = :socket.send(peer, "REJECTED ANONYMOUS\r\n")
          assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
          :ok
        end)

      assert {:error, {:auth_rejected, ["ANONYMOUS"]}} = Rebus.connect(rejecting_addr)
      assert :ok = Task.await(rejecting_server, 2_000)

      {anonymous_server, anonymous_addr} =
        start_auth_server(fn peer ->
          assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
          :ok = :socket.send(peer, "REJECTED ANONYMOUS\r\n")
          assert "AUTH ANONYMOUS" = receive_line(peer)
          :ok = :socket.send(peer, "OK #{@guid}\r\n")
          assert "BEGIN " = receive_line(peer)
          reply_to_hello(peer)
          wait_for_finish()
        end)

      assert {:ok, connection} = Rebus.connect(anonymous_addr, allow_anonymous: true)
      assert :ok = Rebus.close(connection)
      send(anonymous_server.pid, :finish)
      assert :ok = Task.await(anonymous_server, 2_000)
    end

    test "completes a peer-to-peer connection that has no bus driver" do
      {server, addr} =
        start_auth_server(fn peer ->
          assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
          :ok = :socket.send(peer, "REJECTED ANONYMOUS\r\n")
          assert "AUTH ANONYMOUS" = receive_line(peer)
          :ok = :socket.send(peer, "OK #{@guid}\r\n")
          assert "BEGIN " = receive_line(peer)

          # A peer-to-peer endpoint implements no bus driver, so the first
          # frame it ever sees must be the application's own method call.
          assert %Message{
                   type: :method_call,
                   header_fields: %{member: "Ping"},
                   serial: serial
                 } = receive_message(peer)

          reply =
            Message.new!(:method_return,
              reply_serial: serial,
              serial: 1,
              signature: "s",
              body: ["pong"]
            )

          {:ok, encoded} = Message.encode(reply)
          :ok = :socket.send(peer, encoded)
          wait_for_finish()
        end)

      assert {:ok, connection} = Rebus.connect(addr, bus: false, allow_anonymous: true)

      ping =
        Message.new!(:method_call,
          path: "/org/example/Peer",
          interface: "org.example.Peer",
          member: "Ping"
        )

      assert {:ok, %Message{type: :method_return, body: ["pong"]}} =
               Rebus.call(connection, ping, 2_000)

      assert is_nil(:sys.get_state(connection).name)
      assert :ok = Rebus.close(connection)
      send(server.pid, :finish)
      assert :ok = Task.await(server, 2_000)
    end

    test "reports anonymous rejection and malformed responses without peer payloads" do
      for {reply, expected} <- [
            {"REJECTED ANONYMOUS\r\n", {:auth_rejected, ["ANONYMOUS"]}},
            {"ERROR peer-auth-payload-sentinel\r\n", :auth_failed}
          ] do
        {server, addr} =
          start_auth_server(fn peer ->
            assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
            :ok = :socket.send(peer, "REJECTED ANONYMOUS\r\n")
            assert "AUTH ANONYMOUS" = receive_line(peer)
            :ok = :socket.send(peer, reply)
            assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
            :ok
          end)

        assert {:error, ^expected} = Rebus.connect(addr, allow_anonymous: true)
        assert :ok = Task.await(server, 2_000)
      end
    end
  end

  test "malformed peer authentication data never appears in errors or logs" do
    sentinel = "peer-auth-payload-sentinel"

    {server, addr} =
      start_auth_server(fn peer ->
        assert "\0AUTH EXTERNAL " <> _external_id = receive_line(peer)
        :ok = :socket.send(peer, "REJECTED DBUS_COOKIE_SHA1 #{sentinel}\r\n")
        assert {:error, :closed} = :socket.recv(peer, 0, [], 1_000)
        :ok
      end)

    log =
      capture_log(fn ->
        assert {:error, :auth_failed} = Rebus.connect(addr)
      end)

    refute log =~ sentinel
    assert :ok = Task.await(server, 2_000)
  end

  test "rejects a non-boolean anonymous opt-in" do
    assert {:error, :invalid_allow_anonymous} =
             Rebus.connect(%{family: :inet, addr: {127, 0, 0, 1}, port: 1}, allow_anonymous: :yes)
  end

  defp with_private_keyring(context, cookie, fun) do
    home =
      Path.join(
        System.tmp_dir!(),
        "rebus-auth-#{System.unique_integer([:positive, :monotonic])}"
      )

    keyring = Path.join(home, ".dbus-keyrings")
    previous_home = System.get_env("HOME")

    try do
      :ok = File.mkdir_p(keyring)
      :ok = File.chmod(keyring, 0o700)
      :ok = File.write(Path.join(keyring, context), "1 0 #{cookie}\n")
      :ok = File.chmod(Path.join(keyring, context), 0o600)
      System.put_env("HOME", home)
      fun.(home)
    after
      if is_nil(previous_home),
        do: System.delete_env("HOME"),
        else: System.put_env("HOME", previous_home)

      _ = File.rm_rf(home)
    end
  end

  defp start_auth_server(handshake) do
    parent = self()
    ready_ref = make_ref()

    task =
      Task.async(fn ->
        {:ok, listener} = :socket.open(:inet, :stream, :default)
        :ok = :socket.bind(listener, %{family: :inet, addr: :loopback, port: 0})
        :ok = :socket.listen(listener, 1)
        {:ok, addr} = :socket.sockname(listener)
        send(parent, {ready_ref, addr})
        {:ok, peer} = :socket.accept(listener, 1_000)

        try do
          handshake.(peer)
        after
          :ok = :socket.close(peer)
          :ok = :socket.close(listener)
        end
      end)

    assert_receive {^ready_ref, addr}, 1_000
    {task, addr}
  end

  defp receive_line(peer) do
    buffer = Process.get(:rebus_auth_server_buffer, <<>>)

    case :binary.match(buffer, "\r\n") do
      {size, 2} ->
        line = binary_part(buffer, 0, size)
        rest_size = byte_size(buffer) - size - 2
        Process.put(:rebus_auth_server_buffer, binary_part(buffer, size + 2, rest_size))
        line

      :nomatch ->
        {:ok, data} = :socket.recv(peer, 0, [], 1_000)
        Process.put(:rebus_auth_server_buffer, buffer <> data)
        receive_line(peer)
    end
  end

  defp reply_to_hello(peer) do
    hello = receive_message(peer)
    assert %Message{type: :method_call, header_fields: %{member: "Hello"}, serial: serial} = hello

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        serial: 1,
        signature: "s",
        body: [":1.100"]
      )

    {:ok, encoded} = Message.encode(reply)
    :ok = :socket.send(peer, encoded)
  end

  defp receive_message(peer) do
    buffer = Process.get(:rebus_auth_server_buffer, <<>>)

    case Message.parse(buffer) do
      {:ok, message, rest} ->
        Process.put(:rebus_auth_server_buffer, rest)
        message

      nil ->
        {:ok, data} = :socket.recv(peer, 0, [], 1_000)
        Process.put(:rebus_auth_server_buffer, buffer <> data)
        receive_message(peer)
    end
  end

  defp wait_for_finish do
    receive do
      :finish -> :ok
    after
      2_000 -> :ok
    end
  end
end
