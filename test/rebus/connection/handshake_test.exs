defmodule Rebus.Connection.HandshakeTest do
  # The cookie cases replace $HOME for the duration of the test.
  use ExUnit.Case, async: false

  alias Rebus.Connection.Handshake
  alias Rebus.ScriptedTransport
  alias Rebus.TestImpl

  @guid "30313233343536373839616263646566"
  @other_guid "46454443424139383736353433323130"
  # The hex-encoded decimal uid EXTERNAL sends as its authorization identity.
  @auth_id "353031"
  @username "rebus-user"

  describe "EXTERNAL" do
    test "sends the NUL byte, the hex uid and BEGIN, and returns the peer GUID" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\n"])

      assert {:ok, %{guid: @guid, unix_fd_negotiated?: false, rest: <<>>}} = run(sock)

      assert ScriptedTransport.sent(sock) ==
               <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n" <> "BEGIN \r\n"
    end

    test "returns bytes read past BEGIN as the start of the message stream" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\nl\0\0\1frame"])

      assert {:ok, %{rest: "l\0\0\1frame"}} = run(sock)
    end

    test "accepts a response fragmented across several reads" do
      <<first::binary-size(16), second::binary-size(16)>> = @guid

      sock =
        ScriptedTransport.start([
          "O",
          "K ",
          first,
          {:error, {:timeout, second}},
          "\r",
          "\ntrailing"
        ])

      assert {:ok, %{guid: @guid, rest: "trailing"}} = run(sock)
      assert ScriptedTransport.remaining(sock) == []
    end

    test "verifies an expected GUID case-insensitively" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\n"])

      assert {:ok, %{guid: @guid}} = run(sock, expected_guid: String.upcase(@guid))
    end

    test "rejects a GUID the address did not advertise before writing BEGIN" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\n"])

      assert {:error, :guid_mismatch} = run(sock, expected_guid: @other_guid)
      assert ScriptedTransport.sent(sock) == <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n"
    end

    test "rejects a malformed OK response" do
      sock = ScriptedTransport.start(["OK not-a-guid\r\n"])

      assert {:error, :auth_failed} = run(sock)
    end
  end

  describe "NEGOTIATE_UNIX_FD" do
    test "reports agreement" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\n", "AGREE_UNIX_FD\r\n"])

      assert {:ok, %{unix_fd_negotiated?: true, rest: <<>>}} = run(sock, unix_fd_transport?: true)

      assert ScriptedTransport.sent(sock) ==
               <<0>> <>
                 "AUTH EXTERNAL #{@auth_id}\r\n" <> "NEGOTIATE_UNIX_FD\r\n" <> "BEGIN \r\n"
    end

    test "leaves the connection usable when the peer declines" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\nERROR not supported\r\nrest"])

      assert {:ok, %{unix_fd_negotiated?: false, rest: "rest"}} =
               run(sock, unix_fd_transport?: true)
    end

    test "treats an unrecognized answer as an authentication failure" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\n", "MAYBE_UNIX_FD\r\n"])

      assert {:error, :auth_failed} = run(sock, unix_fd_transport?: true)
    end
  end

  describe "REJECTED" do
    test "returns the advertised mechanisms when none can be attempted" do
      sock = ScriptedTransport.start(["REJECTED EXTERNAL KERBEROS_V4\r\n"])

      assert {:error, {:auth_rejected, ["EXTERNAL", "KERBEROS_V4"]}} = run(sock)
      assert ScriptedTransport.sent(sock) == <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n"
    end

    test "rejects a malformed mechanism list" do
      sock = ScriptedTransport.start(["REJECTED EXTERNAL\tANONYMOUS\r\n"])

      assert {:error, :auth_failed} = run(sock, allow_anonymous?: true)
    end
  end

  describe "ANONYMOUS" do
    test "is attempted when advertised and explicitly enabled" do
      sock = ScriptedTransport.start(["REJECTED ANONYMOUS\r\n", "OK #{@guid}\r\n"])

      assert {:ok, %{guid: @guid}} = run(sock, allow_anonymous?: true)

      assert ScriptedTransport.sent(sock) ==
               <<0>> <>
                 "AUTH EXTERNAL #{@auth_id}\r\n" <> "AUTH ANONYMOUS\r\n" <> "BEGIN \r\n"
    end

    test "is never attempted without the opt-in" do
      sock = ScriptedTransport.start(["REJECTED ANONYMOUS\r\n", "OK #{@guid}\r\n"])

      assert {:error, {:auth_rejected, ["ANONYMOUS"]}} = run(sock)
      refute ScriptedTransport.sent(sock) =~ "AUTH ANONYMOUS"
    end

    test "is never attempted when the peer did not advertise it" do
      sock = ScriptedTransport.start(["REJECTED EXTERNAL\r\n", "OK #{@guid}\r\n"])

      assert {:error, {:auth_rejected, ["EXTERNAL"]}} = run(sock, allow_anonymous?: true)
      refute ScriptedTransport.sent(sock) =~ "AUTH ANONYMOUS"
    end

    test "rejects a second rejection" do
      sock =
        ScriptedTransport.start(["REJECTED ANONYMOUS\r\n", "REJECTED EXTERNAL ANONYMOUS\r\n"])

      assert {:error, {:auth_rejected, ["EXTERNAL", "ANONYMOUS"]}} =
               run(sock, allow_anonymous?: true)
    end
  end

  describe "DBUS_COOKIE_SHA1" do
    @context "rebus_handshake_context"
    @cookie "0123456789abcdef"
    @server_challenge "server-challenge"

    test "sends the hex username, answers the challenge and completes" do
      with_private_keyring(fn uid ->
        sock =
          ScriptedTransport.start([
            "REJECTED DBUS_COOKIE_SHA1\r\n",
            "DATA #{challenge()}\r\n",
            "OK #{@guid}\r\ntrailing"
          ])

        assert {:ok, %{guid: @guid, rest: "trailing"}} =
                 run(sock, auth_id: auth_id(uid), identity: identity())

        prefix =
          <<0>> <>
            "AUTH EXTERNAL #{auth_id(uid)}\r\n" <>
            "AUTH DBUS_COOKIE_SHA1 #{Base.encode16(@username, case: :lower)}\r\n"

        sent = ScriptedTransport.sent(sock)
        assert String.starts_with?(sent, prefix)

        assert "DATA " <> tail =
                 binary_part(sent, byte_size(prefix), byte_size(sent) - byte_size(prefix))

        assert String.ends_with?(tail, "\r\nBEGIN \r\n")
        encoded = String.replace_suffix(tail, "\r\nBEGIN \r\n", "")
        assert encoded == String.downcase(encoded)
        assert {:ok, response} = Base.decode16(encoded, case: :lower)
        assert [client_challenge, digest] = :binary.split(response, " ", [:global])

        assert digest ==
                 :crypto.hash(:sha, [@server_challenge, ":", client_challenge, ":", @cookie])
                 |> Base.encode16(case: :lower)
      end)
    end

    test "treats a rejection after the response as terminal" do
      with_private_keyring(fn uid ->
        sock =
          ScriptedTransport.start([
            "REJECTED ANONYMOUS DBUS_COOKIE_SHA1\r\n",
            "DATA #{challenge()}\r\n",
            "REJECTED ANONYMOUS\r\n"
          ])

        assert {:error, :auth_failed} =
                 run(sock,
                   auth_id: auth_id(uid),
                   identity: identity(),
                   allow_anonymous?: true
                 )

        refute ScriptedTransport.sent(sock) =~ "AUTH ANONYMOUS"
      end)
    end

    test "treats a rejection of the mechanism itself as terminal" do
      with_private_keyring(fn uid ->
        sock =
          ScriptedTransport.start([
            "REJECTED ANONYMOUS DBUS_COOKIE_SHA1\r\n",
            "REJECTED ANONYMOUS\r\n"
          ])

        assert {:error, {:auth_rejected, ["ANONYMOUS"]}} =
                 run(sock,
                   auth_id: auth_id(uid),
                   identity: identity(),
                   allow_anonymous?: true
                 )

        refute ScriptedTransport.sent(sock) =~ "AUTH ANONYMOUS"
      end)
    end

    test "falls back to an advertised ANONYMOUS only before AUTH is sent" do
      sock =
        ScriptedTransport.start(["REJECTED ANONYMOUS DBUS_COOKIE_SHA1\r\n", "OK #{@guid}\r\n"])

      identity = TestImpl.identity(username: fn _timeout -> {:error, :exit_status} end)

      assert {:ok, %{guid: @guid}} = run(sock, identity: identity, allow_anonymous?: true)

      assert ScriptedTransport.sent(sock) ==
               <<0>> <>
                 "AUTH EXTERNAL #{@auth_id}\r\n" <> "AUTH ANONYMOUS\r\n" <> "BEGIN \r\n"
    end

    test "stops when no username is available and anonymous is not enabled" do
      sock = ScriptedTransport.start(["REJECTED ANONYMOUS DBUS_COOKIE_SHA1\r\n"])
      identity = TestImpl.identity(username: fn _timeout -> {:error, :exit_status} end)

      assert {:error, :auth_cookie_unavailable} = run(sock, identity: identity)
      assert ScriptedTransport.sent(sock) == <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n"
    end

    test "stops when the keyring cannot answer the challenge" do
      sock =
        ScriptedTransport.start([
          "REJECTED ANONYMOUS DBUS_COOKIE_SHA1\r\n",
          "DATA #{challenge()}\r\n"
        ])

      assert {:error, :auth_cookie_unavailable} =
               run(sock, identity: identity(), allow_anonymous?: true)

      refute ScriptedTransport.sent(sock) =~ "AUTH ANONYMOUS"
      refute ScriptedTransport.sent(sock) =~ "DATA "
    end
  end

  describe "line framing" do
    test "refuses a line longer than the bounded auth line size" do
      sock = ScriptedTransport.start([String.duplicate("A", 1_025) <> "\r\n"])

      assert {:error, :auth_failed} = run(sock)
    end

    test "refuses an unterminated line that outgrows the bound" do
      sock = ScriptedTransport.start([String.duplicate("A", 1_100)])

      assert {:error, :auth_failed} = run(sock)
    end

    test "reports a closed peer" do
      sock = ScriptedTransport.start([])

      assert {:error, :closed} = run(sock)
    end
  end

  describe "deadlines" do
    test "refuses to write anything once the setup deadline has passed" do
      sock = ScriptedTransport.start(["OK #{@guid}\r\n"])

      assert {:error, :read_timeout} =
               Handshake.run(
                 sock,
                 @auth_id,
                 System.monotonic_time(:millisecond) - 1,
                 1_000,
                 options()
               )

      assert ScriptedTransport.sent(sock) == <<>>
    end

    test "reports a receive timeout as :read_timeout" do
      sock = ScriptedTransport.start([{:error, :timeout}])

      assert {:error, :read_timeout} = run(sock)
    end

    test "reports an empty partial receive timeout as :read_timeout" do
      sock = ScriptedTransport.start([{:error, {:timeout, <<>>}}])

      assert {:error, :read_timeout} = run(sock)
    end
  end

  defp run(sock, overrides \\ []) do
    {auth_id, overrides} = Keyword.pop(overrides, :auth_id, @auth_id)

    Handshake.run(
      sock,
      auth_id,
      System.monotonic_time(:millisecond) + 1_000,
      1_000,
      options(overrides)
    )
  end

  defp options(overrides \\ []) do
    struct!(
      %Handshake.Options{
        transport: ScriptedTransport,
        identity: Rebus.TestImpl.Identity,
        write_timeout: 1_000
      },
      overrides
    )
  end

  defp identity, do: TestImpl.identity(username: fn _timeout -> {:ok, @username} end)

  defp auth_id(uid), do: :binary.encode_hex(Integer.to_string(uid))

  defp challenge,
    do: Base.encode16("#{@context} 1 #{@server_challenge}", case: :lower)

  defp with_private_keyring(fun) do
    home =
      Path.join(
        System.tmp_dir!(),
        "rebus-handshake-#{System.unique_integer([:positive, :monotonic])}"
      )

    keyring = Path.join(home, ".dbus-keyrings")
    previous_home = System.get_env("HOME")

    try do
      :ok = File.mkdir_p(keyring)
      :ok = File.chmod(keyring, 0o700)
      :ok = File.write(Path.join(keyring, @context), "1 0 #{@cookie}\n")
      :ok = File.chmod(Path.join(keyring, @context), 0o600)
      System.put_env("HOME", home)
      fun.(File.stat!(home).uid)
    after
      if is_nil(previous_home),
        do: System.delete_env("HOME"),
        else: System.put_env("HOME", previous_home)

      _ = File.rm_rf(home)
    end
  end
end
