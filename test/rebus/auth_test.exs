defmodule Rebus.AuthTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Rebus.Auth
  alias Rebus.Connection
  alias Rebus.Message
  alias Rebus.TestImpl

  @guid "30313233343536373839414243444546"
  @lexical_cookie "aaaaaaaaaaaaaaaa"
  @kernel_cookie "bbbbbbbbbbbbbbbb"

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
            "REJECTED lower case",
            "REJECTED DBUS_COOKIE_SHA1\tANONYMOUS",
            "REJECTED " <> Enum.join(List.duplicate("ANONYMOUS", 65), " "),
            "REJECTED " <> String.duplicate("A", 65)
          ] do
        assert {:error, :auth_failed} = Auth.parse_rejected(malformed)
      end

      assert {:error, :auth_failed} = Auth.parse_rejected("DATA not-a-rejection")
    end

    test "tolerates the spacing implementations actually send" do
      assert {:ok, ["EXTERNAL", "DBUS_COOKIE_SHA1", "ANONYMOUS"]} =
               Auth.parse_rejected("REJECTED EXTERNAL DBUS_COOKIE_SHA1 ANONYMOUS")

      assert {:ok, ["DBUS_COOKIE_SHA1"]} = Auth.parse_rejected("REJECTED DBUS_COOKIE_SHA1 ")

      assert {:ok, ["EXTERNAL", "ANONYMOUS"]} =
               Auth.parse_rejected("REJECTED EXTERNAL  ANONYMOUS")

      assert {:ok, ["ANONYMOUS"]} = Auth.parse_rejected("REJECTED ext*ernal ANONYMOUS")
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

    test "uses a private keyring when HOME is a relative symlink to its directory" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        symlinked_home = home <> "-relative-symlink"

        with_home(symlinked_home, [symlinked_home], fn ->
          :ok = File.ln_s(Path.basename(home), symlinked_home)

          assert {:ok, _response} = cookie_response(context, home)
        end)
      end)
    end

    # On macOS `System.tmp_dir!/0` already sits under `/var`, itself a symlink
    # to `/private/var`, so this case is exercised incidentally there; it earns
    # its place on Linux, where `/tmp` is a real directory.
    test "uses a private keyring when an intermediate HOME component is a symlink" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        linked_parent = home <> "-parent-symlink"
        linked_home = Path.join(linked_parent, Path.basename(home))

        with_home(linked_home, [linked_parent], fn ->
          :ok = File.ln_s(Path.dirname(home), linked_parent)

          assert {:ok, _response} = cookie_response(context, home)
        end)
      end)
    end

    test "follows a bounded chain of HOME symlinks" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        first_link = home <> "-link-1"
        second_link = home <> "-link-2"

        with_home(first_link, [second_link, first_link], fn ->
          :ok = File.ln_s(home, first_link)
          :ok = File.ln_s(first_link, second_link)

          # The single-link case proves the fixture, so the two-link result
          # below is about the chain rather than about the links themselves.
          assert {:ok, _one_link} = cookie_response(context, home)

          System.put_env("HOME", second_link)
          assert {:ok, _two_links} = cookie_response(context, home)
        end)
      end)
    end

    test "follows a chain of HOME symlinks written with a trailing separator" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        first_link = home <> "-slash-link-1"
        second_link = home <> "-slash-link-2"

        with_home(second_link, [second_link, first_link], fn ->
          :ok = File.ln_s(home, first_link)
          :ok = File.ln_s(first_link, second_link)

          # `lstat` follows a trailing separator, so an unnormalised HOME would
          # skip resolution entirely. The two spellings must agree.
          assert {:ok, _plain} = cookie_response(context, home)

          System.put_env("HOME", second_link <> "/")
          assert {:ok, _trailing_slash} = cookie_response(context, home)
        end)
      end)
    end

    test "rejects a HOME symlink chain longer than the limit" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        # `link-n` reaches the real home in n hops, so `link-8` sits exactly on
        # the eight-link limit and `link-9` is one hop beyond it.
        links = Enum.map(1..9, &(home <> "-chain-link-#{&1}"))

        with_home(home, Enum.reverse(links), fn ->
          Enum.reduce(links, home, fn link, target ->
            :ok = File.ln_s(target, link)
            link
          end)

          System.put_env("HOME", Enum.at(links, 7))
          assert {:ok, _eight_links} = cookie_response(context, home)

          reasons =
            logged_reasons(fn ->
              System.put_env("HOME", Enum.at(links, 8))
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)

              # `lstat` follows a trailing separator or `.` component, so
              # without normalisation these spellings would skip resolution and
              # accept the nine-link chain. Pinning the bound under them pins
              # the normalisation.
              System.put_env("HOME", Enum.at(links, 8) <> "/")
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)

              System.put_env("HOME", Enum.at(links, 8) <> "/.")
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)

              System.put_env("HOME", Enum.at(links, 8) <> "/./")
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)
            end)

          assert reasons == List.duplicate("home_unsafe", 4)
        end)
      end)
    end

    test "rejects a HOME whose path or link target ends in .." do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        # `sub/..` names the valid keyring home itself, and `up -> ..` from
        # inside it does the same, so only the final-`..` rule can reject these:
        # a build without it accepts both, and the positive control proves the
        # fixture. The rule exists because reaching that parent means following
        # the component before `..`, so the string checked would not be the
        # string used.
        sub = Path.join(home, "sub")
        :ok = File.mkdir_p(sub)
        :ok = File.ln_s("..", Path.join(sub, "up"))

        with_home(home, [], fn ->
          assert {:ok, _control} = cookie_response(context, home)

          reasons =
            logged_reasons(fn ->
              System.put_env("HOME", sub <> "/..")
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)

              System.put_env("HOME", sub <> "/../")
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)

              System.put_env("HOME", Path.join(sub, "up"))
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)
            end)

          assert reasons == List.duplicate("home_unsafe", 3)
        end)
      end)
    end

    test "reads the kernel's directory for a relative HOME link target that steps upward" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        base = home <> "-lexical"

        try do
          # `base/a -> x/y`, so `base/a/link -> ../b/dir` names `base/x/b/dir`
          # to the kernel and `base/b/dir` to a lexical expansion. Both are
          # valid keyring homes carrying different cookies, so the response
          # digest says which of them was read.
          :ok = File.mkdir_p(Path.join(base, "x/y"))
          :ok = File.ln_s("x/y", Path.join(base, "a"))
          :ok = File.ln_s("../b/dir", Path.join(base, "x/y/link"))
          populate_keyring(Path.join(base, "b/dir"), context, @lexical_cookie)
          populate_keyring(Path.join(base, "x/b/dir"), context, @kernel_cookie)

          with_home(Path.join(base, "a/link"), [], fn ->
            assert {:ok, response} = cookie_response(context, home)
            assert cookie_behind(response) == :kernel
          end)
        after
          File.rm_rf(base)
        end
      end)
    end

    test "reads the kernel's directory when HOME itself contains a .. component" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        base = home <> "-dotdot-home"

        try do
          # `base/aa -> xx/yy`, so the kernel reads `base/aa/..` as `base/xx`
          # and `HOME=base/aa/../link` is `base/xx/link -> bb/dir`, that is
          # `base/xx/bb/dir`. Expanding the target against the home's dirname
          # would collapse `base/aa/..` to `base` and name `base/bb/dir`.
          :ok = File.mkdir_p(Path.join(base, "xx/yy"))
          :ok = File.ln_s("xx/yy", Path.join(base, "aa"))
          :ok = File.ln_s("bb/dir", Path.join(base, "xx/link"))
          populate_keyring(Path.join(base, "bb/dir"), context, @lexical_cookie)
          populate_keyring(Path.join(base, "xx/bb/dir"), context, @kernel_cookie)

          with_home(Path.join(base, "aa/../link"), [], fn ->
            assert {:ok, response} = cookie_response(context, home)
            assert cookie_behind(response) == :kernel
          end)
        after
          File.rm_rf(base)
        end
      end)
    end

    test "reads the kernel's directory through an absolute link target containing .." do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        base = home <> "-dotdot-target"

        try do
          # `HOME` holds no `..` at all here: the first link's absolute target
          # carries it, and the hop after that is relative. The kernel reaches
          # `base/xx/bb/dir`; a lexical expansion of the second hop against
          # `base/aa/..` would reach `base/bb/dir`.
          :ok = File.mkdir_p(Path.join(base, "xx/yy"))
          :ok = File.ln_s("xx/yy", Path.join(base, "aa"))
          :ok = File.ln_s("bb/dir", Path.join(base, "xx/link2"))
          :ok = File.ln_s(Path.join(base, "aa/../link2"), Path.join(base, "home"))
          populate_keyring(Path.join(base, "bb/dir"), context, @lexical_cookie)
          populate_keyring(Path.join(base, "xx/bb/dir"), context, @kernel_cookie)

          with_home(Path.join(base, "home"), [], fn ->
            assert {:ok, response} = cookie_response(context, home)
            assert cookie_behind(response) == :kernel
          end)
        after
          File.rm_rf(base)
        end
      end)
    end

    test "accepts a relative HOME link target that steps back down through .." do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        base = home <> "-benign-dotdot"

        try do
          # `sub2/../real` is what the kernel resolves for any path, so it must
          # resolve here too rather than being refused for containing `..`.
          :ok = File.mkdir_p(Path.join(base, "sub2"))
          populate_keyring(Path.join(base, "real"), context, cookie)
          :ok = File.ln_s("sub2/../real", Path.join(base, "link"))

          with_home(Path.join(base, "link"), [], fn ->
            assert {:ok, _response} = cookie_response(context, home)
          end)
        after
          File.rm_rf(base)
        end
      end)
    end

    test "rejects a HOME symlink that does not resolve to a directory" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        file_link = home <> "-file-symlink"
        dangling_link = home <> "-dangling-symlink"

        with_home(file_link, [file_link, dangling_link], fn ->
          regular = Path.join(home, "not-a-directory")
          :ok = File.write(regular, "")
          :ok = File.ln_s(regular, file_link)
          :ok = File.ln_s(Path.join(home, "missing"), dangling_link)

          reasons =
            logged_reasons(fn ->
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)

              System.put_env("HOME", dangling_link)
              assert {:error, :auth_cookie_unavailable} = cookie_response(context, home)
            end)

          assert reasons == List.duplicate("home_unsafe", 2)
        end)
      end)
    end

    test "rejects a HOME symlink whose target directory is group writable" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        symlinked_home = home <> "-writable-symlink"

        with_home(symlinked_home, [symlinked_home], fn ->
          %File.Stat{uid: uid, mode: mode} = File.stat!(home)
          :ok = File.ln_s(home, symlinked_home)
          :ok = File.chmod(home, 0o775)

          reasons =
            logged_reasons(fn ->
              assert {:error, :auth_cookie_unavailable} =
                       Auth.cookie_response(
                         "user",
                         uid,
                         Base.encode16("#{context} 1 server-challenge", case: :lower)
                       )
            end)

          assert reasons == ["home_unsafe"]

          :ok = File.chmod(home, Bitwise.band(mode, 0o7777))
        end)
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

        assert logged_reasons(fn ->
                 assert {:error, :auth_cookie_unavailable} =
                          Auth.cookie_response(
                            "user",
                            File.stat!(keyring).uid,
                            Base.encode16("#{context} 1 challenge", case: :lower)
                          )
               end) == ["keyring_unsafe"]
      end)

      with_private_keyring(context, cookie, fn home ->
        path = Path.join([home, ".dbus-keyrings", context])
        target = Path.join(home, "cookie-target")
        :ok = File.write(target, "1 0 #{cookie}\n")
        :ok = File.rm(path)
        :ok = File.ln_s(target, path)

        assert logged_reasons(fn ->
                 assert {:error, :auth_cookie_unavailable} =
                          Auth.cookie_response(
                            "user",
                            File.stat!(home).uid,
                            Base.encode16("#{context} 1 challenge", case: :lower)
                          )
               end) == ["cookie_unsafe"]
      end)
    end

    test "fails closed for malformed cookie fields and bounded keyring records" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      # The argument guard is the only path that reaches the boundary without a
      # category of its own, so it reports the safety net rather than nothing.
      assert logged_reasons(fn ->
               assert {:error, :auth_cookie_unavailable} = Auth.cookie_response(:user, 0, "00")
             end) == ["internal"]

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

        unavailable = fn cookie_id, challenge, expected_reason ->
          assert logged_reasons(fn ->
                   assert {:error, :auth_cookie_unavailable} = response.(cookie_id, challenge)
                 end) == [expected_reason]
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
        unavailable.("1", "challenge", "keyring_malformed")

        :ok = File.write(path, "2 0 #{cookie}\n")
        unavailable.("1", "challenge", "cookie_missing")

        :ok = File.write(path, "1 0 #{cookie}\n1 1 #{cookie}\n")
        unavailable.("1", "challenge", "cookie_duplicate")

        :ok = File.write(path, "not-a-cookie-record\n")
        unavailable.("1", "challenge", "cookie_missing")

        :ok = File.write(path, "1 0 zz\n")
        unavailable.("1", "challenge", "keyring_malformed")

        :ok = File.write(path, "1 0 ABCD\n")
        assert {:ok, _response} = response.("1", "challenge")

        :ok = File.write(path, "1 0 f\n")
        unavailable.("1", "challenge", "keyring_malformed")

        :ok = File.write(path, String.duplicate("A", 1_025))
        unavailable.("1", "challenge", "cookie_missing")

        # The reference implementation bounds a cookie context at 255 bytes, so
        # one of exactly that length must still be usable.
        long_context = String.duplicate("a", 255)
        :ok = File.write(Path.join(keyring, long_context), "1 0 #{cookie}\n")
        :ok = File.chmod(Path.join(keyring, long_context), 0o600)

        assert {:ok, _response} =
                 Auth.cookie_response(
                   "user",
                   uid,
                   Base.encode16("#{long_context} 1 challenge", case: :lower)
                 )

        assert {:error, :auth_failed} =
                 Auth.cookie_response(
                   "user",
                   uid,
                   Base.encode16("#{String.duplicate("a", 256)} 1 challenge", case: :lower)
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
                  {[name: anonymous_name, allow_anonymous: true],
                   %{
                     addr: anonymous_addr,
                     impl: Rebus.Impl.build(identity: unavailable_username)
                   }}}
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
                  {[name: rejecting_name],
                   %{addr: rejecting_addr, impl: Rebus.Impl.build(identity: blocked_username)}}}
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

  describe "cookie diagnostics" do
    test "reports a home that is not an absolute path" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        # `System.user_home/0` cannot be made to return nil from the suite, so
        # the other half of the category - a home that is not absolute - stands
        # for it.
        with_home("relative/home", [], fn ->
          assert_unavailable("home_missing", fn -> cookie_response(context, home) end)
        end)
      end)
    end

    test "reports a home whose path ends in .." do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        sub = Path.join(home, "sub")
        :ok = File.mkdir_p(sub)

        with_home(sub <> "/..", [], fn ->
          assert_unavailable("home_unsafe", fn -> cookie_response(context, home) end)
        end)
      end)
    end

    test "reports a keyring directory that is not private" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        :ok = File.chmod(Path.join(home, ".dbus-keyrings"), 0o755)

        assert_unavailable("keyring_unsafe", fn -> cookie_response(context, home) end)
      end)
    end

    test "reports a cookie file that is not private" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        :ok = File.chmod(Path.join([home, ".dbus-keyrings", context]), 0o644)

        assert_unavailable("cookie_unsafe", fn -> cookie_response(context, home) end)
      end)
    end

    # The size checks compare two `lstat`s with the bytes actually read, so a
    # deterministic mismatch needs a writer racing the read. The classification
    # is exercised directly instead, and the boundary log is proved by the
    # categories above that do reach it through the file system.
    test "reports a cookie file that changed under the read" do
      assert Auth.cookie_unchanged(5, 5, "abcde") == :ok
      assert Auth.cookie_unchanged(5, 4, "abcde") == {:error, {:unavailable, :cookie_changed}}
      assert Auth.cookie_unchanged(5, 5, "abcd") == {:error, {:unavailable, :cookie_changed}}
    end

    test "reports a cookie file that cannot be read" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        path = Path.join([home, ".dbus-keyrings", context])
        :ok = File.chmod(path, 0o000)

        # root ignores the permission bits, so the open succeeds there and the
        # category cannot be provoked at all.
        if File.stat!(path).uid == 0 do
          assert {:ok, _response} = cookie_response(context, home)
        else
          assert_unavailable("cookie_unreadable", fn -> cookie_response(context, home) end)
        end
      end)
    end

    test "reports a keyring holding more records than the bound allows" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        records = Enum.map_join(1..257, "\n", fn id -> "#{id} 0 #{cookie}" end) <> "\n"
        :ok = File.write(Path.join([home, ".dbus-keyrings", context]), records)

        assert_unavailable("keyring_malformed", fn -> cookie_response(context, home) end)
      end)
    end

    test "reports a cookie ID no record carries" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        :ok = File.write(Path.join([home, ".dbus-keyrings", context]), "4242 0 #{cookie}\n")

        assert_unavailable("cookie_missing", fn -> cookie_response(context, home) end)
      end)
    end

    test "reports a cookie ID more than one record carries" do
      context = "rebus_test_context"
      cookie = "0123456789abcdef"

      with_private_keyring(context, cookie, fn home ->
        :ok =
          File.write(
            Path.join([home, ".dbus-keyrings", context]),
            "1 0 #{cookie}\n1 1 #{cookie}\n"
          )

        assert_unavailable("cookie_duplicate", fn -> cookie_response(context, home) end)
      end)
    end

    # No supported platform can be made to report a stat without POSIX owner
    # and mode metadata, so the fail-closed classification is exercised
    # directly rather than through the file system.
    test "reports a platform without POSIX owner and mode metadata" do
      assert Auth.posix_metadata(0, 0o700) == :ok
      assert Auth.posix_metadata(:undefined, 0o700) == {:error, {:unavailable, :unsupported}}
      assert Auth.posix_metadata(0, :undefined) == {:error, {:unavailable, :unsupported}}
    end

    test "never lets a sentinel value reach an error or a log" do
      home = Path.join(Rebus.TestTmp.path("auth"), "SENTINELHOME-keyring")
      keyring = Path.join(home, ".dbus-keyrings")
      context = "sentinelctx"
      cookie_id = "424242"
      cookie = Base.encode16("SENTINEL", case: :lower)
      challenge = "sentinelchallenge"
      username = "sentineluser"
      path = Path.join(keyring, context)
      previous_home = System.get_env("HOME")

      sentinels = ["SENTINEL", "sentinel", cookie_id, cookie, home, context, challenge, username]

      try do
        :ok = File.mkdir_p(keyring)
        :ok = File.chmod(keyring, 0o700)
        :ok = File.write(path, "#{cookie_id} 0 #{cookie}\n")
        :ok = File.chmod(path, 0o600)
        System.put_env("HOME", home)
        uid = File.stat!(home).uid

        respond = fn id ->
          Auth.cookie_response(
            username,
            uid,
            Base.encode16("#{context} #{id} #{challenge}", case: :lower)
          )
        end

        drive = fn fun ->
          {result, log} = with_log(fun)

          Enum.each(sentinels, fn sentinel ->
            refute log =~ sentinel
            refute inspect(result) =~ sentinel
          end)

          {result, log}
        end

        # A success logs nothing at all, so the whole fixture is covered by the
        # refutations above only once a failure has something to disclose.
        assert {{:ok, _response}, ""} = drive.(fn -> respond.(cookie_id) end)

        assert {{:error, :auth_cookie_unavailable}, missing} =
                 drive.(fn -> respond.("999999") end)

        assert missing =~ "unavailable reason=cookie_missing"

        records = Enum.map_join(1..257, "\n", fn id -> "#{id} 0 #{cookie}" end) <> "\n"
        :ok = File.write(path, records)

        assert {{:error, :auth_cookie_unavailable}, malformed} =
                 drive.(fn -> respond.(cookie_id) end)

        assert malformed =~ "unavailable reason=keyring_malformed"

        :ok = File.chmod(path, 0o644)

        assert {{:error, :auth_cookie_unavailable}, unsafe_cookie} =
                 drive.(fn -> respond.(cookie_id) end)

        assert unsafe_cookie =~ "unavailable reason=cookie_unsafe"

        :ok = File.chmod(keyring, 0o755)

        assert {{:error, :auth_cookie_unavailable}, unsafe_keyring} =
                 drive.(fn -> respond.(cookie_id) end)

        assert unsafe_keyring =~ "unavailable reason=keyring_unsafe"
      after
        if is_nil(previous_home),
          do: System.delete_env("HOME"),
          else: System.put_env("HOME", previous_home)

        _ = File.rm_rf(home)
      end
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

        assert logged_reasons(fn ->
                 assert {:error, :auth_cookie_unavailable} = Rebus.connect(addr)
               end) == ["keyring_unsafe"]

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

        assert logged_reasons(fn ->
                 assert {:error, :auth_cookie_unavailable} =
                          Rebus.connect(addr, allow_anonymous: true)
               end) == ["keyring_unsafe"]

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

          assert [reason] =
                   logged_reasons(fn ->
                     assert {:error, :auth_cookie_unavailable} =
                              Rebus.connect(addr, allow_anonymous: true)
                   end)

          assert reason in ["cookie_unsafe", "cookie_missing"]
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
        :ok = :socket.send(peer, "REJECTED #{sentinel}\r\n")
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

  # The boundary emits exactly one warning per failed cookie attempt, so the
  # occurrence count is asserted rather than mere presence.
  defp assert_unavailable(reason, fun) do
    log = capture_log(fn -> assert {:error, :auth_cookie_unavailable} = fun.() end)

    assert log =~ "D-Bus cookie authentication unavailable reason=#{reason}"
    assert length(Regex.scan(~r/unavailable reason=/, log)) == 1
  end

  defp logged_reasons(fun) do
    log = capture_log(fun)

    ~r/unavailable reason=(\w+)/
    |> Regex.scan(log)
    |> Enum.map(fn [_match, reason] -> reason end)
  end

  # Points `HOME` at `path`, restores the previous value, and removes every
  # link the test created beside the fixture home.
  defp with_home(path, links, fun) do
    previous_home = System.get_env("HOME")

    try do
      System.put_env("HOME", path)
      fun.()
    after
      if is_nil(previous_home),
        do: System.delete_env("HOME"),
        else: System.put_env("HOME", previous_home)

      Enum.each(links, &File.rm/1)
    end
  end

  defp cookie_response(context, home) do
    Auth.cookie_response(
      "user",
      File.stat!(home).uid,
      Base.encode16("#{context} 1 server-challenge", case: :lower)
    )
  end

  # Replays the digest with the client challenge the response carries, so a
  # test with two candidate keyring homes can say which one was read.
  defp cookie_behind(response) do
    {:ok, decoded} = Base.decode16(response, case: :mixed)
    [client_challenge, digest] = :binary.split(decoded, " ", [:global])

    candidates = [lexical: @lexical_cookie, kernel: @kernel_cookie]

    Enum.find_value(candidates, :unknown, fn {name, cookie} ->
      expected =
        :crypto.hash(:sha, ["server-challenge", ":", client_challenge, ":", cookie])
        |> Base.encode16(case: :lower)

      if expected == digest, do: name
    end)
  end

  # Builds a keyring that passes every owner and mode check, at an arbitrary
  # directory rather than at the per-test fixture home.
  defp populate_keyring(dir, context, cookie) do
    keyring = Path.join(dir, ".dbus-keyrings")

    :ok = File.mkdir_p(keyring)
    :ok = File.chmod(dir, 0o755)
    :ok = File.chmod(keyring, 0o700)
    :ok = File.write(Path.join(keyring, context), "1 0 #{cookie}\n")
    :ok = File.chmod(Path.join(keyring, context), 0o600)
  end

  defp with_private_keyring(context, cookie, fun) do
    home = Rebus.TestTmp.path("auth")

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
