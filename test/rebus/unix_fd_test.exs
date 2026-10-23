defmodule Rebus.UnixFDTest do
  use ExUnit.Case, async: false

  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Rights
  alias Rebus.{Message, Service, TestImpl, TestServer, UnixFD}
  alias Rebus.Service.Call
  alias Rebus.UnixFD.Raw

  @connection_name :rebus_unix_fd_test_connection
  @files_path "/org/example/Files"
  @files Rebus.Interface.new!("org.example.Files",
           methods: [{"Take", in: [fd: "h"]}, {"Echo", in: [fd: "h"], out: [fd: "h"]}]
         )

  @moduletag skip:
               if(
                 :os.type() in [{:unix, :linux}, {:unix, :darwin}],
                 do: false,
                 else: "SCM_RIGHTS coverage is supported on Linux and macOS"
               )

  setup do
    path = Rebus.TestTmp.path("ufd")

    {:ok, server} =
      start_supervised(%{
        id: {:unix_fd_server, path},
        start: {TestServer, :start_link, [[tap: self(), family: :local, path: path]]}
      })

    {:ok, address} = TestServer.get_listen_addr(server)

    # No test in this module drives the identity lookup, so the connection
    # replays a cached uid instead of looking it up again.
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
    assert {:error, :invalid_descriptor} = Raw.close(-1)
    assert {:error, :invalid_descriptor} = Raw.close(:not_a_descriptor)
    assert {:error, :ebadf} = Raw.close(1_000_000)
  end

  test "the private OTP close primitive is still exported" do
    Code.ensure_loaded(:prim_file)

    assert function_exported?(:prim_file, :file_desc_to_ref, 2),
           "Rebus.UnixFD.Raw closes owned descriptors with the private OTP function " <>
             ":prim_file.file_desc_to_ref/2, and this OTP release no longer exports it. " <>
             "The migration plan in lib/rebus/unix_fd/raw.ex now applies."
  end

  test "closes a received socket descriptor without disturbing the original", %{
    server: server,
    connection: connection
  } do
    {:ok, sock} = :socket.open(:local, :stream)
    on_exit(fn -> _ = :socket.close(sock) end)
    {:ok, fd} = :socket.getopt(sock, {:otp, :fd})

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "ReceiveSocketFD"
      )

    task = call_task(connection, method)

    assert_receive {^server,
                    %Message{serial: serial, header_fields: %{member: "ReceiveSocketFD"}}},
                   1_000

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "hh",
        body: [0, 1],
        fds: [fd, fd]
      )

    :ok = TestServer.push_with_fds(server, reply, [fd, fd])

    assert {:ok, %Message{type: :method_return, unix_fds: [first, second]}} =
             Task.await(task, 1_000)

    # SCM_RIGHTS hands the receiver an independent duplicate per right, each
    # in its own handle, so closing them leaves this process's own socket
    # open.
    first_fd = raw!(first)
    second_fd = raw!(second)
    assert first_fd != fd
    assert second_fd != fd
    assert first_fd != second_fd
    assert identity!(first) == UnixFD.__fd_identity__(fd)
    assert :ok = UnixFD.close(first)
    assert :ok = UnixFD.close(second)

    # Read-only: /dev/fd is /proc/self/fd on Linux and the fdesc filesystem on
    # macOS, and exists?/1 is a stat, not an open, so this cannot adopt or
    # disturb a reused number.  Pins that closing the handles closed the
    # numbers they held rather than a duplicate.  It sits directly after the
    # closes, before any other syscall in this test frees a number, because
    # it holds only while nothing in the VM has reused either number; the
    # module is async: false for that reason.
    refute File.exists?("/dev/fd/#{first_fd}")
    refute File.exists?("/dev/fd/#{second_fd}")

    assert {:ok, %{family: :local}} = :socket.sockname(sock)
    # Part of the survival check, not cleanup: a socket whose descriptor was
    # closed out from under it fails this close with :ebadf.
    assert :ok = :socket.close(sock)
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
    assert :ok = Raw.close(received)
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

  test "delivers a received descriptor in a handle the caller owns", %{
    server: server,
    connection: connection
  } do
    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "ReceiveFD"
      )

    task = call_task(connection, method)

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

    # `call_task/2` checks that the caller owned the handle on arrival.
    assert {:ok, %Message{type: :method_return, unix_fds: [received]}} = Task.await(task, 1_000)
    assert raw!(received) != fd
    assert identity!(received) == UnixFD.__fd_identity__(fd)
    assert :ok = UnixFD.close(received)
    assert {:ok, 1} = UnixFD.__close_count__(received)
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

    task = call_task(connection, method)

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
            {:dbus_error,
             %Message{
               type: :error,
               header_fields: %{error_name: "org.example.FailedWithFD"},
               unix_fds: [received]
             }}} = Task.await(task, 1_000)

    assert identity!(received) == UnixFD.__fd_identity__(fd)
    assert :ok = UnixFD.close(received)
  end

  test "closes an FD reply handed to a caller that timed out but lives, on its cancel", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    # The connection keeps the request open well past its caller's deadline,
    # so only the caller gives up on it, and reports each descriptor it reads.
    TestImpl.install(connection,
      request_timeout_slack: fn -> 5_000 end,
      recvmsg: fn sock, length, control_size, flags, timeout ->
        result = :socket.recvmsg(sock, length, control_size, flags, timeout)

        with {:ok, %{ctrl: [%{type: :rights, data: <<received::native-signed-32>>}]}} <- result do
          send(parent, {:received_fd, received})
        end

        result
      end
    )

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "TimedOutAliasFD"
      )

    caller =
      spawn(fn ->
        result = Rebus.call(connection, method, 1_000)
        send(parent, {:timed_out_alias_result, result})

        receive do
          :stop_timed_out_alias_caller -> :ok
          message -> send(parent, {:unexpected_message, message})
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

    # Hold the connection while the reply reaches its socket and the caller
    # times out, so it reads the reply, and hands it to a call alias that no
    # longer accepts anything, before the cancel the caller sends as it gives
    # up.
    :ok = :sys.suspend(connection)
    :ok = TestServer.push_with_fds(server, reply, [fd])

    assert eventually(fn ->
             Process.info(connection, :message_queue_len) != {:message_queue_len, 0}
           end)

    assert_receive {:timed_out_alias_result, {:error, :timeout}}, 2_000
    assert Process.alive?(caller)

    log =
      ExUnit.CaptureLog.capture_log(fn ->
        :ok = :sys.resume(connection)
        assert_receive {:received_fd, received}, 1_000

        # The cancel closes it at once: nothing collects any garbage here.
        assert eventually(fn -> not File.exists?("/dev/fd/#{received}") end)
        assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
        elsewhere(fn -> :sys.get_state(connection) end)
      end)

    # Handed over rather than dropped as an orphan.
    refute log =~ "orphaned"
    refute_received {:unexpected_message, _message}
    assert Process.alive?(connection)
    send(caller, :stop_timed_out_alias_caller)
  end

  test "leaves a reply's handles with a caller that received them once its cancel window ends",
       %{server: server, connection: connection} do
    parent = self()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    method =
      Message.new!(:method_call, path: "/test", interface: "test.interface", member: "KeptFD")

    caller =
      spawn(fn ->
        send(parent, {:kept_result, Rebus.call(connection, method, 1_000)})
        Process.sleep(:infinity)
      end)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "KeptFD"}}},
                   1_000

    reply =
      Message.new!(:method_return, reply_serial: serial, signature: "h", body: [0], fds: [fd])

    :ok = TestServer.push_with_fds(server, reply, [fd])
    assert_receive {:kept_result, {:ok, %Message{unix_fds: [received]}}}, 1_000

    # Past the deadline and the connection's grace for a cancel, the caller
    # still owns an open handle and nothing has closed it.
    assert eventually(
             fn -> elsewhere(fn -> :sys.get_state(connection).handed_over.entries == %{} end) end,
             400
           )

    assert {:ok, ^caller} = UnixFD.owner(received)
    assert {:ok, 0} = UnixFD.__close_count__(received)
    assert identity!(received) == UnixFD.__fd_identity__(fd)

    Process.exit(caller, :kill)
    assert_closed_once(received)
  end

  test "closes a reply whose caller cancelled before it arrived", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    TestImpl.install(connection,
      request_timeout_slack: fn -> 5_000 end,
      recvmsg: fn sock, length, control_size, flags, timeout ->
        result = :socket.recvmsg(sock, length, control_size, flags, timeout)

        with {:ok, %{ctrl: [%{type: :rights, data: <<received::native-signed-32>>}]}} <- result do
          send(parent, {:received_fd, received})
        end

        result
      end
    )

    method =
      Message.new!(:method_call, path: "/test", interface: "test.interface", member: "LateFD")

    caller =
      spawn(fn ->
        send(parent, {:late_result, Rebus.call(connection, method, 200)})
        Process.sleep(:infinity)
      end)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "LateFD"}}},
                   1_000

    assert_receive {:late_result, {:error, :timeout}}, 1_000

    # The cancel has withdrawn the call, so the reply correlates to nothing.
    assert eventually(fn ->
             elsewhere(fn -> :sys.get_state(connection).pending.entries == %{} end)
           end)

    reply =
      Message.new!(:method_return, reply_serial: serial, signature: "h", body: [0], fds: [fd])

    log =
      ExUnit.CaptureLog.capture_log(fn ->
        :ok = TestServer.push_with_fds(server, reply, [fd])
        assert_receive {:received_fd, received}, 1_000
        assert eventually(fn -> not File.exists?("/dev/fd/#{received}") end)

        # The connection logs as it finishes with the frame.
        elsewhere(fn -> :sys.get_state(connection) end)
      end)

    assert log =~ "orphaned"
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
    assert Process.alive?(connection)
    Process.exit(caller, :kill)
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
    assert_closed_once(received)
    assert Process.whereis(connection_name) == nil

    refute Enum.any?(DynamicSupervisor.which_children(Rebus.ConnectionSupervisor), fn
             {_id, ^connection, _type, _modules} -> true
             _child -> false
           end)
  end

  test "Rebus.close closes descriptors held by unanswered inbound calls", %{
    server: server,
    connection: connection
  } do
    fd = hold_inbound_call_fd!(server, connection)

    ref = Process.monitor(connection)
    assert :ok = Rebus.close(connection)
    assert_receive {:DOWN, ^ref, :process, ^connection, :shutdown}, 1_000
    assert_closed_once(fd)
  end

  test "a killed connection closes every handle it owns", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    borrowed = hold_inbound_call_fd!(server, connection)
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    # A descriptor held for a frame still being read, as well as the one an
    # unanswered call lent this process.
    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{iov: ["l"], ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]},
        [],
        1_000
      )

    assert eventually(fn -> :sys.get_state(connection).inbound_fds.fds != [] end)
    [partial] = :sys.get_state(connection).inbound_fds.fds

    # :kill skips terminate/2, so only the handles' owner monitor can close
    # them.
    ref = Process.monitor(connection)
    Process.exit(connection, :kill)
    assert_receive {:DOWN, ^ref, :process, ^connection, :killed}, 1_000

    assert_closed_once(borrowed)
    assert_closed_once(partial)
    assert no_new_fds?(before_fds)
  end

  test "a transport stop closes descriptors held by unanswered inbound calls", %{
    server: server,
    connection: connection
  } do
    fd = hold_inbound_call_fd!(server, connection)

    ref = Process.monitor(connection)
    :ok = :socket.shutdown(:sys.get_state(server).cli_sock, :write)
    assert_receive {:DOWN, ^ref, :process, ^connection, {:shutdown, :closed}}, 1_000
    assert_closed_once(fd)
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

        task = call_task(connection, method)

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
        assert raw!(received) != fd
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

  test "closes descriptors held for a frame rejected over the size cap" do
    path = Rebus.TestTmp.path("ufd-cap")

    {:ok, server} =
      start_supervised(%{
        id: {:unix_fd_cap_server, path},
        start: {TestServer, :start_link, [[tap: self(), family: :local, path: path]]}
      })

    {:ok, address} = TestServer.get_listen_addr(server)

    {:ok, connection} =
      Rebus.connect(address,
        max_message_size: 4_096,
        __impl__: %{identity: TestImpl.CachedIdentity}
      )

    on_exit(fn ->
      if Process.alive?(connection), do: Rebus.close(connection)
    end)

    assert :sys.get_state(connection).unix_fd_negotiated?
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    # A frame declared one byte over the cap.
    body_length = 4_096 - 16 + 1
    fixed_header = <<?l, 4, 0, 1, body_length::little-32, 1::little-32, 0::little-32>>

    # The descriptor rides with the first half of the fixed header, so the
    # connection holds it before the frame's size is known.
    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [binary_part(fixed_header, 0, 8)],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    assert eventually(fn -> :sys.get_state(connection).inbound_fds.fds != [] end)
    [received] = :sys.get_state(connection).inbound_fds.fds
    ref = Process.monitor(connection)

    log =
      ExUnit.CaptureLog.capture_log(fn ->
        :ok = :socket.send(:sys.get_state(server).cli_sock, binary_part(fixed_header, 8, 8))

        assert_receive {:DOWN, ^ref, :process, ^connection, {:shutdown, :message_too_large}},
                       1_000
      end)

    assert log =~ "D-Bus connection protocol stopped: :message_too_large"
    assert_closed_once(received)
  end

  test "reads coalesced frames in one recvmsg once descriptors are negotiated", %{
    server: server,
    connection: connection
  } do
    parent = self()
    assert :sys.get_state(connection).unix_fd_negotiated?
    {:ok, handler_ref} = Rebus.add_signal_handler(connection)

    # The server may still be writing setup traffic such as NameAcquired. A
    # completed round trip proves the connection has read everything sent
    # before it, so the only bytes left to read are the ones sent below.
    sync = Message.new!(:method_call, path: "/test", interface: "test.interface", member: "Sync")
    task = Task.async(fn -> Rebus.call(connection, sync, 1_000) end)
    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "Sync"}}}, 1_000
    :ok = TestServer.push(server, Message.new!(:method_return, reply_serial: serial))
    assert {:ok, %Message{type: :method_return}} = Task.await(task, 1_000)

    :ok =
      TestImpl.install(connection,
        recvmsg: fn sock, length, control_size, flags, timeout ->
          result = :socket.recvmsg(sock, length, control_size, flags, timeout)

          case result do
            {:ok, %{iov: iov}} ->
              send(parent, {:negotiated_recvmsg, length, IO.iodata_length(iov)})

            _no_data ->
              :ok
          end

          result
        end
      )

    frames =
      for member <- ["First", "Second", "Third"], into: <<>> do
        signal = Message.new!(:signal, path: "/test", interface: "test.interface", member: member)
        {:ok, encoded} = Message.encode(signal)
        IO.iodata_to_binary(encoded)
      end

    :ok = :socket.send(:sys.get_state(server).cli_sock, frames, [], 1_000)

    for member <- ["First", "Second", "Third"] do
      assert_receive {^handler_ref, %Message{header_fields: %{member: ^member}}}, 1_000
    end

    # One zero-length read takes every frame the single send queued, rather
    # than a fixed header and a remainder per frame.
    size = byte_size(frames)
    assert_received {:negotiated_recvmsg, 0, ^size}
    refute_received {:negotiated_recvmsg, _length, _size}
  end

  test "attributes a coalesced descriptor to the frame that declares it", %{
    server: server,
    connection: connection
  } do
    {:ok, handler_ref} = Rebus.add_signal_handler(connection)

    method =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "CoalescedFD"
      )

    task = call_task(connection, method)

    assert_receive {^server, %Message{serial: serial, header_fields: %{member: "CoalescedFD"}}},
                   1_000

    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    leading =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "BeforeFDReply"
      )

    reply =
      Message.new!(:method_return,
        reply_serial: serial,
        signature: "h",
        body: [0],
        fds: [fd]
      )

    {:ok, leading_encoded} = Message.encode(leading)
    {:ok, reply_encoded} = Message.encode(%{reply | serial: 77})

    # One write, one control message: the descriptor rides with bytes that
    # begin with a frame declaring none, and belongs to the reply after it.
    :ok =
      :socket.sendmsg(
        :sys.get_state(server).cli_sock,
        %{
          iov: [IO.iodata_to_binary(leading_encoded), IO.iodata_to_binary(reply_encoded)],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    assert_receive {^handler_ref, %Message{header_fields: %{member: "BeforeFDReply"}}}, 1_000
    assert {:ok, %Message{type: :method_return, unix_fds: [received]}} = Task.await(task, 1_000)
    assert identity!(received) == UnixFD.__fd_identity__(fd)
    assert :ok = UnixFD.close(received)
  end

  test "delivers descriptors with consecutive replies in separate reads", %{
    server: server,
    connection: connection
  } do
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    for member <- ["FirstFD", "SecondFD"] do
      method =
        Message.new!(:method_call, path: "/test", interface: "test.interface", member: member)

      task = call_task(connection, method)

      assert_receive {^server, %Message{serial: serial, header_fields: %{member: ^member}}},
                     1_000

      reply =
        Message.new!(:method_return, reply_serial: serial, signature: "h", body: [0], fds: [fd])

      :ok = TestServer.push_with_fds(server, reply, [fd])
      assert {:ok, %Message{unix_fds: [received]}} = Task.await(task, 1_000)
      assert :ok = UnixFD.close(received)
    end
  end

  test "Rebus.Bus closes descriptors sent with the driver's replies", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    replies = [
      fn serial ->
        Message.new!(:method_return, reply_serial: serial, signature: "u", body: [1], fds: [fd])
      end,
      fn serial ->
        Message.new!(:error,
          reply_serial: serial,
          error_name: "org.freedesktop.DBus.Error.AccessDenied",
          signature: "s",
          body: ["denied"],
          fds: [fd]
        )
      end
    ]

    results =
      for reply <- replies do
        task =
          Task.async(fn ->
            Rebus.Bus.request_name(connection, "org.example.Fd", [], owner: nil)
          end)

        assert_receive {^server,
                        %Message{serial: serial, header_fields: %{member: "RequestName"}}},
                       1_000

        :ok = TestServer.push_with_fds(server, reply.(serial), [fd])
        Task.await(task, 1_000)
      end

    assert [
             {:ok, :primary_owner},
             {:error, {:bus_error, "org.freedesktop.DBus.Error.AccessDenied"}}
           ] = results

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
                        error_name: "org.freedesktop.DBus.Error.UnknownObject"
                      }
                    }},
                   1_000

    assert Process.alive?(connection)
    assert eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)
  end

  test "lends a call's descriptor to its handler until an answer completes the call", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {:ok, ref} = Service.export(connection, @files_path, [@files])

    for answer <- [
          &Service.reply(&1, []),
          &Service.reply_error(&1, "org.example.Error.Refused", "no"),
          &Service.reply_message(&1, Message.new!(:method_return, reply_serial: 1))
        ] do
      serial = push_fd_call!(server, "Take")

      assert_receive {^ref, %Call{serial: ^serial, unix_fds: [borrowed]} = call}, 1_000
      assert call.message.unix_fds == [borrowed]
      assert {:ok, ^connection} = UnixFD.owner(borrowed)

      # The handler may use it while the call is pending.
      assert {:ok, {:ok, _identity}} = UnixFD.with_fd(borrowed, &UnixFD.__fd_identity__/1)

      assert :ok = answer.(call)
      assert_receive {^server, %Message{header_fields: %{reply_serial: ^serial}}}, 1_000

      # Completing the call closed it, and the handler's alias reads closed.
      assert_closed_once(borrowed)
      assert no_new_fds?(before_fds)
    end
  end

  test "lets a handler close a borrowed handle without a second close", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()

    {_ref, %Call{serial: serial, unix_fds: [borrowed]} = call} =
      fd_call!(server, connection, "Take")

    assert :ok = UnixFD.close(borrowed)
    assert :ok = Service.reply(call, [])
    assert_receive {^server, %Message{header_fields: %{reply_serial: ^serial}}}, 1_000

    assert {:error, :closed} = UnixFD.close(borrowed)
    assert {:ok, 1} = UnixFD.__close_count__(borrowed)
    assert no_new_fds?(before_fds)
  end

  test "closes the descriptor of a call that asked for no reply once it is answered", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {:ok, ref} = Service.export(connection, @files_path, [@files])
    serial = push_fd_call!(server, "Take", flags: [:no_reply_expected])

    assert_receive {^ref, %Call{serial: ^serial, no_reply?: true, unix_fds: [borrowed]} = call},
                   1_000

    assert UnixFD.open?(borrowed)
    assert :ok = Service.reply(call, [])
    assert_closed_once(borrowed)
    assert no_new_fds?(before_fds)
    assert {:error, :unknown_call} = Service.reply(call, [])
    refute_receive {^server, %Message{header_fields: %{reply_serial: _}}}, 100
  end

  test "closes a borrowed descriptor when its call expires", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {_ref, %Call{serial: serial}} = fd_call!(server, connection, "Take", call_timeout: 50)

    assert_receive {^server,
                    %Message{
                      type: :error,
                      header_fields: %{
                        reply_serial: ^serial,
                        error_name: "org.freedesktop.DBus.Error.NoReply"
                      }
                    }},
                   1_000

    assert no_new_fds?(before_fds)

    # One that asked for no reply expires as quietly as it would be answered.
    push_fd_call!(server, "Take", flags: [:no_reply_expected])
    assert_receive {_ref, %Call{no_reply?: true, unix_fds: [_borrowed]} = call}, 1_000
    assert no_new_fds?(before_fds)
    refute_receive {^server, %Message{header_fields: %{reply_serial: _}}}, 100
    assert {:error, :unknown_call} = Service.reply(call, [])
  end

  test "closes a borrowed descriptor when its handler exits", %{
    server: server,
    connection: connection
  } do
    parent = self()
    before_fds = fd_set!()

    handler =
      spawn(fn ->
        receive do
          {_ref, %Call{} = call} -> send(parent, {:handled, call})
        end

        Process.sleep(:infinity)
      end)

    {_ref, serial} = fd_call!(server, connection, "Take", handler: handler)
    assert_receive {:handled, %Call{unix_fds: [borrowed]}}, 1_000
    assert UnixFD.open?(borrowed)

    Process.exit(handler, :kill)

    assert_receive {^server,
                    %Message{
                      type: :error,
                      header_fields: %{
                        reply_serial: ^serial,
                        error_name: "org.freedesktop.DBus.Error.Failed"
                      }
                    }},
                   1_000

    assert_closed_once(borrowed)
    assert no_new_fds?(before_fds)
  end

  test "closes a borrowed descriptor when its object is unexported", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {ref, %Call{serial: serial}} = fd_call!(server, connection, "Take")

    assert :ok = Service.unexport(connection, ref)

    assert_receive {^server,
                    %Message{
                      type: :error,
                      header_fields: %{
                        reply_serial: ^serial,
                        error_name: "org.freedesktop.DBus.Error.Failed"
                      }
                    }},
                   1_000

    assert no_new_fds?(before_fds)
  end

  test "closes the descriptors of calls refused at max_pending_calls", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    :sys.replace_state(connection, &%{&1 | max_pending_calls: 1})
    {ref, %Call{} = pending} = fd_call!(server, connection, "Take")

    serial = push_fd_call!(server, "Take")

    assert_receive {^server,
                    %Message{
                      type: :error,
                      header_fields: %{
                        reply_serial: ^serial,
                        error_name: "org.freedesktop.DBus.Error.LimitsExceeded"
                      }
                    }},
                   1_000

    push_fd_call!(server, "Take", flags: [:no_reply_expected])
    refute_receive {^ref, %Call{}}, 100

    assert :ok = Service.reply(pending, [])
    assert no_new_fds?(before_fds)
  end

  test "transfers a call's descriptors with take_fds, leaving the call to answer", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()
    {_ref, %Call{unix_fds: [borrowed]} = call} = fd_call!(server, connection, "Take")

    assert {:ok, [^borrowed]} = Service.take_fds(call)
    assert {:ok, owner} = UnixFD.owner(borrowed)
    assert owner == self()
    assert {:error, :unknown_call} = Service.take_fds(call)

    assert :ok = Service.reply(call, [])
    assert_receive {^server, %Message{type: :method_return}}, 1_000
    assert {:error, :unknown_call} = Service.take_fds(call)

    # Completing the call closed nothing: the handle is this process's.
    assert UnixFD.open?(borrowed)
    assert :ok = UnixFD.close(borrowed)
    assert {:ok, 1} = UnixFD.__close_count__(borrowed)
    assert no_new_fds?(before_fds)
  end

  test "transfers taken handles to whichever process takes them", %{
    server: server,
    connection: connection
  } do
    parent = self()
    {_ref, %Call{unix_fds: [borrowed]} = call} = fd_call!(server, connection, "Take")

    taker =
      spawn(fn ->
        send(parent, {:taken, Service.take_fds(call)})
        Process.sleep(:infinity)
      end)

    assert_receive {:taken, {:ok, [^borrowed]}}, 1_000
    assert {:ok, ^taker} = UnixFD.owner(borrowed)
    assert :ok = Service.reply(call, [])

    # The taker owns it past the call, and its exit closes it.
    assert UnixFD.open?(borrowed)
    Process.exit(taker, :kill)
    assert_closed_once(borrowed)
  end

  test "sends descriptors with a reply, but not the call's borrowed ones", %{
    server: server,
    connection: connection
  } do
    before_fds = fd_set!()

    {_ref, %Call{serial: serial, unix_fds: [borrowed]} = call} =
      fd_call!(server, connection, "Echo")

    # Outbound descriptors are raw numbers. A handler can read one out of a
    # borrowed handle, but not send it.
    assert {:error, :unix_fd_borrowed} = Service.reply(call, [0], fds: [raw!(borrowed)])
    refute_receive {^server, %Message{header_fields: %{reply_serial: _}}}, 100

    {:ok, [taken]} = Service.take_fds(call)
    assert {:ok, :ok} = UnixFD.with_fd(taken, &Service.reply(call, [0], fds: [&1]))

    assert_receive {^server,
                    %Message{
                      type: :method_return,
                      header_fields: %{reply_serial: ^serial},
                      body: [0],
                      unix_fds: [received]
                    }},
                   1_000

    # The reply borrowed the descriptor, so it is still the handler's to close,
    # as the server's received duplicate is the server's.
    assert :ok = Raw.close(received)

    # An error reply carries them the same way.
    {:ok, ref} = Service.export(connection, @files_path <> "/Errors", [@files])
    serial = push_fd_call!(server, "Echo", path: @files_path <> "/Errors")
    assert_receive {^ref, %Call{serial: ^serial, unix_fds: [borrowed]} = call}, 1_000

    assert {:error, :unix_fd_borrowed} =
             Service.reply_error(call, "org.example.Error.Refused", "no", fds: [raw!(borrowed)])

    assert {:ok, [error_taken]} = Service.take_fds(call)

    assert {:ok, :ok} =
             UnixFD.with_fd(
               taken,
               &Service.reply_error(call, "org.example.Error.Refused", "no", fds: [&1])
             )

    assert_receive {^server,
                    %Message{
                      type: :error,
                      header_fields: %{reply_serial: ^serial},
                      unix_fds: [received]
                    }},
                   1_000

    assert :ok = Raw.close(received)
    assert :ok = UnixFD.close(error_taken)
    assert :ok = UnixFD.close(taken)
    assert no_new_fds?(before_fds)
  end

  test "leaves taken handles closed exactly once after take_fds times out", %{
    server: server,
    connection: connection
  } do
    parent = self()
    {_ref, %Call{unix_fds: [borrowed]} = call} = fd_call!(server, connection, "Take")

    # The connection reads the take only after the taker has given up on it,
    # and still makes the live taker the owner.
    :ok = :sys.suspend(connection)

    taker =
      spawn(fn ->
        send(parent, {:take_result, Service.take_fds(call, 100)})

        receive do
          message -> send(parent, {:unexpected_message, message})
        end
      end)

    assert_receive {:take_result, {:error, :timeout}}, 1_000
    :ok = :sys.resume(connection)

    assert eventually(fn -> UnixFD.owner(borrowed) == {:ok, taker} end)
    assert {:error, :unknown_call} = Service.take_fds(call)
    assert :ok = Service.reply(call, [])
    assert UnixFD.open?(borrowed)
    refute_received {:unexpected_message, _message}

    Process.exit(taker, :kill)
    assert_closed_once(borrowed)
  end

  test "closes call descriptors whose taker exits before the connection reads the take", %{
    server: server,
    connection: connection
  } do
    {_ref, %Call{unix_fds: [borrowed]} = call} = fd_call!(server, connection, "Take")

    :ok = :sys.suspend(connection)
    taker = spawn(fn -> Service.take_fds(call, 1_000) end)

    assert eventually(fn ->
             Process.info(connection, :message_queue_len) != {:message_queue_len, 0}
           end)

    taker_ref = Process.monitor(taker)
    Process.exit(taker, :kill)
    assert_receive {:DOWN, ^taker_ref, :process, ^taker, :killed}, 1_000
    :ok = :sys.resume(connection)

    # The transfer found the taker gone and closed the handle.
    assert_closed_once(borrowed)
    assert {:error, :unknown_call} = Service.take_fds(call)
    assert :ok = Service.reply(call, [])
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
    path = Rebus.TestTmp.path("ufd-err")

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
    # only a quarantine mark remains while the eight-byte prefix is buffered.
    assert eventually(fn ->
             state = :sys.get_state(connection)

             Rights.quarantined?(state.inbound_fds) and not Rights.holding?(state.inbound_fds) and
               state.inbound.size == byte_size(prefix)
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

    # Descriptors are attributed by header count, so the over-limit batch
    # belongs to the frame that declares one.
    rejected =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "TooManyFDs",
        signature: "h",
        body: [0],
        fds: [fd]
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
    assert {:error, :ebadf} = Raw.close(received)
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

    assert {:error, :ebadf} = Raw.close(invalid_iov_fd)

    assert {:ok,
            %{
              ctrl: [%{level: :socket, type: :rights, data: <<missing_iov_fd::native-signed-32>>}]
            } = recvmsg} = receive_rights_message!(sender, receiver, fd)

    assert {:stop, {:shutdown, :invalid_unix_fds}, _state} =
             Rebus.Connection.handle_receive_result(
               {:ok, Map.drop(recvmsg, [:iov])},
               :sys.get_state(connection)
             )

    assert {:error, :ebadf} = Raw.close(missing_iov_fd)
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
    path = Rebus.TestTmp.path("ufd-pair")
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

  # A descriptor an exported object's handler is holding: a call routed to
  # this process and left unanswered, so the connection still owns what it
  # carried.
  defp hold_inbound_call_fd!(server, connection) do
    {_ref, %Call{unix_fds: [fd]}} = fd_call!(server, connection, "Take")
    fd
  end

  # Exports `@files` to this process, or to `opts[:handler]`, and pushes a call
  # to `member` carrying a descriptor; see `push_fd_call!/3`. Returns the
  # export and, unless another process handles it, the call as delivered.
  defp fd_call!(server, connection, member, opts \\ []) do
    {:ok, ref} =
      Service.export(
        connection,
        @files_path,
        [@files],
        Keyword.take(opts, [:handler, :call_timeout])
      )

    serial = push_fd_call!(server, member, opts)

    if Keyword.has_key?(opts, :handler) do
      {ref, serial}
    else
      assert_receive {^ref, %Call{serial: ^serial, unix_fds: [_borrowed]} = call}, 1_000
      {ref, call}
    end
  end

  # Pushes a call to `member` of `@files` from the test server. It carries the
  # server's own socket descriptor, which the connection receives as a number
  # of its own, so whatever the connection closes is never the server's.
  defp push_fd_call!(server, member, opts \\ []) do
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})

    call =
      Message.new!(:method_call,
        path: Keyword.get(opts, :path, @files_path),
        interface: "org.example.Files",
        member: member,
        sender: ":1.99",
        signature: "h",
        body: [0],
        fds: [fd],
        flags: Keyword.get(opts, :flags, [])
      )

    {:ok, serial} = TestServer.push_call_with_fds(server, call, [fd])
    serial
  end

  # Calls `method` from a task that checks it owns every handle in the reply,
  # then hands them to the test process, so they outlive the task.
  defp call_task(connection, method) do
    parent = self()

    Task.async(fn ->
      result = Rebus.call(connection, method, 1_000)

      case result do
        {:ok, %Message{unix_fds: handles}} -> hand_back(handles, parent)
        {:error, {:dbus_error, %Message{unix_fds: handles}}} -> hand_back(handles, parent)
        _other -> :ok
      end

      result
    end)
  end

  defp hand_back(handles, parent) do
    me = self()

    for handle <- handles do
      {:ok, ^me} = UnixFD.owner(handle)
      :ok = UnixFD.controlling_process(handle, parent)
    end
  end

  # Runs `fun` in a short-lived process, so nothing it copies into its heap,
  # such as a handle in a connection's state, outlives the call.
  defp elsewhere(fun), do: fun |> Task.async() |> Task.await()

  # The raw number behind an open handle, for comparison only.
  defp raw!(handle) do
    {:ok, fd} = UnixFD.with_fd(handle, & &1)
    fd
  end

  defp identity!(handle) do
    {:ok, identity} = UnixFD.with_fd(handle, &UnixFD.__fd_identity__/1)
    identity
  end

  # close(2) on owner exit runs just after the handle reads closed, so the
  # count is polled.
  defp assert_closed_once(handle) do
    assert eventually(fn ->
             not UnixFD.open?(handle) and UnixFD.__close_count__(handle) == {:ok, 1}
           end)
  end

  defp no_new_fds?(before_fds),
    do: eventually(fn -> MapSet.difference(fd_set!(), before_fds) == MapSet.new() end)

  # Polls for up to a second, as loaded macOS runners need.
  defp eventually(fun, attempts \\ 100)

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
