defmodule Rebus.UnixFDDisabledTest do
  # A build without Unix fd support, made with REBUS_UNIX_FDS=0. Such a build
  # has no native library, never negotiates descriptor passing and refuses to
  # send descriptors. What it does with descriptors a peer pushes anyway
  # depends on the kernel: Linux discards them, while macOS installs them
  # where nothing in this build can close them, so the connection stops.
  use ExUnit.Case, async: false

  import ExUnit.CaptureLog

  alias Rebus.Connection.Setup
  alias Rebus.{Message, TestFD, TestImpl, TestServer, UnixFD}

  @moduletag skip: TestFD.disabled_skip_reason()

  setup do
    path = Rebus.TestTmp.path("ufd-off")

    {:ok, server} =
      start_supervised(%{
        id: {:unix_fd_disabled_server, path},
        start: {TestServer, :start_link, [[tap: self(), family: :local, path: path]]}
      })

    {:ok, address} = TestServer.get_listen_addr(server)

    {:ok, connection} =
      Rebus.connect(address, __impl__: %{identity: TestImpl.CachedIdentity})

    on_exit(fn ->
      if Process.alive?(connection), do: Rebus.close(connection)
    end)

    %{server: server, connection: connection}
  end

  test "Rebus.UnixFD reports no support, and its native module still loads" do
    refute UnixFD.supported?()
    assert Code.ensure_loaded?(Rebus.UnixFD.NIF)
    assert {:error, :unsupported} = UnixFD.__pipe__()
    assert {:error, :unsupported} = UnixFD.wrap(0, self())
    assert {:error, :unsupported} = UnixFD.close(%UnixFD{ref: make_ref()})
    assert {:error, :unsupported} = UnixFD.dup(%UnixFD{ref: make_ref()})
    assert {:error, :unsupported} = UnixFD.__fd_identity__(0)
    assert UnixFD.__live_count__() == 0
    assert UnixFD.__drain__() == :ok
  end

  test "a local socket cannot pass descriptors, and reads with recvmsg only on macOS" do
    {:module, :socket} = Code.ensure_loaded(:socket)

    refute Setup.unix_fd_transport_supported?(:local)
    assert Setup.recvmsg_reads?(:local) == (:os.type() == {:unix, :darwin})
  end

  test "does not negotiate descriptor passing with a peer that would agree", %{
    connection: connection
  } do
    # The server answers NEGOTIATE_UNIX_FD with AGREE_UNIX_FD, so the
    # connection is unnegotiated only because it never asked.
    state = :sys.get_state(connection)
    refute state.unix_fd_transport?
    refute state.unix_fd_negotiated?
    assert state.recvmsg_reads? == (:os.type() == {:unix, :darwin})
  end

  test "refuses to send descriptors and writes nothing", %{
    server: server,
    connection: connection
  } do
    signal =
      Message.new!(:signal,
        path: "/test",
        interface: "test.interface",
        member: "SendFD",
        signature: "h",
        body: [0],
        fds: [0]
      )

    call =
      Message.new!(:method_call,
        path: "/test",
        interface: "test.interface",
        member: "CallFD",
        signature: "h",
        body: [0],
        fds: [0]
      )

    assert {:error, :unix_fd_unsupported} = Rebus.send(connection, signal)
    assert {:error, :unix_fd_unsupported} = Rebus.call(connection, call, 1_000)
    refute_receive {^server, %Message{header_fields: %{member: "SendFD"}}}, 100
    refute_receive {^server, %Message{header_fields: %{member: "CallFD"}}}, 100
  end

  # A test's own skip tag replaces the module's, so each repeats it.
  @tag skip:
         TestFD.disabled_skip_reason() ||
           if(:os.type() == {:unix, :linux}, do: false, else: "Linux only")
  test "on Linux, descriptors a peer pushes never reach this process", %{
    server: server,
    connection: connection
  } do
    {:ok, handler_ref} = Rebus.add_signal_handler(connection)
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})
    before_fds = fd_set!()

    log =
      capture_log(fn ->
        :ok = TestServer.push_with_fds(server, signal("SharedFD", [fd]), [fd])
        :ok = TestServer.push(server, signal("AfterSharedFD", []))

        assert_receive {^handler_ref, %Message{header_fields: %{member: "AfterSharedFD"}}},
                       1_000
      end)

    # The frame that declared a descriptor is dropped, as for any peer that
    # sends one without negotiating, and the connection carries on.
    refute_received {^handler_ref, %Message{header_fields: %{member: "SharedFD"}}}
    assert log =~ "D-Bus FD frame dropped: :unix_fd_not_negotiated"
    assert Process.alive?(connection)
    assert fd_set!() == before_fds
  end

  @tag skip:
         TestFD.disabled_skip_reason() ||
           if(:os.type() == {:unix, :darwin}, do: false, else: "macOS only")
  test "on macOS, descriptors a peer pushes stop the connection", %{
    server: server,
    connection: connection
  } do
    {:ok, fd} = :socket.getopt(:sys.get_state(server).cli_sock, {:otp, :fd})
    ref = Process.monitor(connection)

    log =
      capture_log(fn ->
        :ok = TestServer.push_with_fds(server, signal("SharedFD", [fd]), [fd])

        assert_receive {:DOWN, ^ref, :process, ^connection, {:shutdown, :unix_fd_not_negotiated}},
                       1_000
      end)

    assert log =~ "D-Bus connection protocol stopped: :unix_fd_not_negotiated"
  end

  defp signal(member, fds) do
    {signature, body} = if fds == [], do: {"", []}, else: {"h", [0]}

    Message.new!(:signal,
      path: "/test",
      interface: "test.interface",
      member: member,
      signature: signature,
      body: body,
      fds: fds
    )
  end

  defp fd_set! do
    {:ok, names} = File.ls("/proc/self/fd")

    # Listing the directory opens a descriptor of its own, which is closed
    # again by the time its entry is checked.
    names
    |> Enum.filter(&match?({:ok, _}, :file.read_link(~c"/proc/self/fd/" ++ to_charlist(&1))))
    |> MapSet.new()
  end
end
