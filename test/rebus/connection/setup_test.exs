defmodule Rebus.Connection.SetupTest do
  use ExUnit.Case, async: true

  alias Rebus.Connection
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Setup
  alias Rebus.Connection.Writer
  alias Rebus.Message
  alias Rebus.ScriptedTransport
  alias Rebus.TestFD
  alias Rebus.TestImpl
  alias Rebus.UnixFD

  @guid "30313233343536373839616263646566"
  @other_guid "46454443424139383736353433323130"
  # The hex-encoded decimal uid EXTERNAL sends as its authorization identity.
  @auth_id "353031"
  @addr %{family: :inet, addr: {127, 0, 0, 1}, port: 1}
  @hello_serial 1

  describe "setup/2" do
    test "drives the handshake and leaves the connection waiting to send Hello" do
      {sock, state} = connection(["OK #{@guid}\r\n"])

      assert {:continue, :hello, %Connection{guid: @guid, inbound: %Inbound{size: 0}}} =
               Setup.setup(state, @addr)

      assert ScriptedTransport.sent(sock) ==
               <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n" <> "BEGIN \r\n"
    end

    test "keeps bytes read past BEGIN as the start of the message stream" do
      {_sock, state} = connection(["OK #{@guid}\r\nl\0\0\1frame"])

      assert {:continue, :hello, %Connection{inbound: %Inbound{size: 9}}} =
               Setup.setup(state, @addr)
    end

    test "establishes without Hello on a peer-to-peer connection" do
      {sock, state} = connection(["OK #{@guid}\r\n"], bus?: false)

      assert {:continue, :established, established} = Setup.setup(state, @addr)

      assert {:continue, :recv, %Connection{established?: true, name: nil, hello_serial: nil}} =
               Setup.established(established)

      # A peer-to-peer endpoint never writes a Hello frame.
      assert ScriptedTransport.sent(sock) ==
               <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n" <> "BEGIN \r\n"
    end

    test "propagates a GUID mismatch out of the handshake" do
      {_sock, state} = connection(["OK #{@guid}\r\n"], expected_guid: @other_guid)

      assert {:shutdown, :guid_mismatch, %Connection{}} = Setup.setup(state, @addr)
    end

    test "reports a mismatched GUID to a waiting caller" do
      waiter_ref = make_ref()

      {_sock, state} =
        connection(["OK #{@guid}\r\n"],
          expected_guid: @other_guid,
          connect_waiter: {self(), waiter_ref},
          connect_waiter_monitor: Process.monitor(self())
        )

      assert {:shutdown, :guid_mismatch, %Connection{}} = Setup.setup(state, @addr)
      assert_received {^waiter_ref, {:error, :guid_mismatch}}
    end

    test "stops without touching the socket when the connect waiter is already gone" do
      waiter = dead_process()

      {sock, state} =
        connection(["OK #{@guid}\r\n"],
          connect_waiter: {waiter, make_ref()},
          connect_waiter_monitor: Process.monitor(waiter)
        )

      assert {:shutdown, :caller_gone, %Connection{}} = Setup.setup(state, @addr)
      assert ScriptedTransport.sent(sock) == <<>>
    end

    test "stops without touching the socket when the owner is already gone" do
      owner = dead_process()

      {sock, state} =
        connection(["OK #{@guid}\r\n"], owner: owner, owner_monitor: Process.monitor(owner))

      assert {:shutdown, :owner_down, %Connection{}} = Setup.setup(state, @addr)
      assert ScriptedTransport.sent(sock) == <<>>
    end

    test "reports a dead owner to a waiting caller" do
      owner = dead_process()
      waiter_ref = make_ref()

      {sock, state} =
        connection(["OK #{@guid}\r\n"],
          connect_waiter: {self(), waiter_ref},
          connect_waiter_monitor: Process.monitor(self()),
          owner: owner,
          owner_monitor: Process.monitor(owner)
        )

      # The waiter monitors the connection only once its supervisor has
      # returned the PID, so a connection that stops here may already be gone
      # and the monitor carries `:noproc`. The notification is what makes
      # `connect/2` answer `:owner_down` regardless of that ordering.
      assert {:shutdown, :owner_down, %Connection{}} = Setup.setup(state, @addr)
      assert_received {^waiter_ref, {:error, :owner_down}}
      assert ScriptedTransport.sent(sock) == <<>>
    end

    test "reports a dead caller rather than a dead owner" do
      gone = dead_process()
      waiter_ref = make_ref()

      {_sock, state} =
        connection(["OK #{@guid}\r\n"],
          connect_waiter: {gone, waiter_ref},
          connect_waiter_monitor: Process.monitor(gone),
          owner: gone,
          owner_monitor: Process.monitor(gone)
        )

      assert {:shutdown, :caller_gone, %Connection{}} = Setup.setup(state, @addr)
      # Nothing is told: the waiter this connection would answer is the dead
      # process, not this test.
      refute_received {^waiter_ref, _result}
    end

    test "refuses to establish a connection whose owner died during setup" do
      owner = dead_process()
      waiter_ref = make_ref()

      {_sock, state} =
        connection([],
          bus?: false,
          connect_waiter: {self(), waiter_ref},
          connect_waiter_monitor: Process.monitor(self()),
          owner: owner,
          owner_monitor: Process.monitor(owner)
        )

      # The waiter is never acknowledged, so its connect/2 answers with the
      # connection's own shutdown reason instead of a PID that stops at once.
      assert {:shutdown, :owner_down, %Connection{}} = Setup.established(state)
      refute_received {^waiter_ref, :accepted}
    end

    test "refuses to establish an unwaited connection whose owner died" do
      owner = dead_process()

      {_sock, state} =
        connection([], bus?: false, owner: owner, owner_monitor: Process.monitor(owner))

      # Nothing is waiting on this connection, so the dead owner ends it here
      # rather than on the next pass through the receive loop.
      assert {:shutdown, :owner_down, %Connection{}} = Setup.established(state)
    end

    test "waits for the caller's acknowledgement before continuing" do
      waiter_ref = make_ref()

      {_sock, state} =
        connection(["OK #{@guid}\r\n"],
          connect_waiter: {self(), waiter_ref},
          connect_waiter_monitor: Process.monitor(self())
        )

      # No continuation: the connection idles until the caller accepts it.
      assert {:ok, %Connection{guid: @guid}} = Setup.setup(state, @addr)
      assert_received {^waiter_ref, {:ok, pid}}
      assert pid == self()
    end
  end

  # Each phase advances a test clock by the time it spends, and records the
  # timeout it was handed. A direct address computes no budget of its own, so
  # these drive `setup/2` the way `Rebus.connect/2` does for one.
  describe "setup/2 budget" do
    test "gives each phase only what the phases before it left" do
      {_sock, state} = budgeted_connection(["OK #{@guid}\r\n"], auth_id: 600, connect: 300)

      assert {:continue, :hello, %Connection{guid: @guid}} = Setup.setup(state, @addr)
      assert phase_timeouts() == [auth_id: 1_000, connect: 400, recv: 100]
    end

    test "times out the handshake within the budget the earlier phases spent" do
      {sock, state} =
        budgeted_connection(["OK #{@guid}\r\n"], auth_id: 600, connect: 300, recv: :timeout)

      assert {:shutdown, :read_timeout, %Connection{}} = Setup.setup(state, @addr)
      # The read was given the 100 ms left, not a fresh 1,000 ms.
      assert phase_timeouts() == [auth_id: 1_000, connect: 400, recv: 100]
      assert ScriptedTransport.sent(sock) == <<0>> <> "AUTH EXTERNAL #{@auth_id}\r\n"
    end

    test "stops before authenticating once the earlier phases spent the budget" do
      {sock, state} = budgeted_connection(["OK #{@guid}\r\n"], auth_id: 600, connect: 400)

      assert {:shutdown, :read_timeout, %Connection{}} = Setup.setup(state, @addr)
      assert phase_timeouts() == [auth_id: 1_000, connect: 400]
      assert ScriptedTransport.sent(sock) == <<>>
    end

    test "does not connect once the identity lookup spent the budget" do
      {sock, state} = budgeted_connection(["OK #{@guid}\r\n"], auth_id: 1_000)

      assert {:shutdown, :read_timeout, %Connection{}} = Setup.setup(state, @addr)
      assert phase_timeouts() == [auth_id: 1_000]
      assert ScriptedTransport.sent(sock) == <<>>
    end
  end

  describe "hello/1" do
    test "sends Hello and records the serial it must correlate" do
      {sock, state} = connection([])

      assert {:continue, :hello_reply_buffer, %Connection{hello_serial: 1} = state} =
               Setup.hello(state)

      # The Hello serial is consumed, so the first application frame is 2.
      assert Writer.serial(state.writer) == 2
      assert ScriptedTransport.sent(sock) == hello_frame()
    end
  end

  describe "hello_reply_buffer/1" do
    test "accepts a unique name and retains the bytes that followed the reply" do
      rest = binary_part(signal_frame(), 0, 8)
      state = hello_pending(hello_reply(":1.42"), rest)

      assert {:continue, :recv,
              %Connection{
                name: ":1.42",
                established?: true,
                hello_serial: nil,
                inbound: %Inbound{size: 8}
              }} = Setup.hello_reply_buffer(state)
    end

    test "maps an error reply to its validated error name" do
      reply = Message.new!(:error, error_name: "org.example.Denied", reply_serial: @hello_serial)

      assert {:protocol_error, {:hello_failed, "org.example.Denied"}, %Connection{}} =
               Setup.hello_reply_buffer(hello_pending(reply))
    end

    test "rejects an error reply with no error name" do
      reply = Message.new!(:method_return, reply_serial: @hello_serial)

      assert {:protocol_error, {:hello_failed, :missing_unique_name}, %Connection{}} =
               Setup.hello_reply_buffer(hello_pending(reply))
    end

    test "rejects a reply whose body is not a unique name" do
      assert {:protocol_error, {:hello_failed, :invalid_unique_name}, %Connection{}} =
               Setup.hello_reply_buffer(hello_pending(hello_reply("org.example.NotUnique")))
    end

    test "rejects any other frame arriving before the Hello reply" do
      signal =
        Message.new!(:signal, path: "/test", interface: "org.example.Test", member: "Early")

      assert {:protocol_error, {:unexpected_handshake_message, :signal}, %Connection{}} =
               Setup.hello_reply_buffer(hello_pending(signal))
    end

    @tag skip: TestFD.skip_reason()
    test "refuses a Hello reply that carries descriptors" do
      fd = descriptor()

      # The frame only declares a descriptor; the connection attributes the
      # handle it is holding.
      reply =
        Message.new!(:method_return,
          reply_serial: @hello_serial,
          signature: "h",
          body: [0],
          fds: [0]
        )

      state = %{
        hello_pending(reply)
        | unix_fd_negotiated?: true,
          inbound_fds: Rights.retain(Rights.new(), [fd])
      }

      # The connection closes the handle before stopping.
      assert {:protocol_error, :invalid_unix_fds, %Connection{}} =
               Setup.hello_reply_buffer(state)

      refute UnixFD.open?(fd)
      assert {:ok, 1} = UnixFD.__close_count__(fd)
    end
  end

  describe "hello_reply/1" do
    test "reads the Hello reply from the socket when nothing was buffered" do
      {:ok, encoded} = Message.encode(%{hello_reply(":1.42") | serial: 7})
      {_sock, state} = connection([IO.iodata_to_binary(encoded)])
      state = %{state | hello_serial: @hello_serial}

      assert {:continue, :recv, %Connection{name: ":1.42", established?: true}} =
               Setup.hello_reply(state)
    end

    test "stops on a protocol read timeout rather than blocking" do
      {_sock, state} = connection([{:error, :timeout}])
      state = %{state | hello_serial: @hello_serial}

      assert {:protocol_error, :read_timeout, %Connection{}} = Setup.hello_reply(state)
    end

    test "stops when the peer closes before the Hello reply arrives" do
      {_sock, state} = connection([])
      state = %{state | hello_serial: @hello_serial}

      assert {:transport_error, :closed, %Connection{}} = Setup.hello_reply(state)
    end
  end

  # A connection whose socket is a `ScriptedTransport` agent. `connect/3`
  # succeeds without a real socket and `close/1` is a no-op so the agent
  # survives the assertions that read what was written to it.
  defp connection(script, overrides \\ []) do
    sock = ScriptedTransport.start(script)

    :ok =
      TestImpl.put(self(), %{
        transport_connect: fn _sock, _addr, _timeout -> :ok end,
        recv: &ScriptedTransport.recv/4,
        send: &ScriptedTransport.send/4,
        close: fn _sock -> :ok end
      })

    state =
      struct!(
        %Connection{
          sock: sock,
          impl: Rebus.Impl.build(transport: TestImpl),
          precomputed_auth_id: @auth_id,
          setup_timeout: 1_000,
          read_timeout: 1_000,
          write_timeout: 1_000
        },
        overrides
      )

    {sock, state}
  end

  # A `connection/2` that looks up its own identity and reads a test clock.
  # `spend` maps a phase to the milliseconds it takes; `recv: :timeout` makes
  # the first handshake read run out its whole timeout and report it.
  defp budgeted_connection(script, spend) do
    Process.put(:setup_budget_clock, 0)
    Process.put(:setup_budget_timeouts, [])

    {sock, state} =
      connection(script,
        precomputed_auth_id: nil,
        impl:
          Rebus.Impl.build(
            transport: TestImpl,
            identity: TestImpl.Identity,
            clock: TestImpl.Clock
          )
      )

    :ok =
      TestImpl.put(self(), %{
        monotonic_time: fn -> Process.get(:setup_budget_clock) end,
        auth_id: fn timeout ->
          spend(:auth_id, timeout, Keyword.get(spend, :auth_id, 0))
          {:ok, "501\n"}
        end,
        transport_connect: fn _sock, _addr, timeout ->
          spend(:connect, timeout, Keyword.get(spend, :connect, 0))
          :ok
        end,
        recv: fn sock, length, flags, timeout ->
          case Keyword.get(spend, :recv, 0) do
            :timeout ->
              spend(:recv, timeout, timeout)
              {:error, :timeout}

            elapsed ->
              spend(:recv, timeout, elapsed)
              ScriptedTransport.recv(sock, length, flags, timeout)
          end
        end
      })

    {sock, state}
  end

  defp spend(phase, timeout, elapsed) do
    Process.put(:setup_budget_timeouts, [{phase, timeout} | Process.get(:setup_budget_timeouts)])
    Process.put(:setup_budget_clock, Process.get(:setup_budget_clock) + elapsed)
  end

  defp phase_timeouts, do: Enum.reverse(Process.get(:setup_budget_timeouts))

  defp hello_pending(%Message{} = reply, rest \\ <<>>) do
    {_sock, state} = connection([])
    {:ok, encoded} = Message.encode(%{reply | serial: 7})

    %{state | hello_serial: @hello_serial, inbound: Inbound.new(iodata(encoded) <> rest)}
  end

  defp hello_reply(name) do
    Message.new!(:method_return, reply_serial: @hello_serial, signature: "s", body: [name])
  end

  defp hello_frame do
    hello =
      Message.new!(:method_call,
        path: "/",
        interface: "org.freedesktop.DBus",
        destination: "org.freedesktop.DBus",
        member: "Hello"
      )

    {:ok, encoded} = Message.encode(%{hello | serial: 1})
    iodata(encoded)
  end

  defp signal_frame do
    signal =
      Message.new!(:signal, path: "/test", interface: "org.example.Test", member: "Trailing")

    {:ok, encoded} = Message.encode(%{signal | serial: 2})
    iodata(encoded)
  end

  # A handle this process owns, as the connection owns every descriptor it
  # receives: one end of a new pipe.
  defp descriptor do
    {:ok, read, write} = UnixFD.__pipe__()
    :ok = UnixFD.close(write)
    read
  end

  defp dead_process do
    pid = spawn(fn -> :ok end)
    ref = Process.monitor(pid)
    assert_receive {:DOWN, ^ref, :process, ^pid, _reason}
    pid
  end

  defp iodata(data), do: IO.iodata_to_binary(data)
end
