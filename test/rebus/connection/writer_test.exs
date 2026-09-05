defmodule Rebus.Connection.WriterTest do
  use ExUnit.Case, async: true

  import ExUnit.CaptureLog, only: [with_log: 1]

  alias Rebus.Connection.Hooks
  alias Rebus.Connection.Writer
  alias Rebus.Message
  alias Rebus.ScriptedTransport

  @max_serial 4_294_967_295
  @max_queued_replies 64
  # The writer never touches a descriptor; it only copies the numbers into the
  # SCM_RIGHTS control data, so a literal stands in for a real one.
  @fd 7

  describe "advance/2" do
    test "writes queued frames in the order they were queued" do
      sock = scripted()
      ctx = ctx(sock)
      call = call_msg("First")
      signal = signal_msg("Second")
      reply = reply_msg()

      writer =
        Writer.new()
        |> Writer.queue(op(:call, call))
        |> Writer.queue(op(:send, signal))
        |> Writer.queue(op(:reply, reply))

      assert {{:ok, _writer}, _ctx, [%{serial: 1}]} = run(Writer.advance(writer, ctx), ctx)

      assert ScriptedTransport.writes(sock) == [
               {:send, encoded(call, 1)},
               {:send, encoded(signal, 2)},
               {:send, encoded(reply, 3)}
             ]
    end

    test "holds a connection reply behind a call that has not finished writing" do
      continuation = {:select_info, :send, make_ref()}
      sock = scripted(send: [{:select, continuation}])
      ctx = ctx(sock)
      call = call_msg("Blocking")
      reply = reply_msg()

      writer =
        Writer.new()
        |> Writer.queue(op(:call, call))
        |> Writer.queue(op(:reply, reply))

      assert {:ok, writer} = Writer.advance(writer, ctx)
      assert %{wait: {:select, ^continuation, _handle}} = Writer.active(writer)
      assert ScriptedTransport.writes(sock) == [{:send, encoded(call, 1)}]

      # Only finishing the call releases the reply queued behind it.
      assert {{:ok, _writer}, _ctx, [_entry]} =
               run(Writer.resume_select(writer, continuation, ctx), ctx)

      assert ScriptedTransport.writes(sock) == [
               {:send, encoded(call, 1)},
               {:send, encoded(call, 1)},
               {:send, encoded(reply, 2)}
             ]
    end

    test "answers a :send caller once its frame is written" do
      sock = scripted()
      ctx = ctx(sock)
      ref = make_ref()

      assert {:ok, writer} =
               Writer.enqueue(
                 Writer.new(),
                 op(:send, signal_msg("Done"), from: {self(), ref}),
                 ctx
               )

      assert_receive {^ref, :ok}
      assert Writer.active(writer) == nil
      assert Writer.serial(writer) == 2
    end

    test "hands a written :call back with the entry the connection must index" do
      sock = scripted()
      ctx = ctx(sock)
      from = {self(), make_ref()}
      request_ref = make_ref()
      deadline = System.monotonic_time(:millisecond) + 5_000

      operation =
        op(:call, call_msg("Go"), from: from, request_ref: request_ref, deadline: deadline)

      assert {:call_written, entry, writer} = Writer.enqueue(Writer.new(), operation, ctx)

      assert %{serial: 1, from: ^from, request_ref: ^request_ref, deadline: ^deadline} = entry
      assert is_reference(entry.timer_ref)
      assert is_reference(entry.monitor_ref)

      # The correlation entry has left the writer: the caller's monitor is now
      # the connection's to release.
      assert Writer.active(writer) == nil
      assert writer.monitor_index == %{}
      assert Writer.serial(writer) == 2
    end

    test "drops a call whose deadline passed while its frame was being written" do
      signal = signal_msg("Late")
      frame = encoded(signal, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(send: [{:ok, tail}])
      ctx = ctx(sock)
      ref = make_ref()

      operation =
        op(:call, signal, from: {self(), ref}, deadline: System.monotonic_time(:millisecond) + 30)

      assert {:continue, writer} = Writer.enqueue(Writer.new(), operation, ctx)
      Process.sleep(40)

      # The frame is already partly on the wire, so it is finished for framing's
      # sake, but there is no longer a request to correlate a reply with.
      assert {:ok, writer} = Writer.advance(writer, ctx)
      assert Writer.active(writer) == nil
      assert writer.monitor_index == %{}
      refute_receive {^ref, _reply}, 20
    end

    test "continues a partial write with the exact tail the transport left" do
      signal = signal_msg("Partial")
      frame = encoded(signal, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(send: [{:ok, tail}])
      ctx = ctx(sock)

      assert {:continue, writer} = Writer.enqueue(Writer.new(), op(:send, signal), ctx)
      assert %{rest: ^tail, partial?: true} = Writer.active(writer)

      assert {:ok, _writer} = Writer.advance(writer, ctx)
      assert ScriptedTransport.writes(sock) == [{:send, frame}, {:send, tail}]
    end

    test "parks on a select notification and resumes with the tail it was left" do
      continuation = {:select_info, :send, make_ref()}
      signal = signal_msg("Selected")
      frame = encoded(signal, 1)
      tail = binary_part(frame, 4, byte_size(frame) - 4)
      sock = scripted(send: [{:select, {continuation, tail}}])
      ctx = ctx(sock)
      ref = make_ref()

      assert {:ok, writer} =
               Writer.enqueue(Writer.new(), op(:send, signal, from: {self(), ref}), ctx)

      assert %{wait: {:select, ^continuation, _handle}, rest: ^tail, partial?: true} =
               Writer.active(writer)

      # Nothing is attempted again until the notification arrives.
      assert {:ok, ^writer} = Writer.advance(writer, ctx)
      assert ScriptedTransport.writes(sock) == [{:send, frame}]

      assert {:ok, _writer} = Writer.resume_select(writer, continuation, ctx)
      assert_receive {^ref, :ok}
      assert ScriptedTransport.writes(sock) == [{:send, frame}, {:send, tail}]
    end

    test "abandons a frame the transport timed out before accepting a byte" do
      first = signal_msg("Stalled")
      second = signal_msg("Next")
      sock = scripted(send: [fn data -> {:error, {:timeout, data}} end])
      ctx = ctx(sock)
      ref = make_ref()

      writer =
        Writer.new()
        |> Writer.queue(op(:send, first, from: {self(), ref}))
        |> Writer.queue(op(:send, second))

      assert {:ok, _writer} = Writer.advance(writer, ctx)
      assert_receive {^ref, {:error, :timeout}}

      # Nothing entered the stream, so the abandoned frame's serial is reused.
      assert ScriptedTransport.writes(sock) == [
               {:send, encoded(first, 1)},
               {:send, encoded(second, 1)}
             ]
    end

    test "stops the connection when a write times out after partial progress" do
      signal = signal_msg("Partial")
      frame = encoded(signal, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(send: [{:ok, tail}, fn data -> {:error, {:timeout, data}} end])
      ctx = ctx(sock)
      ref = make_ref()

      assert {:continue, writer} =
               Writer.enqueue(Writer.new(), op(:send, signal, from: {self(), ref}), ctx)

      assert {:stop, :timeout, _writer} = Writer.advance(writer, ctx)
      refute_receive {^ref, _reply}, 20
    end

    test "stops the connection on a fatal transport error" do
      sock = scripted(send: [{:error, :closed}])
      ctx = ctx(sock)

      assert {:stop, :closed, _writer} =
               Writer.enqueue(Writer.new(), op(:send, signal_msg("Closed")), ctx)
    end

    test "fails the write closed when the transport returns an unknown result" do
      sock = scripted(send: [:unexpected_socket_shape])
      ctx = ctx(sock)

      assert {:stop, :send_failed, _writer} =
               Writer.enqueue(Writer.new(), op(:send, signal_msg("Weird")), ctx)
    end

    test "skips an operation whose deadline passed before it was dequeued" do
      expired = signal_msg("Expired")
      live = signal_msg("Live")
      sock = scripted()
      ctx = ctx(sock)
      ref = make_ref()

      writer =
        Writer.new()
        |> Writer.queue(
          op(:send, expired,
            from: {self(), ref},
            deadline: System.monotonic_time(:millisecond) - 1
          )
        )
        |> Writer.queue(op(:send, live))

      assert {:ok, writer} = Writer.advance(writer, ctx)

      # The caller is already answering its own `GenServer.call/3` timeout.
      refute_receive {^ref, _reply}, 20
      assert ScriptedTransport.writes(sock) == [{:send, encoded(live, 1)}]
      assert writer.monitor_index == %{}
    end

    test "answers the caller when the connection refuses the message" do
      sock = scripted()
      ctx = ctx(sock, validate: fn %Message{} -> {:error, :unix_fd_unsupported} end)
      ref = make_ref()

      assert {:ok, _writer} =
               Writer.enqueue(
                 Writer.new(),
                 op(:send, signal_msg("Refused"), from: {self(), ref}),
                 ctx
               )

      assert_receive {^ref, {:error, :unix_fd_unsupported}}
      assert ScriptedTransport.writes(sock) == []
    end

    test "logs and drops a connection reply it cannot send" do
      sock = scripted()
      ctx = ctx(sock, validate: fn %Message{} -> {:error, :unix_fd_unsupported} end)

      {result, log} =
        with_log(fn -> Writer.enqueue(Writer.new(), op(:reply, reply_msg()), ctx) end)

      assert {:ok, _writer} = result
      assert log =~ "D-Bus internal reply dropped: :unix_fd_unsupported"
      assert ScriptedTransport.writes(sock) == []
    end

    test "arms a write timeout for the frame it starts" do
      continuation = {:select_info, :send, make_ref()}
      sock = scripted(send: [{:select, continuation}])
      ctx = ctx(sock, write_timeout: 10)
      request_ref = make_ref()

      assert {:ok, _writer} =
               Writer.enqueue(
                 Writer.new(),
                 op(:send, signal_msg("Armed"), request_ref: request_ref),
                 ctx
               )

      assert_receive {:write_timeout, ^request_ref}, 500
    end
  end

  describe "write_timeout/2" do
    test "abandons the active frame when nothing has been written" do
      continuation = {:select_info, :send, make_ref()}
      first = signal_msg("Stalled")
      second = signal_msg("Next")
      sock = scripted(send: [{:select, continuation}])
      ctx = ctx(sock)
      ref = make_ref()

      writer =
        Writer.new()
        |> Writer.queue(op(:send, first, from: {self(), ref}))
        |> Writer.queue(op(:send, second))

      assert {:ok, writer} = Writer.advance(writer, ctx)
      assert {:ok, writer} = Writer.write_timeout(writer, ctx)

      assert_receive {^ref, {:error, :timeout}}
      assert Writer.active(writer) == nil

      assert ScriptedTransport.writes(sock) == [
               {:send, encoded(first, 1)},
               {:cancel, continuation},
               {:send, encoded(second, 1)}
             ]
    end

    test "stops the connection when the frame is already partly written" do
      signal = signal_msg("Partial")
      frame = encoded(signal, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(send: [{:ok, tail}])
      ctx = ctx(sock)
      ref = make_ref()

      assert {:continue, writer} =
               Writer.enqueue(Writer.new(), op(:send, signal, from: {self(), ref}), ctx)

      assert {:stop, :timeout, _writer} = Writer.write_timeout(writer, ctx)
      refute_receive {^ref, _reply}, 20
    end
  end

  describe "cancel/3" do
    test "drops a queued operation without writing it" do
      sock = scripted()
      ctx = ctx(sock)
      first = signal_msg("First")
      second = signal_msg("Cancelled")
      request_ref = make_ref()
      ref = make_ref()

      writer =
        Writer.new()
        |> Writer.queue(op(:send, first))
        |> Writer.queue(op(:send, second, from: {self(), ref}, request_ref: request_ref))

      assert {:ok, writer} = Writer.cancel(writer, request_ref, ctx)
      assert MapSet.member?(writer.cancelled_refs, request_ref)

      assert {:ok, writer} = Writer.advance(writer, ctx)

      refute_receive {^ref, _reply}, 20
      assert ScriptedTransport.writes(sock) == [{:send, encoded(first, 1)}]
      assert :queue.is_empty(writer.queue)
      assert MapSet.size(writer.queued_refs) == 0
      assert MapSet.size(writer.cancelled_refs) == 0
      assert writer.monitor_index == %{}
    end

    test "ignores a request that is neither queued nor active" do
      writer = Writer.new()
      assert {:ok, ^writer} = Writer.cancel(writer, make_ref(), ctx(scripted()))
    end

    test "drops an active operation that has not written a byte" do
      continuation = {:select_info, :send, make_ref()}
      first = signal_msg("Parked")
      second = signal_msg("Next")
      sock = scripted(send: [{:select, continuation}])
      ctx = ctx(sock)
      request_ref = make_ref()
      ref = make_ref()

      writer =
        Writer.new()
        |> Writer.queue(op(:send, first, from: {self(), ref}, request_ref: request_ref))
        |> Writer.queue(op(:send, second))

      assert {:ok, writer} = Writer.advance(writer, ctx)
      assert %{partial?: false, wait: {:select, ^continuation, _handle}} = Writer.active(writer)

      assert {:ok, writer} = Writer.cancel(writer, request_ref, ctx)
      assert Writer.active(writer) == nil
      refute_receive {^ref, _reply}, 20

      # The parked select is cancelled, and the frame behind it takes the serial
      # the cancelled frame never consumed.
      assert ScriptedTransport.writes(sock) == [
               {:send, encoded(first, 1)},
               {:cancel, continuation},
               {:send, encoded(second, 1)}
             ]
    end

    test "finishes an active partial frame but discards its result" do
      signal = signal_msg("Partial")
      frame = encoded(signal, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(send: [{:ok, tail}])
      ctx = ctx(sock)
      request_ref = make_ref()
      ref = make_ref()

      assert {:continue, writer} =
               Writer.enqueue(
                 Writer.new(),
                 op(:send, signal, from: {self(), ref}, request_ref: request_ref),
                 ctx
               )

      assert {:ok, writer} = Writer.cancel(writer, request_ref, ctx)
      assert %{partial?: true} = Writer.active(writer)

      assert {:ok, writer} = Writer.advance(writer, ctx)

      refute_receive {^ref, _reply}, 20
      assert ScriptedTransport.writes(sock) == [{:send, frame}, {:send, tail}]
      # Framing is preserved, so the serial is still consumed.
      assert Writer.serial(writer) == 2
      assert MapSet.size(writer.cancelled_refs) == 0
    end
  end

  describe "cancel_monitored/3" do
    test "cancels a queued request whose caller went down" do
      sock = scripted()
      ctx = ctx(sock)
      caller = spawn(fn -> Process.sleep(:infinity) end)
      on_exit(fn -> Process.exit(caller, :kill) end)
      request_ref = make_ref()

      writer =
        Writer.queue(
          Writer.new(),
          op(:send, signal_msg("Down"), from: {caller, make_ref()}, request_ref: request_ref)
        )

      assert [{monitor_ref, ^request_ref}] = Map.to_list(writer.monitor_index)
      assert {^request_ref, writer} = Writer.pop_monitor(writer, monitor_ref)
      assert writer.monitor_index == %{}
      assert :error = Writer.pop_monitor(writer, monitor_ref)

      assert {:ok, writer} = Writer.cancel_monitored(writer, request_ref, ctx)
      assert {:ok, writer} = Writer.advance(writer, ctx)

      assert ScriptedTransport.writes(sock) == []
      assert MapSet.size(writer.cancelled_refs) == 0
    end
  end

  describe "serial allocation" do
    test "numbers frames in sequence and wraps after the maximum" do
      sock = scripted()
      ctx = ctx(sock)
      signal = signal_msg("Wrap")

      assert {:ok, writer} =
               Writer.enqueue(%{Writer.new() | serial: @max_serial}, op(:send, signal), ctx)

      assert Writer.serial(writer) == 1

      assert {:ok, writer} = Writer.enqueue(writer, op(:send, signal), ctx)
      assert Writer.serial(writer) == 2

      assert ScriptedTransport.writes(sock) == [
               {:send, encoded(signal, @max_serial)},
               {:send, encoded(signal, 1)}
             ]
    end

    test "skips serials still spoken for by a pending reply" do
      sock = scripted()
      ctx = ctx(sock, pending: %{1 => :live, 2 => :live})
      signal = signal_msg("Skip")

      assert {:ok, writer} = Writer.enqueue(Writer.new(), op(:send, signal), ctx)
      assert Writer.serial(writer) == 4
      assert ScriptedTransport.writes(sock) == [{:send, encoded(signal, 3)}]
    end

    test "accounts for the Hello frame the handshake writes itself" do
      assert Writer.serial(Writer.consume_serial(Writer.new())) == 2
      assert Writer.serial(Writer.consume_serial(%{Writer.new() | serial: @max_serial})) == 1
    end

    test "allocates within a bounded range" do
      assert {:ok, 2} = Writer.allocate_serial(1, %{1 => :pending}, 2)
      assert {:ok, 1} = Writer.allocate_serial(2, %{2 => :pending}, 2)

      assert {:error, :serial_exhausted} =
               Writer.allocate_serial(1, %{1 => :pending, 2 => :pending}, 2)
    end
  end

  describe "descriptor-carrying frames" do
    test "emits SCM_RIGHTS once and sends the tail without control data" do
      msg = fd_msg()
      frame = encoded(msg, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(sendmsg: [{:ok, tail}])
      ctx = ctx(sock)
      ref = make_ref()

      assert {:continue, writer} =
               Writer.enqueue(Writer.new(), op(:send, msg, from: {self(), ref}), ctx)

      assert %{fd_control: :accepted, rest: ^tail, partial?: true} = Writer.active(writer)

      assert {:ok, _writer} = Writer.advance(writer, ctx)
      assert_receive {^ref, :ok}

      assert [
               {:sendmsg,
                %{iov: [^frame], ctrl: [%{level: :socket, type: :rights, data: rights}]}},
               {:send, ^tail}
             ] = ScriptedTransport.writes(sock)

      assert rights == <<@fd::native-signed-32>>
    end

    test "parks a frame that made no progress on its sendmsg continuation" do
      continuation = {:select_info, :sendmsg, make_ref()}
      msg = fd_msg()
      frame = encoded(msg, 1)
      sock = scripted(sendmsg: [{:select, continuation}, :ok])
      ctx = ctx(sock)

      assert {:ok, writer} = Writer.enqueue(Writer.new(), op(:send, msg), ctx)

      assert %{fd_control: :select_continuation, wait: {:select, ^continuation, _handle}} =
               Writer.active(writer)

      assert {:ok, _writer} = Writer.resume_select(writer, continuation, ctx)

      # No byte was accepted, so the OTP continuation still owns the control
      # data and resuming it is the only way to emit the rights.
      assert [{:sendmsg, %{iov: [^frame], ctrl: [_rights]}}, {:sendmsg, [^frame]}] =
               ScriptedTransport.writes(sock)
    end

    test "cancels a sendmsg select continuation once a byte has been accepted" do
      continuation = {:select_info, :sendmsg, make_ref()}
      msg = fd_msg()
      frame = encoded(msg, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(sendmsg: [{:select, {continuation, tail}}])
      ctx = ctx(sock)

      assert {:continue, writer} = Writer.enqueue(Writer.new(), op(:send, msg), ctx)
      assert %{fd_control: :accepted, rest: ^tail} = Writer.active(writer)

      assert {:ok, _writer} = Writer.advance(writer, ctx)

      # The continuation retains the original control data, so it is cancelled
      # and the tail goes out as a plain send: the rights are emitted once.
      assert [{:sendmsg, _message}, {:cancel, ^continuation}, {:send, ^tail}] =
               ScriptedTransport.writes(sock)
    end

    test "reports a descriptor-local failure to the caller and keeps writing" do
      msg = fd_msg()
      next = signal_msg("After")
      sock = scripted(sendmsg: [{:error, :ebadf}])
      ctx = ctx(sock)
      ref = make_ref()

      writer =
        Writer.new()
        |> Writer.queue(op(:send, msg, from: {self(), ref}))
        |> Writer.queue(op(:send, next))

      assert {:ok, _writer} = Writer.advance(writer, ctx)
      assert_receive {^ref, {:error, :unix_fd_send_failed}}
      assert [{:sendmsg, _message}, {:send, _frame}] = ScriptedTransport.writes(sock)
    end

    test "stops the connection when a descriptor error follows accepted bytes" do
      msg = fd_msg()
      frame = encoded(msg, 1)
      tail = binary_part(frame, 1, byte_size(frame) - 1)
      sock = scripted(sendmsg: [{:error, {:ebadf, tail}}])
      ctx = ctx(sock)

      assert {:stop, :ebadf, _writer} = Writer.enqueue(Writer.new(), op(:send, msg), ctx)
    end
  end

  describe "connection-originated replies" do
    test "caps the queue and logs the refusal once per saturation episode" do
      continuation = {:select_info, :send, make_ref()}
      sock = scripted(send: [{:select, continuation}])
      ctx = ctx(sock)

      writer = queue_replies(Writer.new(), @max_queued_replies - 1)
      refute Writer.replies_saturated?(writer)

      writer = queue_replies(writer, 1)
      assert Writer.replies_saturated?(writer)

      {writer, log} = with_log(fn -> Writer.refuse_reply(writer) end)
      assert log =~ "D-Bus internal reply dropped: :reply_queue_full"
      assert writer.saturated?

      {writer, log} = with_log(fn -> Writer.refuse_reply(writer) end)
      refute log =~ "reply_queue_full"

      # Draining one reply ends the episode, so the next refusal is logged.
      assert {:ok, writer} = Writer.advance(writer, ctx)
      refute Writer.replies_saturated?(writer)
      refute writer.saturated?

      writer = queue_replies(writer, 1)
      assert Writer.replies_saturated?(writer)

      {_writer, log} = with_log(fn -> Writer.refuse_reply(writer) end)
      assert log =~ "D-Bus internal reply dropped: :reply_queue_full"
    end
  end

  describe "abandon_all/1" do
    test "answers queued and active callers and drops connection replies" do
      continuation = {:select_info, :send, make_ref()}
      sock = scripted(send: [{:select, continuation}])
      ctx = ctx(sock)
      active_ref = make_ref()
      queued_ref = make_ref()

      writer =
        %{Writer.new() | serial: 9}
        |> Writer.queue(op(:call, call_msg("Active"), from: {self(), active_ref}))
        |> Writer.queue(op(:send, signal_msg("Queued"), from: {self(), queued_ref}))
        |> Writer.queue(op(:reply, reply_msg()))

      assert {:ok, writer} = Writer.advance(writer, ctx)
      assert %{wait: {:select, ^continuation, _handle}} = Writer.active(writer)

      writer = Writer.abandon_all(writer)

      assert_receive {^active_ref, {:error, :disconnected}}
      assert_receive {^queued_ref, {:error, :disconnected}}
      # The queued reply had no caller and is simply discarded.
      assert writer.replies == 0

      assert Writer.active(writer) == nil
      assert :queue.is_empty(writer.queue)
      assert MapSet.size(writer.queued_refs) == 0
      assert MapSet.size(writer.cancelled_refs) == 0
      assert writer.monitor_index == %{}
      refute writer.saturated?
      # The serial counter deliberately survives teardown.
      assert Writer.serial(writer) == 9
    end
  end

  describe "classify_send_result/2" do
    test "classifies socket send results without exposing payloads" do
      assert :ok = Writer.classify_send_result(:ok, 3)
      assert {:error, :timeout} = Writer.classify_send_result({:error, {:timeout, "abc"}}, 3)

      assert {:error, {:send_fatal, :timeout}} =
               Writer.classify_send_result({:error, {:timeout, "a"}}, 3)

      assert {:error, {:send_fatal, :closed}} =
               Writer.classify_send_result({:error, {:closed, "abc"}}, 3)

      assert {:error, {:send_fatal, :closed}} = Writer.classify_send_result({:error, :closed}, 3)

      assert {:error, {:send_fatal, :timeout}} =
               Writer.classify_send_result({:error, {:timeout, %{}}}, 3)

      assert {:error, {:send_fatal, :send_failed}} =
               Writer.classify_send_result({:error, {"weird", "abc"}}, 3)

      assert {:continue, "bc"} = Writer.classify_send_result({:ok, "bc"}, 3)

      assert {:error, {:send_fatal, :send_failed}} = Writer.classify_send_result({:ok, ["bc"]}, 3)

      assert {:error, {:send_fatal, :timeout}} =
               Writer.classify_send_result({:error, {:timeout, "bc"}}, 3)

      select_info = {:select_info, :send, make_ref()}
      completion_info = {:completion_info, :send, make_ref()}

      assert {:select, ^select_info, nil} = Writer.classify_send_result({:select, select_info}, 3)

      assert {:select, ^select_info, "bc"} =
               Writer.classify_send_result({:select, {select_info, "bc"}}, 3)

      # The completion backend is not supported: its result shape now reaches
      # the unknown-result fallback and fails the write closed.
      assert {:error, {:send_fatal, :send_failed}} =
               Writer.classify_send_result({:completion, completion_info}, 3)

      assert {:error, {:send_fatal, :send_failed}} =
               Writer.classify_send_result({:unexpected, :socket_shape}, 3)
    end
  end

  describe "socket_send_args/2" do
    test "builds nonblocking socket continuation arguments" do
      continuation = {:select_info, :send, make_ref()}
      assert {"rest", [], :nowait} = Writer.socket_send_args("rest", nil)

      assert {"rest", ^continuation, :nowait} =
               Writer.socket_send_args("rest", {:continue, continuation})
    end
  end

  # Mirrors the connection's own loop: a written `:call` is registered in
  # `pending` before the next frame starts, because serial allocation reads
  # that table, and a `:continue` result is what the GenServer turns back into
  # `advance/2`.
  defp run(result, ctx, entries \\ [])

  defp run({:call_written, entry, writer}, ctx, entries) do
    ctx = %{ctx | pending: Map.put(ctx.pending, entry.serial, entry)}
    run(Writer.advance(writer, ctx), ctx, [entry | entries])
  end

  defp run({:continue, writer}, ctx, entries), do: run(Writer.advance(writer, ctx), ctx, entries)
  defp run(result, ctx, entries), do: {result, ctx, Enum.reverse(entries)}

  defp scripted(script \\ []), do: ScriptedTransport.start(Map.new(script))

  defp ctx(sock, opts \\ []) do
    %{
      sock: sock,
      transport: ScriptedTransport,
      hooks: Hooks.Default,
      write_timeout: Keyword.get(opts, :write_timeout, 5_000),
      pending: Keyword.get(opts, :pending, %{}),
      validate: Keyword.get(opts, :validate, fn %Message{} -> :ok end)
    }
  end

  defp op(kind, %Message{} = msg, opts \\ []) do
    %{
      kind: kind,
      from: Keyword.get(opts, :from, default_from(kind)),
      msg: msg,
      deadline: Keyword.get(opts, :deadline, System.monotonic_time(:millisecond) + 5_000),
      request_ref: Keyword.get(opts, :request_ref, make_ref())
    }
  end

  defp default_from(:reply), do: nil
  defp default_from(_kind), do: {self(), make_ref()}

  defp queue_replies(writer, count) do
    Enum.reduce(1..count, writer, fn _index, writer ->
      Writer.queue(writer, op(:reply, reply_msg()))
    end)
  end

  defp call_msg(member),
    do: Message.new!(:method_call, path: "/test", interface: "org.example.Test", member: member)

  defp signal_msg(member),
    do: Message.new!(:signal, path: "/test", interface: "org.example.Test", member: member)

  defp reply_msg, do: Message.new!(:method_return, reply_serial: 1)

  defp fd_msg do
    Message.new!(:signal,
      path: "/test",
      interface: "org.example.Test",
      member: "Fd",
      signature: "h",
      body: [0],
      fds: [@fd]
    )
  end

  defp encoded(%Message{} = msg, serial) do
    {:ok, bin} = Message.encode(%{msg | serial: serial})
    IO.iodata_to_binary(bin)
  end
end
