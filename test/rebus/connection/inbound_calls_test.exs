defmodule Rebus.Connection.InboundCallsTest do
  # Not async: assertions turn on whether a specific descriptor number is still
  # open, and a concurrently opened descriptor could reuse a closed number.
  use ExUnit.Case, async: false

  alias Rebus.Connection.InboundCalls
  alias Rebus.Connection.InboundCalls.Entry
  alias Rebus.TestFD
  alias Rebus.UnixFD

  @moduletag skip: TestFD.skip_reason()

  describe "insert/3" do
    test "records the call under its reference and starts its expiry timer" do
      fd = TestFD.dup!()
      handler = self()
      ref = make_ref()
      before = System.monotonic_time(:millisecond)

      {entry, calls} =
        InboundCalls.insert(
          InboundCalls.new(),
          %{ref: ref, serial: 7, sender: ":1.99", handler: handler, unix_fds: [fd]},
          60_000
        )

      assert %Entry{ref: ^ref, serial: 7, sender: ":1.99", handler: ^handler, unix_fds: [^fd]} =
               entry

      assert is_reference(entry.timer_ref)
      assert is_integer(Process.read_timer(entry.timer_ref))
      assert entry.deadline >= before + 60_000
      assert entry.deadline <= System.monotonic_time(:millisecond) + 60_000
      assert {:ok, ^entry} = InboundCalls.fetch(calls, ref)

      # Inserting takes the descriptor into the table without closing it. The
      # probe closes it, so the table must not be closed after it.
      assert open?(fd)
    end

    test "starts no timer for a call that never expires" do
      {entry, calls} = insert(InboundCalls.new(), call_timeout: :infinity)

      assert entry.timer_ref == nil
      assert entry.deadline == nil
      assert {:ok, ^entry} = InboundCalls.fetch(calls, entry.ref)
    end

    test "keeps calls apart by reference" do
      {first, calls} = insert(InboundCalls.new(), serial: 1)
      {second, calls} = insert(calls, serial: 1)

      assert {:ok, ^first} = InboundCalls.fetch(calls, first.ref)
      assert {:ok, ^second} = InboundCalls.fetch(calls, second.ref)

      InboundCalls.close_all(calls)
    end

    test "refuses a call timeout that is not positive" do
      call = %{ref: make_ref(), serial: 1, sender: nil, handler: self(), unix_fds: []}

      assert_raise FunctionClauseError, fn -> InboundCalls.insert(InboundCalls.new(), call, 0) end
    end
  end

  describe "fetch/2" do
    test "misses a reference that was never inserted" do
      assert :error = InboundCalls.fetch(InboundCalls.new(), make_ref())
    end
  end

  describe "complete/2" do
    test "removes the call, cancels its timer and closes its descriptors" do
      fd = TestFD.dup!()
      {entry, calls} = insert(InboundCalls.new(), unix_fds: [fd])

      assert {^entry, calls} = InboundCalls.complete(calls, entry.ref)

      assert Process.read_timer(entry.timer_ref) == false
      assert :error = InboundCalls.fetch(calls, entry.ref)
      assert calls == InboundCalls.new()
      assert closed?(fd)
    end

    test "answers nothing for a call already completed, so it closes nothing twice" do
      {entry, calls} = insert(InboundCalls.new())
      {^entry, calls} = InboundCalls.complete(calls, entry.ref)

      assert {nil, ^calls} = InboundCalls.complete(calls, entry.ref)
    end

    test "leaves other calls in place" do
      {first, calls} = insert(InboundCalls.new())
      {second, calls} = insert(calls)

      assert {^first, calls} = InboundCalls.complete(calls, first.ref)
      assert {:ok, ^second} = InboundCalls.fetch(calls, second.ref)
      assert is_integer(Process.read_timer(second.timer_ref))

      InboundCalls.close_all(calls)
    end

    test "completes a call that never expires" do
      fd = TestFD.dup!()
      {entry, calls} = insert(InboundCalls.new(), call_timeout: :infinity, unix_fds: [fd])

      assert {^entry, calls} = InboundCalls.complete(calls, entry.ref)
      assert calls == InboundCalls.new()
      assert closed?(fd)
    end
  end

  describe "expire/2" do
    test "the timer reports the call by reference once its timeout passes" do
      {entry, calls} = insert(InboundCalls.new(), call_timeout: 10)
      ref = entry.ref

      assert_receive {:inbound_call_timeout, ^ref}, 1_000

      assert {^entry, calls} = InboundCalls.expire(calls, ref)
      assert calls == InboundCalls.new()
    end

    test "removes the expired call and closes its descriptors" do
      fd = TestFD.dup!()
      {entry, calls} = insert(InboundCalls.new(), call_timeout: 10, unix_fds: [fd])
      ref = entry.ref
      assert_receive {:inbound_call_timeout, ^ref}, 1_000

      assert {^entry, calls} = InboundCalls.expire(calls, ref)
      assert :error = InboundCalls.fetch(calls, ref)
      assert closed?(fd)
    end

    test "finds nothing when the timer fires after the call completed" do
      fd = TestFD.dup!()
      {entry, calls} = insert(InboundCalls.new(), call_timeout: 10, unix_fds: [fd])
      ref = entry.ref

      # The timer has already sent its message, so cancelling it on completion
      # cannot recall it.
      assert_receive {:inbound_call_timeout, ^ref}, 1_000
      {^entry, calls} = InboundCalls.complete(calls, ref)
      assert closed?(fd)

      assert {nil, ^calls} = InboundCalls.expire(calls, ref)
    end
  end

  describe "close_all/1" do
    test "cancels every timer and closes every descriptor" do
      first_fd = TestFD.dup!()
      second_fd = TestFD.dup!()
      {first, calls} = insert(InboundCalls.new(), unix_fds: [first_fd])
      {second, calls} = insert(calls, call_timeout: :infinity, unix_fds: [second_fd])

      assert :ok = InboundCalls.close_all(calls)

      assert Process.read_timer(first.timer_ref) == false
      assert second.timer_ref == nil
      assert closed?(first_fd)
      assert closed?(second_fd)
    end
  end

  describe "fail_all/1" do
    test "closes everything and leaves an empty table" do
      first_fd = TestFD.dup!()
      second_fd = TestFD.dup!()
      {first, calls} = insert(InboundCalls.new(), unix_fds: [first_fd])
      {second, calls} = insert(calls, unix_fds: [second_fd])

      assert InboundCalls.fail_all(calls) == InboundCalls.new()

      assert Process.read_timer(first.timer_ref) == false
      assert Process.read_timer(second.timer_ref) == false
      assert closed?(first_fd)
      assert closed?(second_fd)
      refute_received {:inbound_call_timeout, _ref}
    end
  end

  defp insert(calls, opts \\ []) do
    call = %{
      ref: make_ref(),
      serial: Keyword.get(opts, :serial, 1),
      sender: Keyword.get(opts, :sender, ":1.99"),
      handler: self(),
      unix_fds: Keyword.get(opts, :unix_fds, [])
    }

    InboundCalls.insert(calls, call, Keyword.get(opts, :call_timeout, 60_000))
  end

  defp open?(fd), do: UnixFD.close(fd) == :ok

  defp closed?(fd), do: UnixFD.close(fd) == {:error, :ebadf}
end
