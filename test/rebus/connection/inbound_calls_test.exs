defmodule Rebus.Connection.InboundCallsTest do
  use ExUnit.Case, async: true

  alias Rebus.Connection.InboundCalls
  alias Rebus.Connection.InboundCalls.Entry
  alias Rebus.TestFD
  alias Rebus.UnixFD

  @moduletag skip: TestFD.skip_reason()

  describe "insert/3" do
    test "records the call under its reference and starts its expiry timer" do
      fd = handle!()
      handler = self()
      ref = make_ref()
      export = make_ref()
      before = System.monotonic_time(:millisecond)

      {entry, calls} =
        InboundCalls.insert(
          InboundCalls.new(),
          %{
            ref: ref,
            serial: 7,
            sender: ":1.99",
            export: export,
            handler: handler,
            unix_fds: [fd]
          },
          60_000
        )

      assert %Entry{
               ref: ^ref,
               serial: 7,
               sender: ":1.99",
               export: ^export,
               handler: ^handler,
               unix_fds: [^fd]
             } = entry

      assert is_reference(entry.timer_ref)
      assert is_integer(Process.read_timer(entry.timer_ref))
      assert entry.deadline >= before + 60_000
      assert entry.deadline <= System.monotonic_time(:millisecond) + 60_000
      assert {:ok, ^entry} = InboundCalls.fetch(calls, ref)

      # Inserting takes the handle into the table without closing it.
      assert open?(fd)
      InboundCalls.close_all(calls)
    end

    test "records whether the caller asked for no reply" do
      {entry, calls} = insert(InboundCalls.new())
      refute entry.no_reply?

      {entry, calls} = insert(calls, no_reply?: true)
      assert entry.no_reply?
      refute entry.taken?

      InboundCalls.close_all(calls)
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
      call = %{
        ref: make_ref(),
        serial: 1,
        sender: nil,
        export: make_ref(),
        handler: self(),
        unix_fds: []
      }

      assert_raise FunctionClauseError, fn -> InboundCalls.insert(InboundCalls.new(), call, 0) end
    end
  end

  describe "fetch/2" do
    test "misses a reference that was never inserted" do
      assert :error = InboundCalls.fetch(InboundCalls.new(), make_ref())
    end
  end

  describe "size/1" do
    test "counts the calls waiting for an answer" do
      assert InboundCalls.size(InboundCalls.new()) == 0

      {first, calls} = insert(InboundCalls.new())
      {_second, calls} = insert(calls)
      assert InboundCalls.size(calls) == 2

      {_first, calls} = InboundCalls.complete(calls, first.ref)
      assert InboundCalls.size(calls) == 1

      InboundCalls.close_all(calls)
    end
  end

  describe "pop_export/2" do
    test "removes and releases every call made on the export, in serial order" do
      export = make_ref()
      fd = handle!()
      {second, calls} = insert(InboundCalls.new(), export: export, serial: 9, unix_fds: [fd])
      {other, calls} = insert(calls, serial: 8)
      {first, calls} = insert(calls, export: export, serial: 3)

      assert {[^first, ^second], calls} = InboundCalls.pop_export(calls, export)

      assert Process.read_timer(first.timer_ref) == false
      assert Process.read_timer(second.timer_ref) == false
      assert closed?(fd)
      assert InboundCalls.size(calls) == 1
      assert {:ok, ^other} = InboundCalls.fetch(calls, other.ref)

      InboundCalls.close_all(calls)
    end

    test "finds nothing for an export with no calls" do
      {_entry, calls} = insert(InboundCalls.new())

      assert {[], ^calls} = InboundCalls.pop_export(calls, make_ref())

      InboundCalls.close_all(calls)
    end
  end

  describe "complete/2" do
    test "removes the call, cancels its timer and closes its descriptors" do
      fd = handle!()
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
      fd = handle!()
      {entry, calls} = insert(InboundCalls.new(), call_timeout: :infinity, unix_fds: [fd])

      assert {^entry, calls} = InboundCalls.complete(calls, entry.ref)
      assert calls == InboundCalls.new()
      assert closed?(fd)
    end
  end

  describe "take_fds/2" do
    test "moves the descriptors out and leaves the call pending" do
      fd = handle!()
      {entry, calls} = insert(InboundCalls.new(), unix_fds: [fd])

      assert {:ok, ^entry, calls} = InboundCalls.take_fds(calls, entry.ref)

      assert {:ok, %Entry{unix_fds: [], taken?: true} = taken} =
               InboundCalls.fetch(calls, entry.ref)

      assert taken.timer_ref == entry.timer_ref

      # Completing the call no longer closes what was taken.
      assert {%Entry{}, _calls} = InboundCalls.complete(calls, entry.ref)
      assert open?(fd)
      UnixFD.close(fd)
    end

    test "takes descriptors only once" do
      fd = handle!()
      {entry, calls} = insert(InboundCalls.new(), unix_fds: [fd])
      {:ok, ^entry, calls} = InboundCalls.take_fds(calls, entry.ref)

      assert :error = InboundCalls.take_fds(calls, entry.ref)
      assert open?(fd)
      InboundCalls.close_all(calls)
      UnixFD.close(fd)
    end

    test "finds nothing for a call that is gone" do
      {entry, calls} = insert(InboundCalls.new())
      {^entry, calls} = InboundCalls.complete(calls, entry.ref)

      assert :error = InboundCalls.take_fds(calls, entry.ref)
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
      fd = handle!()
      {entry, calls} = insert(InboundCalls.new(), call_timeout: 10, unix_fds: [fd])
      ref = entry.ref
      assert_receive {:inbound_call_timeout, ^ref}, 1_000

      assert {^entry, calls} = InboundCalls.expire(calls, ref)
      assert :error = InboundCalls.fetch(calls, ref)
      assert closed?(fd)
    end

    test "finds nothing when the timer fires after the call completed" do
      fd = handle!()
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

  describe "releasing a call" do
    test "closes only the handles this process still owns" do
      owned = handle!()
      moved = handle!()
      owner = spawn(fn -> Process.sleep(:infinity) end)
      :ok = UnixFD.controlling_process(moved, owner)
      {entry, calls} = insert(InboundCalls.new(), unix_fds: [owned, moved])

      assert {^entry, _calls} = InboundCalls.complete(calls, entry.ref)

      assert closed?(owned)
      assert {:ok, ^owner} = UnixFD.owner(moved)
      Process.exit(owner, :kill)
    end

    test "leaves a handle its handler already closed closed once" do
      fd = handle!()
      {entry, calls} = insert(InboundCalls.new(), unix_fds: [fd])
      handler = Task.async(fn -> UnixFD.close(fd) end)
      assert :ok = Task.await(handler)

      assert {^entry, _calls} = InboundCalls.complete(calls, entry.ref)
      assert closed?(fd)
    end
  end

  describe "close_all/1" do
    test "cancels every timer and closes every descriptor" do
      first_fd = handle!()
      second_fd = handle!()
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
      first_fd = handle!()
      second_fd = handle!()
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
      export: Keyword.get_lazy(opts, :export, &make_ref/0),
      handler: self(),
      unix_fds: Keyword.get(opts, :unix_fds, []),
      no_reply?: Keyword.get(opts, :no_reply?, false)
    }

    InboundCalls.insert(calls, call, Keyword.get(opts, :call_timeout, 60_000))
  end

  # One end of a new pipe, owned by the test process as the connection would
  # own a received descriptor.
  defp handle! do
    {:ok, read, write} = UnixFD.__pipe__()
    :ok = UnixFD.close(write)
    read
  end

  defp open?(handle), do: UnixFD.open?(handle)

  # The close is synchronous in the process that made it, so close(2) has
  # run exactly once by the time it returns.
  # The calls table closes handles without waiting for close(2), so wait
  # for the closer before counting.
  defp closed?(handle) do
    :ok = UnixFD.__drain__()
    not UnixFD.open?(handle) and UnixFD.__close_count__(handle) == {:ok, 1}
  end
end
