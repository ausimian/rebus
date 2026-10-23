defmodule Rebus.Connection.HandedOverTest do
  use ExUnit.Case, async: true

  alias Rebus.Connection.HandedOver
  alias Rebus.TestFD
  alias Rebus.UnixFD

  @moduletag skip: TestFD.skip_reason()

  test "a cancel closes the handles handed to the caller" do
    {handle, caller} = handed_over!()
    request_ref = make_ref()
    deadline = System.monotonic_time(:millisecond) + 60_000
    handed_over = HandedOver.put(HandedOver.new(), request_ref, [handle], deadline)

    assert {:ok, handed_over} = HandedOver.cancel(handed_over, request_ref)
    assert handed_over == HandedOver.new()
    refute UnixFD.open?(handle)
    :ok = UnixFD.__drain__()
    assert {:ok, 1} = UnixFD.__close_count__(handle)

    # A second cancel finds nothing, so nothing closes twice.
    assert :error = HandedOver.cancel(handed_over, request_ref)
    Process.exit(caller, :kill)
  end

  test "once the grace after the deadline passes, the entry goes and the handles stay" do
    {handle, caller} = handed_over!()
    request_ref = make_ref()
    # A deadline already past leaves only the grace to wait for.
    deadline = System.monotonic_time(:millisecond) - 900
    handed_over = HandedOver.put(HandedOver.new(), request_ref, [handle], deadline)

    assert_receive {:handed_over_expired, ^request_ref}, 1_000
    handed_over = HandedOver.expire(handed_over, request_ref)

    assert handed_over == HandedOver.new()
    assert :error = HandedOver.cancel(handed_over, request_ref)
    assert {:ok, ^caller} = UnixFD.owner(handle)
    assert {:ok, 0} = UnixFD.__close_count__(handle)
    Process.exit(caller, :kill)
  end

  test "a cancel for a request it does not hold finds nothing" do
    assert :error = HandedOver.cancel(HandedOver.new(), make_ref())
  end

  # A handle already handed to a live caller, as the connection leaves it.
  defp handed_over! do
    {:ok, read, write} = UnixFD.__pipe__()
    :ok = UnixFD.close(write)
    caller = spawn(fn -> Process.sleep(:infinity) end)
    :ok = UnixFD.controlling_process(read, caller)
    {read, caller}
  end
end
