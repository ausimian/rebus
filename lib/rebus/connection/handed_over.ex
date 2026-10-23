defmodule Rebus.Connection.HandedOver do
  @moduledoc false

  # Replies whose handles have moved to the process that called
  # `Rebus.call/3`, remembered by request reference until that caller can no
  # longer cancel the call.
  #
  # The connection makes the caller the owner of a reply's handles before it
  # answers. A caller that is alive but has stopped waiting never sees that
  # answer, yet owns the handles, and they would stay open until it exits or
  # garbage-collects the discarded reply. Such a caller always says so: it
  # casts `{:cancel, request_ref}` as it gives up, and `cancel/2` closes the
  # handles then.
  #
  # That rests on one invariant: the cancel arrives if and only if the reply
  # was not delivered. `Rebus.SafeCall` casts it only when `GenServer.call/3`
  # exits with a timeout, and OTP's `gen:do_call/4` exits so only after it
  # has deactivated the call's reply alias and found no reply in the mailbox
  # (its `receive ... after 0` returns a reply that raced in instead). Once
  # the alias is inactive, a reply still in flight is discarded. So a caller
  # that sends the cancel never had the reply, and one that had it never
  # sends the cancel. Every `Rebus.call/3` timeout is an integer, so every
  # call can be cancelled this way.
  #
  # An entry is dropped without closing anything once the caller can no
  # longer cancel: when its cancel would have arrived by now, `@grace` after
  # the request's absolute deadline. The caller's own timer runs out at that
  # deadline, so the grace only has to cover the time it takes to be
  # scheduled, exit the call and have its cast reach this connection, which
  # takes milliseconds even on a loaded runner; one second is generous. A
  # cancel later than that finds no entry, and the handles fall back to
  # closing when the caller exits or collects the discarded reply.
  #
  # Like `Rebus.Connection.Pending` this owns timers that target `self()`,
  # the connection, which sends `{:handed_over_expired, request_ref}`.

  use TypedStruct

  alias Rebus.UnixFD

  @grace 1_000

  typedstruct enforce: true do
    field :entries, %{reference() => {[UnixFD.t()], reference()}}, default: %{}
  end

  @spec new() :: t()
  def new, do: %__MODULE__{}

  # Remembers the handles of a reply handed over for `request_ref`, until
  # `@grace` after the request's `deadline`.
  @spec put(t(), reference(), [UnixFD.t()], integer()) :: t()
  def put(%__MODULE__{} = handed_over, request_ref, [_ | _] = handles, deadline)
      when is_reference(request_ref) and is_integer(deadline) do
    timeout = max(0, deadline + @grace - System.monotonic_time(:millisecond))
    timer_ref = Process.send_after(self(), {:handed_over_expired, request_ref}, timeout)
    %{handed_over | entries: Map.put(handed_over.entries, request_ref, {handles, timer_ref})}
  end

  # The caller of `request_ref` timed out, so it never received the reply:
  # its handles are closed, without waiting for close(2). Any holder may
  # close a handle, and closing one the caller has somehow closed already is
  # harmless.
  @spec cancel(t(), reference()) :: {:ok, t()} | :error
  def cancel(%__MODULE__{} = handed_over, request_ref) do
    case Map.pop(handed_over.entries, request_ref) do
      {nil, _entries} ->
        :error

      {{handles, timer_ref}, entries} ->
        _ = Process.cancel_timer(timer_ref)
        Enum.each(handles, &UnixFD.close_async/1)
        {:ok, %{handed_over | entries: entries}}
    end
  end

  # The caller can no longer cancel, so it received the reply and its handles
  # are its own. A timer that fires after a cancel finds nothing.
  @spec expire(t(), reference()) :: t()
  def expire(%__MODULE__{} = handed_over, request_ref),
    do: %{handed_over | entries: Map.delete(handed_over.entries, request_ref)}
end
