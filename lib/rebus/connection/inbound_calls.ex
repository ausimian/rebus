defmodule Rebus.Connection.InboundCalls do
  @moduledoc false

  # The table of inbound method calls handed to a handler process and not yet
  # answered, keyed by the reference that identifies each call to its handler.
  # It is the inbound mirror of `Rebus.Connection.Pending`: there the
  # connection waits for a peer to answer one of its calls, here a peer waits
  # for a local handler to answer one of the peer's.
  #
  # Like `Rebus.Connection.Pending` this is not a pure structure: an entry owns
  # its expiry timer and the handles that arrived with the call, which the
  # connection keeps until the call completes. Taking an entry out of the table
  # therefore cancels that timer and closes those handles here rather than at
  # each call site, so every way out closes them. Only handles the connection
  # still owns are closed, and closing is idempotent, so one a handler has
  # already closed is harmless. The one exception is `take_fds/2`, which moves
  # the handles out of the entry for the connection to hand over, and leaves
  # the entry itself in place.
  #
  # Every entry is bound to the export it was routed through, so unexporting
  # an object, or losing its handler, fails exactly the calls made on it.
  # Expiry timers send `{:inbound_call_timeout, ref}` to the connection.

  use TypedStruct

  alias Rebus.UnixFD

  defmodule Entry do
    @moduledoc false

    use TypedStruct

    # One call awaiting its handler's answer: the serial and sender a reply is
    # addressed to, the export and handler it was routed to, the descriptors
    # it carried, and the timer that expires it. `timer_ref` and `deadline`
    # are `nil` for a call that never expires. `from` is the local process
    # waiting for the answer to a call made with `Rebus.Service.invoke/7`,
    # and `nil` for a call from a peer, whose answer goes to the socket.
    # `no_reply?` marks a call whose caller asked for no reply, which is
    # recorded only to release its descriptors, and `taken?` one whose
    # descriptors `take_fds/2` has already moved out.

    typedstruct enforce: true do
      field :ref, reference()
      field :serial, pos_integer()
      field :sender, binary() | nil
      field :export, reference()
      field :handler, pid()
      field :unix_fds, [UnixFD.t()]
      field :timer_ref, reference() | nil
      field :deadline, integer() | nil
      field :from, GenServer.from() | nil, default: nil
      field :no_reply?, boolean(), default: false
      field :taken?, boolean(), default: false
    end
  end

  @typedoc "The fields a caller supplies for a new entry."
  @type call :: %{
          required(:ref) => reference(),
          required(:serial) => pos_integer(),
          required(:sender) => binary() | nil,
          required(:export) => reference(),
          required(:handler) => pid(),
          required(:unix_fds) => [UnixFD.t()],
          optional(:from) => GenServer.from(),
          optional(:no_reply?) => boolean()
        }

  typedstruct enforce: true do
    field :entries, %{reference() => Entry.t()}, default: %{}
  end

  @spec new() :: t()
  def new, do: %__MODULE__{}

  # Records a call routed to a handler and starts its expiry timer, which
  # targets `self()`: the connection is the only process that calls in here.
  # The sender is copied so a retained entry cannot pin the receive buffer it
  # was parsed from.
  @spec insert(t(), call(), pos_integer() | :infinity) :: {Entry.t(), t()}
  def insert(%__MODULE__{} = calls, %{ref: ref} = call, call_timeout)
      when is_reference(ref) and
             ((is_integer(call_timeout) and call_timeout > 0) or call_timeout == :infinity) do
    {timer_ref, deadline} = start_timer(ref, call_timeout)

    entry = %Entry{
      ref: ref,
      serial: call.serial,
      sender: copy(call.sender),
      export: call.export,
      handler: call.handler,
      unix_fds: call.unix_fds,
      timer_ref: timer_ref,
      deadline: deadline,
      from: Map.get(call, :from),
      no_reply?: Map.get(call, :no_reply?, false)
    }

    {entry, %{calls | entries: Map.put(calls.entries, ref, entry)}}
  end

  @spec fetch(t(), reference()) :: {:ok, Entry.t()} | :error
  def fetch(%__MODULE__{entries: entries}, ref), do: Map.fetch(entries, ref)

  # How many calls are waiting, which the connection bounds.
  @spec size(t()) :: non_neg_integer()
  def size(%__MODULE__{entries: entries}), do: map_size(entries)

  # The handler answered. The entry leaves the table with its timer cancelled
  # and its handles closed, so a second answer finds nothing.
  @spec complete(t(), reference()) :: {Entry.t() | nil, t()}
  def complete(%__MODULE__{} = calls, ref), do: pop(calls, ref)

  # Moves a call's handles out for the connection to hand over, leaving the
  # call pending with nothing of its own to close. The entry is returned as
  # it was, holding them. Handles can be taken only once, so a second take,
  # like a take of a call that is gone, finds nothing.
  @spec take_fds(t(), reference()) :: {:ok, Entry.t(), t()} | :error
  def take_fds(%__MODULE__{entries: entries} = calls, ref) do
    case Map.fetch(entries, ref) do
      {:ok, %Entry{taken?: false} = entry} ->
        taken = %{entry | unix_fds: [], taken?: true}
        {:ok, entry, %{calls | entries: Map.put(entries, ref, taken)}}

      _taken_or_missing ->
        :error
    end
  end

  # The expiry timer fired. A timer that fires after the call completed is
  # stale and finds nothing, which is the expected outcome rather than an
  # error: cancelling a timer cannot recall a message it has already sent.
  @spec expire(t(), reference()) :: {Entry.t() | nil, t()}
  def expire(%__MODULE__{} = calls, ref), do: pop(calls, ref)

  # The export the calls were routed through is gone, so every call made on
  # it leaves the table, released, for the connection to fail. The scan is
  # bounded by the connection's limit on pending calls.
  @spec pop_export(t(), reference()) :: {[Entry.t()], t()}
  def pop_export(%__MODULE__{entries: entries} = calls, export) do
    {popped, kept} = Map.split_with(entries, fn {_ref, entry} -> entry.export == export end)
    popped = popped |> Map.values() |> Enum.sort_by(& &1.serial)
    Enum.each(popped, &release/1)
    {popped, %{calls | entries: kept}}
  end

  # Cancels every timer and closes every retained handle without touching the
  # rest of the table. Used from `terminate/2`, where nothing
  # survives the return.
  @spec close_all(t()) :: :ok
  def close_all(%__MODULE__{entries: entries}) do
    Enum.each(entries, fn {_ref, entry} -> release(entry) end)
  end

  # Teardown: closes and forgets everything. Handlers are not told; a later
  # answer finds the connection gone, as a signal handler would.
  @spec fail_all(t()) :: t()
  def fail_all(%__MODULE__{} = calls) do
    close_all(calls)
    new()
  end

  defp pop(%__MODULE__{} = calls, ref) do
    case Map.pop(calls.entries, ref) do
      {nil, _entries} ->
        {nil, calls}

      {%Entry{} = entry, entries} ->
        release(entry)
        {entry, %{calls | entries: entries}}
    end
  end

  defp release(%Entry{timer_ref: timer_ref, unix_fds: fds}) do
    cancel_timer(timer_ref)
    UnixFD.close_owned(fds)
  end

  defp copy(nil), do: nil
  defp copy(sender) when is_binary(sender), do: :binary.copy(sender)

  defp start_timer(_ref, :infinity), do: {nil, nil}

  defp start_timer(ref, call_timeout) do
    deadline = System.monotonic_time(:millisecond) + call_timeout
    {Process.send_after(self(), {:inbound_call_timeout, ref}, call_timeout), deadline}
  end

  defp cancel_timer(nil), do: :ok

  defp cancel_timer(timer_ref) do
    _ = Process.cancel_timer(timer_ref)
    :ok
  end
end
