defmodule Rebus.Connection.Pending do
  @moduledoc false

  # The table of method calls waiting for a reply, together with the two
  # indexes that find one by the caller's request reference or by the monitor
  # held on that caller.
  #
  # Like `Rebus.Connection.Writer` and `Rebus.Connection.FDClaims` this is not
  # a pure structure: an entry owns the request-timeout timer and a monitor on
  # its caller, so taking one out of the table cancels that timer here rather
  # than at each call site. Releasing the monitor is deliberately not part of
  # every removal: a caller that has already gone down has nothing left to
  # release, and a reply carrying descriptors hands its monitor on to the
  # FD-claim table.

  use TypedStruct

  defmodule Entry do
    @moduledoc false

    use TypedStruct

    # One outstanding method call: the serial it went out with, the caller to
    # answer, the timer bounding the wait, and the references it is indexed by.
    # `Rebus.Connection.Writer` builds this when the frame reaches the peer.

    typedstruct enforce: true do
      field :serial, pos_integer()
      field :from, GenServer.from()
      field :timer_ref, reference()
      field :request_ref, reference()
      field :monitor_ref, reference() | nil
      field :deadline, integer()
    end
  end

  typedstruct enforce: true do
    field :entries, %{non_neg_integer() => Entry.t()}, default: %{}
    field :request_index, %{reference() => non_neg_integer()}, default: %{}
    field :monitor_index, %{reference() => non_neg_integer()}, default: %{}
  end

  @spec new() :: t()
  def new, do: %__MODULE__{}

  # Serial allocation asks whether a serial is still spoken for, so the writer
  # is handed the entry map itself rather than a copy of its keys.
  @spec entries(t()) :: %{non_neg_integer() => Entry.t()}
  def entries(%__MODULE__{entries: entries}), do: entries

  @spec put(t(), Entry.t()) :: t()
  def put(%__MODULE__{} = pending, %Entry{} = entry) do
    %{
      pending
      | entries: Map.put(pending.entries, entry.serial, entry),
        request_index: Map.put(pending.request_index, entry.request_ref, entry.serial),
        monitor_index: Map.put(pending.monitor_index, entry.monitor_ref, entry.serial)
    }
  end

  @spec fetch_by_serial(t(), non_neg_integer()) :: {:ok, Entry.t()} | :error
  def fetch_by_serial(%__MODULE__{entries: entries}, serial), do: Map.fetch(entries, serial)

  # The three ways an entry leaves the table. Each cancels the request timer
  # and clears both indexes.
  @spec pop_by_serial(t(), non_neg_integer()) :: {Entry.t() | nil, t()}
  def pop_by_serial(%__MODULE__{} = pending, serial), do: pop(pending, serial)

  @spec pop_by_request(t(), reference()) :: {Entry.t() | nil, t()}
  def pop_by_request(%__MODULE__{} = pending, request_ref) do
    pending |> pop(Map.get(pending.request_index, request_ref)) |> release_popped()
  end

  # The monitor is left alone here: this entry is being dropped because it has
  # already fired.
  @spec pop_by_monitor(t(), reference()) :: {Entry.t() | nil, t()}
  def pop_by_monitor(%__MODULE__{} = pending, monitor_ref),
    do: pop(pending, Map.get(pending.monitor_index, monitor_ref))

  # Stop watching the caller of an entry already taken out of the table.
  @spec release_monitor(Entry.t()) :: :ok
  def release_monitor(%Entry{monitor_ref: monitor_ref}) do
    Process.demonitor(monitor_ref, [:flush])
    :ok
  end

  # Answer the caller of an entry already taken out of the table.
  @spec fail(Entry.t(), term()) :: :ok
  def fail(%Entry{} = entry, reply) do
    release_monitor(entry)
    GenServer.reply(entry.from, reply)
  end

  # Teardown: every caller still waiting for a reply learns the connection is
  # gone.
  @spec fail_all(t()) :: t()
  def fail_all(%__MODULE__{} = pending) do
    Enum.each(pending.entries, fn {_serial, entry} ->
      _ = Process.cancel_timer(entry.timer_ref)
      fail(entry, {:error, :disconnected})
    end)

    new()
  end

  defp pop(%__MODULE__{} = pending, nil), do: {nil, pending}

  defp pop(%__MODULE__{} = pending, serial) do
    case Map.pop(pending.entries, serial) do
      {nil, _entries} ->
        {nil, pending}

      {%Entry{} = entry, entries} ->
        _ = Process.cancel_timer(entry.timer_ref)

        {entry,
         %{
           pending
           | entries: entries,
             request_index: Map.delete(pending.request_index, entry.request_ref),
             monitor_index: Map.delete(pending.monitor_index, entry.monitor_ref)
         }}
    end
  end

  defp release_popped({nil, %__MODULE__{} = pending}), do: {nil, pending}

  defp release_popped({%Entry{} = entry, %__MODULE__{} = pending}) do
    release_monitor(entry)
    {entry, pending}
  end
end
