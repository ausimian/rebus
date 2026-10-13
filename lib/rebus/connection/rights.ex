defmodule Rebus.Connection.Rights do
  @moduledoc false

  # The SCM_RIGHTS half of an inbound D-Bus connection: decoding the control
  # data returned by `recvmsg`, holding the descriptors it carries, and
  # attributing them to the frames parsed from the same bytes.
  #
  # Reads are not aligned to frames, so one `recvmsg` can return the tail of
  # one frame, several whole frames and the head of another. Like libdbus and
  # sd-bus, descriptors are attributed by each frame's `unix_fds` header field,
  # in order. Unlike them, a batch is offered only to the frames that begin
  # inside the bytes it arrived with. When a sender attaches a message's rights
  # to the write that starts the message, as libdbus, sd-bus and dbus-daemon
  # do, both kernels deliver them with bytes in which that message begins:
  # Linux ends a stream read with the one skb that carried rights, and macOS
  # starts a read at the record that carries them. So a batch is never carried
  # into a frame that began after its read, and descriptors no frame in range
  # claims are closed rather than queued for a later one.
  #
  # Nothing here closes a descriptor. Every decision names the descriptors the
  # caller must close, so the single close-or-deliver ownership path stays in
  # one place — the connection — and can be read without following the
  # attribution rules. What the module does own is quarantine: control data
  # rejected on receipt poisons the range it arrived with, so any frame
  # beginning there that declares descriptors is dropped, and rights arriving
  # where no frame begins taint the frame being assembled.
  #
  # Like `Rebus.Connection.Inbound` this is pure: no timers, no logging, no
  # socket. Everything it needs from the connection arrives per call in a
  # `t:context/0`.

  use TypedStruct

  alias Rebus.Connection.Inbound
  alias Rebus.Message
  alias Rebus.UnixFD

  @typedoc """
  The descriptors one `recvmsg` result carried, as a range of stream offsets.
  Frames beginning in `[start, stop)` claim the first `count` of the held
  descriptors in order. A batch with a `reason` was rejected on receipt: its
  descriptors are already closed and a frame claiming from it is dropped.
  """
  @type batch :: %{
          start: non_neg_integer(),
          stop: pos_integer(),
          count: non_neg_integer(),
          reason: atom() | nil
        }

  typedstruct enforce: true do
    # Received descriptors not yet claimed by a frame, in arrival order. The
    # batches partition this list.
    field :fds, [UnixFD.t()], default: []
    # Oldest first. A batch outlives its read only while the frame being
    # assembled began inside it, so there is at most one between receives.
    field :batches, [batch()], default: []
    # The stream offset of the frame at the head of the inbound buffer.
    field :offset, non_neg_integer(), default: 0
    # Set when rights arrived inside the frame being assembled. The
    # descriptors are closed immediately; this bit drops that frame once its
    # byte boundary is available.
    field :tainted?, boolean(), default: false
  end

  @typedoc """
  Everything a decode borrows from the connection for one `recvmsg` result.
  `inbound` is the receive buffer before the result's bytes are appended.
  """
  @type context :: %{
          required(:negotiated?) => boolean(),
          required(:inbound) => Inbound.t(),
          required(:max_bytes) => pos_integer()
        }

  @typedoc """
  How one `recvmsg` result's descriptors are handled. `:frame` holds them for
  the frames beginning in its bytes. `:quarantine` poisons that range,
  `:taint` drops the frame being assembled, and `:stop` ends the connection;
  each names descriptors the connection must close, exactly once.
  """
  @type decision ::
          {:frame, binary(), [UnixFD.t()]}
          | {:quarantine, binary(), [UnixFD.t()], atom()}
          | {:taint, binary(), [UnixFD.t()]}
          | {:stop, atom(), [UnixFD.t()]}

  @typedoc false
  @type fds_result :: {:ok, [UnixFD.t()]} | {:error, atom(), [UnixFD.t()]}

  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc false
  @spec fds(t()) :: [UnixFD.t()]
  def fds(%__MODULE__{fds: fds}), do: fds

  @doc false
  @spec holding?(t()) :: boolean()
  def holding?(%__MODULE__{fds: fds}), do: fds != []

  @doc false
  @spec quarantined?(t()) :: boolean()
  def quarantined?(%__MODULE__{tainted?: tainted?, batches: batches}),
    do: tainted? or Enum.any?(batches, &(&1.reason != nil))

  @doc false
  @spec taint(t()) :: t()
  def taint(%__MODULE__{} = rights), do: %{rights | tainted?: true}

  # Hold descriptors for the frame at the head of the inbound buffer alone.
  # Received batches are added by `decode/3`, which knows their byte range.
  @doc false
  @spec retain(t(), [UnixFD.t()]) :: t()
  def retain(%__MODULE__{} = rights, []), do: rights

  def retain(%__MODULE__{offset: offset} = rights, fds),
    do: add_batch(rights, offset, offset + 1, fds, nil)

  # Decode one `recvmsg` result and hold its descriptors for the frames that
  # begin in its bytes. The bytes are returned for the caller to append, along
  # with any descriptors rejected on receipt, which the caller must close.
  @doc false
  @spec decode(term(), t(), context()) ::
          {:ok, binary(), t(), [UnixFD.t()]} | {:stop, atom(), [UnixFD.t()]}
  def decode(message, %__MODULE__{} = rights, context) do
    case decode_message(message, context) do
      {:frame, data, fds} ->
        {:ok, data, add_received(rights, context.inbound, data, fds, nil), []}

      {:quarantine, data, fds, reason} ->
        {:ok, data, add_received(rights, context.inbound, data, [], reason), fds}

      {:taint, data, fds} ->
        {:ok, data, taint(rights), fds}

      {:stop, reason, fds} ->
        {:stop, reason, fds}
    end
  end

  defp decode_message(%{iov: iov, ctrl: ctrl, flags: flags}, context)
       when is_list(ctrl) and is_list(flags) do
    fds = recvmsg_fds(ctrl, flags)

    case recvmsg_data(iov, context.max_bytes) do
      {:ok, data} ->
        decide(fds, data, Map.put(context, :starts_frame?, starts_frame?(fds, context, data)))

      {:error, reason} ->
        # Validate control data before iodata so descriptors cannot leak when
        # the recvmsg shape is invalid. No frame bytes are usable in this case.
        {:stop, reason, received_fds(fds)}
    end
  end

  defp decode_message(%{ctrl: ctrl, flags: flags}, _context)
       when is_list(ctrl) and is_list(flags),
       do: {:stop, :invalid_unix_fds, received_fds(recvmsg_fds(ctrl, flags))}

  defp decode_message(%{ctrl: ctrl}, _context) when is_list(ctrl),
    do: {:stop, :invalid_unix_fds, received_fds(recvmsg_fds(ctrl, []))}

  defp decode_message(_message, _context), do: {:stop, :invalid_unix_fds, []}

  # Locating a frame boundary costs a header peek, so it is only done for a
  # result that carried descriptors.
  defp starts_frame?({:ok, []}, _context, _data), do: true
  defp starts_frame?(_fds, %{inbound: inbound}, data), do: Inbound.starts_frame?(inbound, data)

  @doc false
  @spec decide(fds_result(), binary(), map()) :: decision()
  def decide({:ok, []}, data, _context), do: {:frame, data, []}

  def decide({:ok, fds}, data, context) do
    cond do
      data == <<>> ->
        # A rights-only recvmsg result has no byte offset to associate with a
        # D-Bus frame, so it cannot be recovered without risking later frame
        # ownership.
        {:stop, :invalid_unix_fds, fds}

      not context.negotiated? ->
        quarantine(data, fds, :unix_fd_not_negotiated, context)

      context.starts_frame? ->
        {:frame, data, fds}

      true ->
        # No frame begins in these bytes, so the rights were sent part way
        # through the frame being assembled.
        {:taint, data, fds}
    end
  end

  def decide({:error, :unix_fd_truncated, fds}, _data, _context) do
    # MSG_CTRUNC means the kernel may have installed descriptors omitted from
    # the returned control data. Their identities are unknowable, so this
    # cannot be quarantined locally and must fail closed.
    {:stop, :unix_fd_truncated, fds}
  end

  def decide({:error, reason, fds}, data, context) do
    # We decoded every complete descriptor before finding the malformed or
    # oversized tail. Close them now and drop only the frames they were for.
    quarantine(data, fds, reason, context)
  end

  defp quarantine(<<>>, fds, _reason, _context), do: {:stop, :invalid_unix_fds, fds}
  defp quarantine(data, fds, reason, %{starts_frame?: true}), do: {:quarantine, data, fds, reason}
  defp quarantine(data, fds, _reason, _context), do: {:taint, data, fds}

  defp add_received(rights, inbound, data, fds, reason) do
    if fds == [] and is_nil(reason) do
      rights
    else
      start = rights.offset + inbound.size
      add_batch(rights, start, start + byte_size(data), fds, reason)
    end
  end

  defp add_batch(rights, start, stop, fds, reason) do
    batch = %{start: start, stop: stop, count: length(fds), reason: reason}
    %{rights | fds: rights.fds ++ fds, batches: rights.batches ++ [batch]}
  end

  # Release every batch no further frame can begin in: those whose bytes end
  # at or before the head frame. Their unclaimed descriptors are named for the
  # caller to close, never carried into a later frame.
  @doc false
  @spec expire(t()) :: {[UnixFD.t()], t()}
  def expire(%__MODULE__{batches: [%{stop: stop} = batch | batches], offset: offset} = rights)
      when stop <= offset do
    {unclaimed, fds} = Enum.split(rights.fds, batch.count)
    {more, rights} = expire(%{rights | fds: fds, batches: batches})
    {unclaimed ++ more, rights}
  end

  def expire(%__MODULE__{} = rights), do: {[], rights}

  # Attribute descriptors to the head frame, `size` bytes long, which has just
  # been parsed, and advance past it. The caller expires stale batches first.
  # Count, index and negotiation validation runs here, once the frame is
  # known. A rejection names the descriptors the frame claimed: they are
  # closed with it, never carried into a later frame.
  @doc false
  @spec attach(t(), Message.t(), pos_integer(), boolean()) ::
          {:ok, Message.t(), t()} | {:error, atom(), [UnixFD.t()], t()}
  def attach(%__MODULE__{} = rights, %Message{} = msg, size, negotiated?) do
    {fds, rejected, advanced} = claim(rights, declared_unix_fds(msg), size)

    with :ok <- frame_clean?(rights.tainted?),
         :ok <- batch_clean?(rejected),
         :ok <- negotiated?(msg, fds, negotiated?),
         {:ok, msg} <- Message.attach_unix_fds(msg, fds) do
      {:ok, msg, advanced}
    else
      {:error, reason} -> {:error, reason, fds, advanced}
    end
  end

  # Advance past a head frame that was dropped before its header could be
  # trusted. Its declared count is unknown, so the rest of its batch cannot be
  # attributed safely: those descriptors are named for closing and the batch
  # is poisoned for any later frame in its range.
  @doc false
  @spec skip(t(), pos_integer()) :: {[UnixFD.t()], t()}
  def skip(%__MODULE__{} = rights, size) do
    case in_scope(rights) do
      {%{reason: nil, count: count} = batch, batches} ->
        {fds, rest} = Enum.split(rights.fds, count)
        poisoned = %{batch | count: 0, reason: :invalid_unix_fds}
        {fds, advance(%{rights | fds: rest, batches: [poisoned | batches]}, size)}

      _otherwise ->
        {[], advance(rights, size)}
    end
  end

  # The head frame claims its declared count, in order, from the batch whose
  # bytes it begins in. A shortfall takes what is there, so the count check
  # fails and those descriptors close with the frame.
  defp claim(rights, 0, size), do: {[], nil, advance(rights, size)}

  defp claim(rights, declared, size) do
    case in_scope(rights) do
      {%{reason: nil, count: count} = batch, batches} ->
        taken = min(declared, count)
        {fds, rest} = Enum.split(rights.fds, taken)
        batch = %{batch | count: count - taken}
        {fds, nil, advance(%{rights | fds: rest, batches: [batch | batches]}, size)}

      {%{reason: reason}, _batches} ->
        {[], reason, advance(rights, size)}

      nil ->
        {[], nil, advance(rights, size)}
    end
  end

  defp in_scope(%__MODULE__{
         batches: [%{start: start, stop: stop} = batch | batches],
         offset: offset
       })
       when start <= offset and offset < stop,
       do: {batch, batches}

  defp in_scope(%__MODULE__{}), do: nil

  defp advance(rights, size), do: %{rights | offset: rights.offset + size, tainted?: false}

  defp declared_unix_fds(%Message{header_fields: header_fields}) do
    case Map.get(header_fields, :unix_fds, 0) do
      count when is_integer(count) and count > 0 -> count
      _none -> 0
    end
  end

  @doc false
  @spec drop_reason(term()) :: :invalid_unix_fds | :unix_fd_not_negotiated | :unix_fd_limit
  def drop_reason(reason)
      when reason in [:invalid_unix_fds, :unix_fd_not_negotiated, :unix_fd_limit],
      do: reason

  def drop_reason(_reason), do: :invalid_unix_fds

  defp frame_clean?(false), do: :ok
  defp frame_clean?(true), do: {:error, :invalid_unix_fds}

  defp batch_clean?(nil), do: :ok
  defp batch_clean?(reason), do: {:error, reason}

  defp negotiated?(%Message{header_fields: header_fields}, fds, negotiated?) do
    if negotiated? or (Map.get(header_fields, :unix_fds, 0) == 0 and fds == []) do
      :ok
    else
      {:error, :unix_fd_not_negotiated}
    end
  end

  @doc false
  @spec recvmsg_data(iodata(), pos_integer()) :: {:ok, binary()} | {:error, atom()}
  def recvmsg_data(iov, max_bytes) do
    data = IO.iodata_to_binary(iov)

    if byte_size(data) <= max_bytes,
      do: {:ok, data},
      else: {:error, :message_too_large}
  rescue
    ArgumentError -> {:error, :invalid_unix_fds}
  end

  @doc false
  @spec recvmsg_fds([map()], [atom()]) :: fds_result()
  def recvmsg_fds(ctrl, flags) do
    case {extract_rights_fds(ctrl), :ctrunc in flags} do
      {{:ok, fds}, true} ->
        {:error, :unix_fd_truncated, fds}

      {{:error, _reason, fds}, true} ->
        # Preserve every complete descriptor decoded before the malformed or
        # oversized tail so the single fail-closed path can close it. CTRUNC
        # takes precedence because the kernel may have omitted more descriptors
        # whose identities are unknowable.
        {:error, :unix_fd_truncated, fds}

      {result, _ctrunc?} ->
        result
    end
  end

  @doc false
  @spec extract_rights_fds([map()]) :: fds_result()
  def extract_rights_fds(ctrl) do
    {fds, reason} =
      Enum.reduce(ctrl, {[], nil}, fn
        %{level: :socket, type: :rights, data: data}, {fds, reason} when is_binary(data) ->
          case decode_rights_data(data) do
            {:ok, received} ->
              append_received_fds(fds, received, reason)

            # A malformed control payload can still contain complete
            # descriptors before its invalid tail. Continue scanning later
            # cmsgs too, retaining every descriptor for the single close path.
            {:error, received} ->
              append_received_fds(fds, received, reason || :invalid_unix_fds)
          end

        # An SCM_RIGHTS item with a non-binary payload must fail closed, but
        # later rights cmsgs can still carry descriptors which must be closed.
        %{level: :socket, type: :rights}, {fds, reason} ->
          {fds, reason || :invalid_unix_fds}

        _cmsg, acc ->
          acc
      end)

    case reason do
      nil -> {:ok, fds}
      reason -> {:error, reason, fds}
    end
  end

  @doc false
  @spec decode_rights_data(binary()) :: {:ok, [UnixFD.t()]} | {:error, [UnixFD.t()]}
  def decode_rights_data(data) do
    complete_size = div(byte_size(data), 4) * 4
    <<complete::binary-size(^complete_size), _tail::binary>> = data
    fds = for <<fd::native-signed-32 <- complete>>, do: fd

    cond do
      Enum.any?(fds, &(&1 < 0)) -> {:error, fds}
      complete_size == byte_size(data) -> {:ok, fds}
      true -> {:error, fds}
    end
  end

  defp append_received_fds(fds, received, reason) do
    fds = fds ++ received

    reason =
      reason ||
        if length(fds) > Message.max_unix_fds(), do: :unix_fd_limit

    {fds, reason}
  end

  defp received_fds({:ok, fds}), do: fds
  defp received_fds({:error, _reason, fds}), do: fds
end
