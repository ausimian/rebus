defmodule Rebus.Connection.Rights do
  @moduledoc false

  # The SCM_RIGHTS half of an inbound D-Bus connection: decoding the control
  # data returned by `recvmsg`, deciding whether the descriptors it carries may
  # be associated with the frame being assembled, and holding them until that
  # frame completes.
  #
  # Nothing here closes a descriptor. Every decision names the descriptors the
  # caller must close, so the single close-or-deliver ownership path stays in
  # one place — the connection — and can be read without following the decoding
  # rules. What the module does own is the quarantine bit: ancillary data
  # rejected before a frame boundary is known belongs to the frame currently
  # being assembled, not to a later coalesced one, and taints only that frame.
  #
  # Like `Rebus.Connection.Inbound` this is pure: no timers, no logging, no
  # socket. Everything it needs from the connection arrives per call in a
  # `t:context/0`.

  use TypedStruct

  alias Rebus.Message
  alias Rebus.UnixFD

  typedstruct enforce: true do
    # Descriptors received with the bytes of the frame currently being
    # assembled, awaiting that frame's completion.
    field :fds, [UnixFD.t()], default: []
    # Set when ancillary data was rejected before the frame it arrived with
    # could be delimited. The descriptors are closed immediately; this bit
    # makes the eventual frame a recoverable drop once its byte boundary is
    # available.
    field :tainted?, boolean(), default: false
  end

  @typedoc """
  Everything a decode borrows from the connection for one `recvmsg` result.
  """
  @type context :: %{
          required(:negotiated?) => boolean(),
          required(:frame_pending?) => boolean(),
          required(:max_bytes) => pos_integer()
        }

  @typedoc """
  What the connection must do with one `recvmsg` result. `:frame` retains the
  descriptors for the frame under assembly; `:quarantine` and `:stop` name
  descriptors the connection must close, exactly once.
  """
  @type decision ::
          {:frame, binary(), [UnixFD.t()]}
          | {:quarantine, binary(), [UnixFD.t()]}
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
  @spec taint(t()) :: t()
  def taint(%__MODULE__{} = rights), do: %{rights | tainted?: true}

  # Only ever reached with an empty buffer: `decide/3` quarantines rather than
  # retaining a second batch of descriptors for one frame.
  @doc false
  @spec retain(t(), [UnixFD.t()]) :: t()
  def retain(%__MODULE__{} = rights, []), do: rights
  def retain(%__MODULE__{} = rights, fds), do: %{rights | fds: fds}

  @doc false
  @spec decode(term(), t(), context()) :: decision()
  def decode(message, rights, context)

  def decode(%{iov: iov, ctrl: ctrl, flags: flags}, %__MODULE__{} = rights, context)
      when is_list(ctrl) and is_list(flags) do
    fds = recvmsg_fds(ctrl, flags)

    case recvmsg_data(iov, context.max_bytes) do
      {:ok, data} ->
        decide(fds, data, %{context | frame_pending?: context.frame_pending? or holding?(rights)})

      {:error, reason} ->
        # Validate control data before iodata so descriptors cannot leak when
        # the recvmsg shape is invalid. No frame bytes are usable in this case.
        {:stop, reason, received_fds(fds)}
    end
  end

  def decode(%{ctrl: ctrl, flags: flags}, %__MODULE__{}, _context)
      when is_list(ctrl) and is_list(flags),
      do: {:stop, :invalid_unix_fds, received_fds(recvmsg_fds(ctrl, flags))}

  def decode(%{ctrl: ctrl}, %__MODULE__{}, _context) when is_list(ctrl),
    do: {:stop, :invalid_unix_fds, received_fds(recvmsg_fds(ctrl, []))}

  def decode(_message, %__MODULE__{}, _context), do: {:stop, :invalid_unix_fds, []}

  @doc false
  @spec decide(fds_result(), binary(), context()) :: decision()
  def decide({:ok, []}, data, _context), do: {:frame, data, []}

  def decide({:ok, fds}, data, context) do
    cond do
      not context.negotiated? ->
        quarantine(data, fds)

      data == <<>> ->
        # A rights-only recvmsg result has no byte offset to associate with a
        # D-Bus frame, so it cannot be recovered without risking later frame
        # ownership.
        {:stop, :invalid_unix_fds, fds}

      context.frame_pending? ->
        quarantine(data, fds)

      true ->
        {:frame, data, fds}
    end
  end

  def decide({:error, :unix_fd_truncated, fds}, _data, _context) do
    # MSG_CTRUNC means the kernel may have installed descriptors omitted from
    # the returned control data. Their identities are unknowable, so this
    # cannot be quarantined frame-locally and must fail closed.
    {:stop, :unix_fd_truncated, fds}
  end

  def decide({:error, _reason, fds}, data, _context) do
    # We decoded every complete descriptor before finding the malformed or
    # oversized tail. Close them now and drop only the byte-aligned frame.
    quarantine(data, fds)
  end

  defp quarantine(<<>>, fds), do: {:stop, :invalid_unix_fds, fds}
  defp quarantine(data, fds), do: {:quarantine, data, fds}

  # The retained descriptors belong to the frame that has just been parsed.
  # Count, index and negotiation validation therefore runs here, once the
  # stream boundary is known. A rejection names the descriptors to close and
  # clears the buffer either way: they are never carried into a later frame.
  @doc false
  @spec attach(t(), Message.t(), boolean()) ::
          {:ok, Message.t(), t()} | {:error, atom(), [UnixFD.t()], t()}
  def attach(%__MODULE__{fds: fds, tainted?: tainted?}, %Message{} = msg, negotiated?) do
    with :ok <- frame_clean?(tainted?),
         :ok <- negotiated?(msg, negotiated?),
         {:ok, msg} <- Message.attach_unix_fds(msg, fds) do
      {:ok, msg, new()}
    else
      {:error, reason} -> {:error, reason, fds, new()}
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

  defp negotiated?(%Message{header_fields: header_fields, unix_fds: fds}, negotiated?) do
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
