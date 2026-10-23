defmodule Rebus.UnixFD do
  @moduledoc """
  Handles for Unix file descriptors.

  A handle owns one open descriptor and closes it exactly once: when any
  holder calls `close/1`, when its owning process exits, or when the last
  reference to it is garbage collected, whichever comes first. A handle never
  exposes its descriptor number except inside `with_fd/2`, and once closed it
  forgets the number, so a stale handle cannot reach an unrelated descriptor
  that the operating system has since given the same number.

  Ownership follows the controlling-process model of ports and sockets. Each
  handle has exactly one owning process, at first the one that created it.
  When the owner exits, for any reason including `:kill`, the descriptor is
  closed at once, even if other processes still hold the handle. Any holder
  may use or close a handle, but only the owner can pass ownership on with
  `controlling_process/2`.

  A handle is local to the node that created it. One that has crossed a
  distribution boundary answers `{:error, :not_local}` to every operation.

  Handles are available on Unix systems, where Rebus builds the native code
  behind them. Elsewhere every function returns `{:error, :unsupported}`.

  See [Unix file descriptor passing](unix_fds.html).
  """

  alias Rebus.UnixFD.{NIF, Raw}

  @enforce_keys [:ref]
  defstruct [:ref]

  @typedoc "A handle that owns one Unix file descriptor."
  @opaque t :: %__MODULE__{ref: reference()}

  @typedoc """
  A raw descriptor number. Rebus borrows a raw descriptor that it sends: it
  never duplicates or closes it.
  """
  @type raw :: non_neg_integer()

  @typedoc false
  @opaque pin :: reference()

  @type error :: :not_local | :unsupported

  # A number that fits a C int, as every descriptor does.
  defguardp is_raw(fd) when is_integer(fd) and fd >= 0 and fd <= 0x7FFFFFFF

  @doc """
  Closes a handle's descriptor.

  Any process holding the handle may close it, not only its owner. Returns
  `{:error, :closed}` if the handle is already closed, by an earlier call or
  because its owner exited, so calling it again is harmless.

  If another process is inside `with_fd/2` on the handle, the handle is closed
  at once but the descriptor stays open until that function returns.
  """
  @spec close(t()) :: :ok | {:error, :closed | error()}
  def close(%__MODULE__{ref: ref}), do: native(ref, &NIF.close_handle/1)

  @doc """
  Makes `pid` the owner of a handle.

  Only the owner can transfer a handle; any other caller gets
  `{:error, :not_owner}`. If `pid` is not alive, the descriptor is closed and
  the call returns `{:error, :noproc}`. A `pid` on another node gets
  `{:error, :not_local}` and leaves the handle as it was.
  """
  @spec controlling_process(t(), pid()) ::
          :ok | {:error, :closed | :noproc | :not_owner | error()}
  def controlling_process(%__MODULE__{ref: ref}, pid) when is_pid(pid) do
    native(ref, &NIF.controlling_process(&1, pid))
  end

  @doc """
  Returns the process that owns a handle.
  """
  @spec owner(t()) :: {:ok, pid()} | {:error, :closed | error()}
  def owner(%__MODULE__{ref: ref}), do: native(ref, &NIF.owner/1)

  @doc """
  Returns whether a handle is open.

  A handle that is not local to this node, or on a platform without handle
  support, is reported as not open.
  """
  @spec open?(t()) :: boolean()
  def open?(%__MODULE__{} = handle), do: match?({:ok, _}, owner(handle))

  @doc """
  Calls `fun` with the handle's raw descriptor number.

  Returns `{:ok, result}` with what `fun` returned, or `{:error, :closed}`
  without calling `fun` if the handle is closed.

  The descriptor stays open until `fun` returns, even if the handle is closed
  or its owner exits meanwhile, and the number is valid only until then. Do
  not keep it, close it, or hand it to anything that outlives `fun`. If the
  calling process is killed inside `fun`, the descriptor is released when the
  process's memory is reclaimed. Exceptions, throws and exits from `fun`
  propagate after the descriptor is released.

  ```elixir
  {:ok, stat} = Rebus.UnixFD.with_fd(handle, fn fd -> File.stat("/dev/fd/\#{fd}") end)
  ```
  """
  @spec with_fd(t(), (raw() -> result)) :: {:ok, result} | {:error, :closed | error()}
        when result: term()
  def with_fd(%__MODULE__{} = handle, fun) when is_function(fun, 1) do
    case pin(handle) do
      {:ok, token, fd} ->
        try do
          {:ok, fun.(fd)}
        after
          unpin(token)
        end

      {:error, _reason} = error ->
        error
    end
  end

  @doc """
  Duplicates a handle's descriptor into a new handle owned by the caller.

  The two handles refer to the same open file but close independently.
  Returns `{:error, :closed}` if the handle is closed, or `{:error, posix}` if
  the operating system cannot duplicate the descriptor.
  """
  @spec dup(t()) :: {:ok, t()} | {:error, :closed | error() | atom()}
  def dup(%__MODULE__{ref: ref}) do
    with {:ok, new_ref} <- native(ref, &NIF.dup_handle/1) do
      {:ok, %__MODULE__{ref: new_ref}}
    end
  end

  # Internal API for the connection.

  @doc false
  # Closes each handle the calling process owns, and leaves the rest alone:
  # one whose ownership has moved on is no longer the caller's to close, and
  # one already closed needs nothing.
  @spec close_owned([t()]) :: :ok
  def close_owned(handles) when is_list(handles) do
    me = self()

    Enum.each(handles, fn handle ->
      # Only the owner can transfer a handle, so one the caller owns stays
      # its own between these two calls; a concurrent close by another
      # holder makes the second answer `{:error, :closed}`.
      with {:ok, ^me} <- owner(handle), do: close(handle)
    end)
  end

  @doc false
  # Closes a received raw descriptor that `wrap/2` refused, when it is still
  # open and the caller's. `wrap/2` has already closed it after `:noproc`,
  # `:not_local` and `:enomem`, and after `:ebadf` or `:invalid_descriptor`
  # there is nothing open under the number: closing it then could only close
  # a descriptor another thread has just been given.
  @spec close_unwrapped(raw(), atom()) :: :ok
  def close_unwrapped(_fd, reason)
      when reason in [:noproc, :not_local, :enomem, :ebadf, :invalid_descriptor],
      do: :ok

  def close_unwrapped(fd, _reason) do
    _ = Raw.close(fd)
    :ok
  end

  @doc false
  # Wraps a raw descriptor in a handle owned by `owner`, a local process.
  # The handle takes over the descriptor: it is closed if `owner` is not
  # alive (`:noproc`) or not local (`:not_local`). It stays the caller's
  # after `:invalid_descriptor`, `:unsupported`, or a POSIX error, which
  # means the number was not open.
  @spec wrap(raw(), pid()) ::
          {:ok, t()} | {:error, :invalid_descriptor | :noproc | error() | atom()}
  def wrap(fd, owner) when is_raw(fd) and is_pid(owner) do
    if supported?() do
      with {:ok, ref} <- NIF.wrap(fd, owner), do: {:ok, %__MODULE__{ref: ref}}
    else
      {:error, :unsupported}
    end
  end

  def wrap(_fd, owner) when is_pid(owner), do: {:error, :invalid_descriptor}

  @doc false
  # Keeps a handle's descriptor open until `unpin/1`, or until the returned
  # token is garbage collected, so the raw number can be used meanwhile. The
  # handle can still be closed; close(2) waits for the last pin.
  @spec pin(t()) :: {:ok, pin(), raw()} | {:error, :closed | error()}
  def pin(%__MODULE__{ref: ref}), do: native(ref, &NIF.pin/1)

  @doc false
  # Releases a pin. It never raises: releasing a pin again, or passing
  # anything that is not a pin token, does nothing.
  @spec unpin(pin()) :: :ok
  def unpin(token) do
    if is_reference(token) and supported?(), do: NIF.unpin(token), else: :ok
  end

  # Test hooks.

  @doc false
  @spec __live_count__() :: non_neg_integer()
  def __live_count__, do: NIF.live_count()

  @doc false
  # How many times close(2) ran for this handle.
  @spec __close_count__(t()) :: {:ok, non_neg_integer()} | {:error, error()}
  def __close_count__(%__MODULE__{ref: ref}), do: native(ref, &NIF.close_count/1)

  @doc false
  # The two ends of a new pipe, owned by the caller.
  @spec __pipe__() :: {:ok, t(), t()} | {:error, atom()}
  def __pipe__ do
    if supported?() do
      with {:ok, read_ref, write_ref} <- NIF.pipe_pair() do
        {:ok, %__MODULE__{ref: read_ref}, %__MODULE__{ref: write_ref}}
      end
    else
      {:error, :unsupported}
    end
  end

  @doc false
  # The {device, inode} that fstat(2) reports for a raw descriptor number.
  @spec __fd_identity__(raw()) :: {:ok, {non_neg_integer(), non_neg_integer()}} | {:error, atom()}
  def __fd_identity__(fd) when is_raw(fd), do: NIF.fd_identity(fd)
  def __fd_identity__(_fd), do: {:error, :invalid_descriptor}

  @doc false
  # The state Inspect shows.
  @spec __state__(t()) :: {:open, pid()} | :closed | error()
  def __state__(%__MODULE__{} = handle) do
    case owner(handle) do
      {:ok, pid} -> {:open, pid}
      {:error, reason} -> reason
    end
  end

  defp native(ref, fun) when is_reference(ref) do
    cond do
      not supported?() -> {:error, :unsupported}
      node(ref) != node() -> {:error, :not_local}
      true -> fun.(ref)
    end
  end

  defp supported?, do: match?({:unix, _}, :os.type())

  defimpl Inspect do
    import Inspect.Algebra

    def inspect(handle, opts) do
      case Rebus.UnixFD.__state__(handle) do
        {:open, owner} -> concat(["#Rebus.UnixFD<open, owner: ", to_doc(owner, opts), ">"])
        state -> "#Rebus.UnixFD<#{state}>"
      end
    end
  end
end
