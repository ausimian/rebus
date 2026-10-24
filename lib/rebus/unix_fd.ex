defmodule Rebus.UnixFD do
  @moduledoc """
  Handles for Unix file descriptors.

  Rebus hands every descriptor it receives to your code as a handle: in the
  `unix_fds` of a `Rebus.call/3` reply, and in the `unix_fds` of a
  `Rebus.Service.Call`. A handle owns one open descriptor and closes it
  exactly once: when any holder calls `close/1`, when its owning process
  exits, or when the last reference to it is garbage collected, whichever
  comes first. A handle never exposes its descriptor number except inside
  `with_fd/2`, and once closed it forgets the number, so a stale handle
  cannot reach an unrelated descriptor that the operating system has since
  given the same number.

  ## Ownership

  Ownership follows the controlling-process model of ports and sockets. Each
  handle has exactly one owning process. When the owner exits, for any
  reason including `:kill`, the handle closes at once, even if other
  processes still hold it, and reads closed for them too. A background
  thread then closes the descriptor, so a close that blocks cannot stall a
  scheduler; the same happens when a handle is garbage collected.

  Any process holding a handle may use it, send it, duplicate it with
  `dup/1`, or close it, but only the owner can pass ownership on with
  `controlling_process/2`. `owner/1` and `open?/1` tell who owns a handle and
  whether it is still open:

      {:ok, %Rebus.Message{unix_fds: [handle]}} = Rebus.call(conn, message)
      {:ok, pid} = Rebus.UnixFD.owner(handle)   # pid == self()
      :ok = Rebus.UnixFD.controlling_process(handle, worker)
      true = Rebus.UnixFD.open?(handle)

  ## Using the descriptor

  `with_fd/2` lends the raw number to a function and keeps the descriptor
  open until the function returns, even if the handle is closed meanwhile.
  Hand the number to something that takes its own copy, such as
  `:socket.open/2`, or open `/dev/fd/N`:

      {:ok, {:ok, sock}} =
        Rebus.UnixFD.with_fd(handle, fn fd ->
          :socket.open(fd, %{domain: :local, type: :stream})
        end)

      :ok = Rebus.UnixFD.close(handle)

  ## Sending

  A handle can be sent in the `fds:` of `Rebus.Message.new/2` and the reply
  functions of `Rebus.Service`, by its owner or any other holder. Sending
  neither closes nor transfers it. The connection keeps the descriptor open
  until the kernel has taken it, even if the handle is closed or its owner
  exits meanwhile, and refuses one that is already closed with
  `{:error, :unix_fd_closed}`.

  ## Distribution and platforms

  A handle is local to the node that created it. One that has crossed to
  another node answers `{:error, :not_local}` to every operation there; back
  on its own node, it works again.

  Handles need a small native library, which Rebus builds on Linux and macOS
  only when the application enables Unix fd support with
  `config :rebus, unix_fds: true` (see
  [Enabling descriptor passing](unix_fds.html#enabling-descriptor-passing)).
  Without it every function returns `{:error, :unsupported}`; `supported?/0`
  tells which applies.

  See [Unix file descriptor passing](unix_fds.html) for the full model.
  """

  alias Rebus.UnixFD.NIF

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
  Returns whether this build of Rebus supports handles, and so can pass
  descriptors.

  It is true on Linux and macOS when the application enabled Unix fd support
  with `config :rebus, unix_fds: true` in its `config/config.exs`. A library
  built on Rebus cannot enable it for its users, so one that needs descriptor
  passing can check this at startup and tell them to.

  ```elixir
  unless Rebus.UnixFD.supported?() do
    raise "add `config :rebus, unix_fds: true` to config/config.exs"
  end
  ```
  """
  @spec supported?() :: boolean()
  def supported?, do: NIF.enabled?()

  @doc """
  Closes a handle's descriptor.

  Any process holding the handle may close it, not only its owner. Returns
  `{:error, :closed}` if the handle is already closed, by an earlier call or
  because its owner exited, so calling it again is harmless.

  The descriptor is closed when this returns `:ok`. Closing it runs on a
  dirty I/O scheduler, so if `close(2)` blocks, for instance on a socket
  with lingering data, only the caller waits.

  If another process is inside `with_fd/2` on the handle, or a connection is
  still sending it, the handle is closed at once but the descriptor stays
  open until that function returns or the send has passed it on.

  ```elixir
  :ok = Rebus.UnixFD.close(handle)
  {:error, :closed} = Rebus.UnixFD.close(handle)
  ```
  """
  @spec close(t()) :: :ok | {:error, :closed | error()}
  def close(%__MODULE__{ref: ref}), do: native(ref, &NIF.close_handle/1)

  @doc """
  Makes `pid` the owner of a handle.

  Only the owner can transfer a handle; any other caller gets
  `{:error, :not_owner}`. A closed handle answers `{:error, :closed}`,
  whoever asks. If `pid` is not alive, the handle is closed and the call
  returns `{:error, :noproc}`. A `pid` on another node gets
  `{:error, :not_local}` and leaves the handle as it was. Making the current
  owner the owner again does nothing.

  ```elixir
  worker = spawn(fn -> receive do {:take, handle} -> use_and_close(handle) end end)
  :ok = Rebus.UnixFD.controlling_process(handle, worker)
  send(worker, {:take, handle})
  ```
  """
  @spec controlling_process(t(), pid()) ::
          :ok | {:error, :closed | :noproc | :not_owner | error()}
  def controlling_process(%__MODULE__{ref: ref}, pid) when is_pid(pid) do
    native(ref, &NIF.controlling_process(&1, pid))
  end

  @doc """
  Returns the process that owns a handle, or `{:error, :closed}` once it is
  closed.

  ```elixir
  {:ok, owner} = Rebus.UnixFD.owner(handle)
  ```
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
  not keep it, close it, or hand it to anything that outlives `fun` without
  taking its own copy, as `:socket.open/2` does. To keep a descriptor of your
  own, use `dup/1`. If the calling process is killed inside `fun`, the
  descriptor is released when the process's memory is reclaimed.
  Exceptions, throws and exits from `fun` propagate after the descriptor is
  released.

  ```elixir
  {:ok, {:ok, contents}} = Rebus.UnixFD.with_fd(handle, &File.read("/dev/fd/\#{&1}"))
  ```

  Returning the number from `fun`, as in `{:ok, fd} = with_fd(handle, & &1)`,
  gives you a number that may already be closed, or reused for something
  else, by the time you use it.
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

  The two handles refer to the same open file but close independently. The
  new handle is owned by the process that calls this, whether or not it owns
  the original. Returns `{:error, :closed}` if the handle is closed, or
  `{:error, posix}` if the operating system cannot duplicate the descriptor.

  ```elixir
  {:ok, mine} = Rebus.UnixFD.dup(borrowed)
  ```
  """
  @spec dup(t()) :: {:ok, t()} | {:error, :closed | error() | atom()}
  def dup(%__MODULE__{ref: ref}) do
    with {:ok, new_ref} <- native(ref, &NIF.dup_handle/1) do
      {:ok, %__MODULE__{ref: new_ref}}
    end
  end

  # Internal API for the connection.

  @doc false
  # Closes a handle as `close/1` does, but returns without waiting for
  # close(2), which a background thread runs, so a close that blocks cannot
  # hold up the caller.
  @spec close_async(t()) :: :ok | {:error, :closed | error()}
  def close_async(%__MODULE__{ref: ref}), do: native(ref, &NIF.close_handle_async/1)

  @doc false
  # Closes each handle the calling process owns, as `close_async/1` does, and
  # leaves the rest alone: one whose ownership has moved on is no longer the
  # caller's to close, and one already closed needs nothing.
  @spec close_owned([t()]) :: :ok
  def close_owned(handles) when is_list(handles) do
    me = self()

    Enum.each(handles, fn handle ->
      # Only the owner can transfer a handle, so one the caller owns stays
      # its own between these two calls; a concurrent close by another
      # holder makes the second answer `{:error, :closed}`.
      with {:ok, ^me} <- owner(handle), do: close_async(handle)
    end)
  end

  @doc false
  # Wraps a raw descriptor in a handle owned by `owner`, a local process.
  # The handle takes over the descriptor, which is closed on any refusal
  # except three: after `:ebadf` nothing is open under the number, after
  # `:invalid_descriptor` it is no descriptor number at all, and after
  # `:unsupported` it stays the caller's, since without the native library
  # nothing here can close it.
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

  @doc false
  # Pins every handle in a list of outbound descriptors, handles and raw
  # numbers in any mix, and returns the raw number of each in the same order
  # together with the pins taken. A raw number passes through unpinned. If a
  # handle cannot be pinned, because it is closed or not local, the pins
  # already taken are released and nothing stays pinned. The same happens if
  # the list is not a proper list.
  @spec pin_all([t() | raw()]) :: {:ok, [raw()], [pin()]} | {:error, :closed | :invalid}
  def pin_all(fds), do: pin_all(fds, [], [])

  defp pin_all([], raw, pins), do: {:ok, Enum.reverse(raw), pins}

  defp pin_all([%__MODULE__{ref: ref} = handle | rest], raw, pins) when is_reference(ref) do
    case pin(handle) do
      {:ok, pin, fd} -> pin_all(rest, [fd | raw], [pin | pins])
      {:error, _reason} -> release_all(pins, {:error, :closed})
    end
  end

  # A struct that is not a real handle has no descriptor to send.
  defp pin_all([%__MODULE__{} | _rest], _raw, pins), do: release_all(pins, {:error, :closed})
  defp pin_all([fd | rest], raw, pins), do: pin_all(rest, [fd | raw], pins)
  defp pin_all(_improper, _raw, pins), do: release_all(pins, {:error, :invalid})

  defp release_all(pins, result) do
    unpin_all(pins)
    result
  end

  @doc false
  # Releases every pin in a list, as `unpin/1` does each one.
  @spec unpin_all([pin()]) :: :ok
  def unpin_all(pins) when is_list(pins), do: Enum.each(pins, &unpin/1)

  # Test hooks.

  @doc false
  @spec __live_count__() :: non_neg_integer()
  def __live_count__, do: if(supported?(), do: NIF.live_count(), else: 0)

  @doc false
  # Waits until the background thread has run every close(2) queued so far.
  @spec __drain__() :: :ok
  def __drain__, do: if(supported?(), do: NIF.drain(), else: :ok)

  @doc false
  # How many times close(2) ran on a normal scheduler thread, which it never
  # should.
  @spec __scheduler_closes__() :: non_neg_integer()
  def __scheduler_closes__, do: if(supported?(), do: NIF.scheduler_close_count(), else: 0)

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
  def __fd_identity__(fd) when is_raw(fd),
    do: if(supported?(), do: NIF.fd_identity(fd), else: {:error, :unsupported})

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
