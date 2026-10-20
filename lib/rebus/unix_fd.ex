defmodule Rebus.UnixFD do
  @moduledoc """
  Closes Unix file descriptors received from a peer.

  A descriptor is the non-negative integer the operating system returned for
  it. Rebus borrows the descriptors given to it for an outbound message and
  never closes or duplicates them. A descriptor received in a reply to
  `Rebus.call/3` belongs to the process that received the message, which
  must close it exactly once, with `close/1` or an OS or OTP API of its own.

  Descriptors on an inbound method call are the exception. The handler of a
  `Rebus.Service.Call` borrows them from the connection, which closes them
  when the call completes. Close one only after taking it with
  `Rebus.Service.take_fds/2`.

  Never close a descriptor twice. Operating systems reuse descriptor
  numbers, so a second close can close an unrelated resource. `close/1` is
  safe in an `after` block and does not log descriptor values.

  These functions need a Unix system with OTP's raw-file support, and return
  `{:error, :unsupported}` elsewhere. See
  [Unix file descriptor passing](unix_fds.html).
  """

  alias Rebus.UnixFD.Raw

  @typedoc "A raw Unix file descriptor, owned by a process or borrowed from one."
  @type t :: non_neg_integer()

  @doc """
  Closes an owned raw Unix file descriptor.

  The descriptor is spent whether this returns `:ok` or a POSIX error, so
  treat the number as unusable afterwards and never retry: the operating
  system may already have given it to another resource.

  ## Return values

  - `:ok` - the descriptor was closed.
  - `{:error, :invalid_descriptor}` - `fd` is not a non-negative integer.
  - `{:error, :close_failed}` - the close failed unexpectedly. The descriptor
    is still spent.
  - `{:error, :unsupported}` - the platform cannot do this. The caller still
    owns the descriptor and must close it with another API.
  - `{:error, reason}` - a POSIX error from the close. The descriptor is
    spent.
  """
  @spec close(t()) :: :ok | {:error, :close_failed | :invalid_descriptor | :unsupported | term()}
  def close(fd) when is_integer(fd) and fd >= 0 do
    case :os.type() do
      {:unix, _} -> Raw.close(fd)
      _ -> {:error, :unsupported}
    end
  end

  def close(_fd), do: {:error, :invalid_descriptor}

  @doc false
  @spec close_all([t()]) :: :ok
  def close_all(fds) when is_list(fds) do
    Enum.each(fds, &close/1)
    :ok
  end
end
