defmodule Rebus.UnixFD do
  @moduledoc """
  Small, explicit helpers for Unix file descriptors carried by D-Bus messages.

  A descriptor is represented as the non-negative integer returned by the
  operating system.  `Rebus` borrows descriptors supplied for an outbound
  message: it never closes or duplicates them.  A descriptor in
  `Rebus.Message.unix_fds` was received from a peer and is owned by the process
  that successfully receives that message.  The owner must close it exactly
  once, or adopt it with its own OS/OTP API.

  `close/1` consumes an owned descriptor exactly once. Do not call it again:
  operating systems may reuse descriptor numbers, so a second close could close
  an unrelated resource. It is suitable for `after` blocks and does not log
  descriptor values.

  The helpers are available only on Unix systems with OTP's raw-file support.
  They return `{:error, :unsupported}` elsewhere.

  See [Unix file descriptor passing](unix_fds.html).
  """

  alias Rebus.UnixFD.Raw

  @typedoc "A raw, process-owned Unix file descriptor."
  @type t :: non_neg_integer()

  @doc """
  Closes an owned raw Unix file descriptor.

  This function consumes the descriptor.  The caller must ensure it is called
  exactly once and must treat the descriptor as unusable afterwards.  A second
  call is never safe, whatever it returns: the operating system may already
  have handed the same number to an unrelated resource, in which case the
  second call closes that resource and reports `:ok`.  An `{:error, :ebadf}`
  from a second call is therefore luck, not proof that the call was harmless.

  Returns:

    * `:ok` when the descriptor was closed.
    * `{:error, reason}` - a POSIX error atom when the descriptor could not be
      adopted or the operating system refused the close (`:ebadf` for a number
      that is not open), or `:badarg` for a number too large for the operating
      system's descriptor type (above 2^31 - 1).
    * `{:error, :close_failed}` on an unexpected internal failure.
    * `{:error, :invalid_descriptor}` when `fd` is not a non-negative integer.
    * `{:error, :unsupported}` on a non-Unix system, or when this OTP release
      lacks the raw-descriptor primitive.
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
