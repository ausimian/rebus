defmodule Rebus.UnixFD do
  @moduledoc """
  Closes Unix file descriptors received in D-Bus replies.

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

  Returns `:ok` on success or `{:error, reason}` on failure.

  This consumes an adopted descriptor whether it returns `:ok` or a POSIX
  error. Treat the number as unusable and never retry: the operating system may
  already have assigned it to another resource. `:invalid_descriptor` rejects
  a value that is not a non-negative integer, `:close_failed` reports an
  unexpected close failure, and `:unsupported` means the platform cannot
  perform this operation. After `:unsupported`, the caller still owns the
  descriptor and must close it with another API.
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
