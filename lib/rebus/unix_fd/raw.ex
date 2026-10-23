# The only place under `lib/` that names a private OTP module.
#
# Why a private API: OTP exposes no public function that closes an arbitrary,
# already-open descriptor number.  `:file` cannot adopt one - it only hands
# back descriptors it opened itself - and `:socket.open/1`, which does adopt an
# existing descriptor, is no use here for two reasons: it adopts sockets only,
# so it cannot cover the pipes and regular files a peer may pass over
# SCM_RIGHTS, and even for a socket it needs the domain and type, which a
# received descriptor does not carry - `:socket.open(fd)` on a live
# `:local`/`:stream` descriptor returns
# `{:error, {:invalid, {:options, :domain, %{}}}}`.
#
# What exercises it: the CI matrix in `.github/workflows/ci.yml` runs the whole
# suite, including this module, on OTP 27, 28 and 29, on both Linux and macOS.
#
# What guards it: the compatibility test "the private OTP close primitive is
# still exported" in `test/rebus/unix_fd_test.exs`.  It asserts that
# `:prim_file.file_desc_to_ref/2` is still exported, so an OTP release that
# removes or renames it fails the suite loudly on the first matrix cell rather
# than degrading `close/1` to `{:error, :unsupported}` at runtime.
#
# Why the contract is "consumed regardless of the result": `:file.close/1`
# reaches `prim_file:close/1` (`erts/preloaded/src/prim_file.erl:154` at
# OTP-28.3), whose `close_nif` moves the reference to its CLOSED state and
# demonitors the owner before calling the OS.  The unix backend clears the
# descriptor from the resource and releases it *before* `close(2)`, so an
# error is reported after the reference has already let go
# (`erts/emulator/nifs/unix/unix_prim_file.c:246`,
# `erts/emulator/nifs/common/prim_file_nif.c:618`, OTP-28.3).  A second
# `:file.close/1` on the same reference returns `{:error, :einval}`.  Nothing
# closes the number a second time: `prim_file`'s janitor (`delayed_close_nif/1`
# from `helper_loop/0`, prim_file.erl:112) is reached only from the
# owner-death monitor, which an explicit close has removed.  So on
# `{:error, _}` this adapter must not retry, and neither can the caller.
#
# Migration: this module closes the raw descriptors the connection still
# carries.  Once the connection carries `Rebus.UnixFD` handles instead, whose
# NIF closes descriptors itself (c_src/rebus_unix_fd.cpp), this module and its
# compatibility test are deleted (#261).
defmodule Rebus.UnixFD.Raw do
  @moduledoc false

  @doc """
  Closes an owned raw descriptor, reporting the outcome.

  This consumes the descriptor whether it returns `:ok` or a POSIX error:
  never retry. `:invalid_descriptor` rejects a value that is not a
  non-negative integer, `:close_failed` reports an unexpected close failure,
  and after `:unsupported` the caller still owns the descriptor.
  """
  @spec close(term()) ::
          :ok | {:error, :close_failed | :invalid_descriptor | :unsupported | term()}
  def close(fd) when is_integer(fd) and fd >= 0 do
    case :os.type() do
      {:unix, _} -> adopt_and_close(fd)
      _ -> {:error, :unsupported}
    end
  end

  def close(_fd), do: {:error, :invalid_descriptor}

  defp adopt_and_close(fd) do
    case :prim_file.file_desc_to_ref(fd, [:raw]) do
      {:ok, file} -> :file.close(file)
      {:error, reason} -> {:error, reason}
    end
  rescue
    UndefinedFunctionError ->
      # OTP no longer ships the private primitive; see the migration note above.
      {:error, :unsupported}

    _exception ->
      {:error, :close_failed}
  catch
    _kind, _reason -> {:error, :close_failed}
  end
end
