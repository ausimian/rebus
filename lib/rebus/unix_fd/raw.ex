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
# Migration: if OTP gains a public way to close a raw descriptor, change
# `close/1` below to use it and delete the compatibility test.  Otherwise the
# fallback is a minimal NIF built with `elixir_make`; that adds a C toolchain
# to the build and to the hex package, so it is a build-system decision to be
# taken on its own, not a change to be made in passing here.
defmodule Rebus.UnixFD.Raw do
  @moduledoc false

  @doc """
  Adopts a raw descriptor and closes it, reporting the outcome.
  """
  @spec close(non_neg_integer()) :: :ok | {:error, term()}
  def close(fd) when is_integer(fd) and fd >= 0 do
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
