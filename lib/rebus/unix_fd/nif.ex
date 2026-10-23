defmodule Rebus.UnixFD.NIF do
  # Native functions behind Rebus.UnixFD, from c_src/rebus_unix_fd.cpp. The
  # library is built only on Unix, so elsewhere nothing is loaded and
  # Rebus.UnixFD answers {:error, :unsupported} before calling in here.
  @moduledoc false

  @on_load :load

  @doc false
  def load do
    case :os.type() do
      {:unix, _} ->
        path = :filename.join(:code.priv_dir(:rebus), ~c"rebus_unix_fd")
        :erlang.load_nif(path, 0)

      _ ->
        :ok
    end
  end

  @spec wrap(integer(), pid()) :: {:ok, reference()} | {:error, atom()}
  def wrap(_fd, _owner), do: :erlang.nif_error(:not_loaded)

  @spec close_handle(reference()) :: :ok | {:error, :closed | :not_local}
  def close_handle(_ref), do: :erlang.nif_error(:not_loaded)

  @spec controlling_process(reference(), pid()) ::
          :ok | {:error, :closed | :noproc | :not_local | :not_owner}
  def controlling_process(_ref, _pid), do: :erlang.nif_error(:not_loaded)

  @spec owner(reference()) :: {:ok, pid()} | {:error, :closed | :not_local}
  def owner(_ref), do: :erlang.nif_error(:not_loaded)

  @spec pin(reference()) ::
          {:ok, reference(), non_neg_integer()} | {:error, :closed | :not_local}
  def pin(_ref), do: :erlang.nif_error(:not_loaded)

  @spec unpin(reference()) :: :ok
  def unpin(_token), do: :erlang.nif_error(:not_loaded)

  @spec dup_handle(reference()) :: {:ok, reference()} | {:error, atom()}
  def dup_handle(_ref), do: :erlang.nif_error(:not_loaded)

  @spec live_count() :: non_neg_integer()
  def live_count, do: :erlang.nif_error(:not_loaded)

  @spec close_count(reference()) :: {:ok, non_neg_integer()} | {:error, :not_local}
  def close_count(_ref), do: :erlang.nif_error(:not_loaded)

  @spec pipe_pair() :: {:ok, reference(), reference()} | {:error, atom()}
  def pipe_pair, do: :erlang.nif_error(:not_loaded)

  @spec fd_identity(integer()) ::
          {:ok, {non_neg_integer(), non_neg_integer()}} | {:error, atom()}
  def fd_identity(_fd), do: :erlang.nif_error(:not_loaded)
end
