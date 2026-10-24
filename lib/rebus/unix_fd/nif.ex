defmodule Rebus.UnixFD.NIF do
  # Native functions behind Rebus.UnixFD, from c_src/rebus_unix_fd.cpp. The
  # library is built only on Linux and macOS, the platforms where Rebus passes
  # descriptors, and only when Unix fd support is enabled,
  # so otherwise nothing is loaded and Rebus.UnixFD answers
  # {:error, :unsupported} before calling in here.
  @moduledoc false

  @on_load :load

  # Set by the application with `config :rebus, unix_fds: true`. mix.exs reads
  # the same key to decide whether to build the library, and compile_env/3
  # makes Mix recompile Rebus when it changes.
  @enabled Application.compile_env(:rebus, :unix_fds, false)

  unless is_boolean(@enabled) do
    raise ArgumentError,
          "expected config :rebus, :unix_fds to be a boolean, got: #{inspect(@enabled)}"
  end

  @doc false
  # Loading only when enabled keeps a build without the library loadable,
  # which matters to a release that loads every module at boot. When enabled,
  # a library that fails to load fails this module, and the boot with it,
  # rather than leaving an application that asked for fd passing without it.
  def load do
    if enabled?() do
      path = :filename.join(:code.priv_dir(:rebus), ~c"rebus_unix_fd")
      :erlang.load_nif(path, 0)
    else
      :ok
    end
  end

  @doc false
  @spec enabled?() :: boolean()
  def enabled?, do: @enabled and :os.type() in [{:unix, :linux}, {:unix, :darwin}]

  @spec wrap(integer(), pid()) :: {:ok, reference()} | {:error, atom()}
  def wrap(_fd, _owner), do: :erlang.nif_error(:not_loaded)

  @spec close_handle(reference()) :: :ok | {:error, :closed | :not_local}
  def close_handle(_ref), do: :erlang.nif_error(:not_loaded)

  @spec close_handle_async(reference()) :: :ok | {:error, :closed | :not_local}
  def close_handle_async(_ref), do: :erlang.nif_error(:not_loaded)

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

  @spec drain() :: :ok
  def drain, do: :erlang.nif_error(:not_loaded)

  @spec scheduler_close_count() :: non_neg_integer()
  def scheduler_close_count, do: :erlang.nif_error(:not_loaded)

  @spec close_count(reference()) :: {:ok, non_neg_integer()} | {:error, :not_local}
  def close_count(_ref), do: :erlang.nif_error(:not_loaded)

  @spec pipe_pair() :: {:ok, reference(), reference()} | {:error, atom()}
  def pipe_pair, do: :erlang.nif_error(:not_loaded)

  @spec fd_identity(integer()) ::
          {:ok, {non_neg_integer(), non_neg_integer()}} | {:error, atom()}
  def fd_identity(_fd), do: :erlang.nif_error(:not_loaded)
end
