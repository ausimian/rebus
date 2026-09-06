defmodule Rebus.Identity.Cached do
  @moduledoc false

  # The default identity source: `Rebus.Identity.Posix` looked up once per VM
  # rather than once per connection. Every connection needs the local uid for
  # EXTERNAL, and a peer offering DBUS_COOKIE_SHA1 costs a second lookup, so
  # without a cache each connection spawns `id` one or two times.
  #
  # Only `{:ok, output}` is cached, and the output is stored exactly as the
  # underlying lookup produced it: the connection still trims and validates it,
  # so the `Rebus.Identity` contract is unchanged. Errors (`:enoent`,
  # `:timeout`, ...) are returned but not cached, so a transient failure is
  # retried by the next connection.
  #
  # Storage is `:persistent_term`, which suits a value written at most twice in
  # a VM's lifetime and read by every connection without copying. Two
  # connections racing the first lookup may both spawn `id` and both write the
  # same result; that is benign, and cheaper than serialising every reader
  # through a process.
  #
  # The trade-off is that a uid or username that changes while the VM runs is
  # not observed. That only happens to a process that changes its credentials
  # in place (setuid), which the BEAM does not do, so caching is accepted.

  alias Rebus.Identity.Posix

  @behaviour Rebus.Identity

  @type lookup :: (pos_integer() -> {:ok, binary()} | {:error, term()})

  @impl Rebus.Identity
  @spec auth_id(pos_integer(), lookup()) :: {:ok, binary()} | {:error, term()}
  def auth_id(timeout, lookup \\ &Posix.auth_id/1)
      when is_integer(timeout) and timeout > 0 and is_function(lookup, 1) do
    cached(:auth_id, timeout, lookup)
  end

  @impl Rebus.Identity
  @spec username(pos_integer(), lookup()) :: {:ok, binary()} | {:error, term()}
  def username(timeout, lookup \\ &Posix.username/1)
      when is_integer(timeout) and timeout > 0 and is_function(lookup, 1) do
    cached(:username, timeout, lookup)
  end

  @doc false
  @spec reset() :: :ok
  def reset do
    :persistent_term.erase(key(:auth_id))
    :persistent_term.erase(key(:username))
    :ok
  end

  defp cached(function, timeout, lookup) do
    key = key(function)

    case :persistent_term.get(key, :miss) do
      :miss -> store(key, lookup.(timeout))
      output -> {:ok, output}
    end
  end

  defp store(key, {:ok, output}) do
    :persistent_term.put(key, output)
    {:ok, output}
  end

  defp store(_key, error), do: error

  defp key(function), do: {__MODULE__, function}
end
