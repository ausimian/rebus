defmodule Rebus.TestImpl do
  @moduledoc false

  # Test doubles for the implementation modules a connection selects through
  # its private `:__impl__` option.
  #
  # Every double delegates to the production module unless an override has been
  # registered for that callback under a key the *calling* process answers to:
  # its own pid, or the name it is registered under. Keying on the caller keeps
  # a stub installed on a live connection separate from a stub used to drive a
  # state value directly from the test process; keying on the registered name
  # lets a test stub a callback the connection performs during setup, before
  # its pid is known, by starting it with a `name:` of the test's choosing.

  @table :rebus_test_impl

  @spec setup!() :: :ok
  def setup! do
    case :ets.whereis(@table) do
      :undefined ->
        _ = :ets.new(@table, [:public, :named_table, :set, read_concurrency: true])
        :ok

      _tid ->
        :ok
    end
  end

  @doc """
  Registers callback overrides under `key`, merging with any already set.

  `key` is a pid or a registered process name; the double consults both for the
  process running the callback. Registrations are dropped when the registering
  test finishes.
  """
  @spec put(pid() | atom(), keyword() | map()) :: :ok
  def put(key \\ self(), overrides) when is_pid(key) or is_atom(key) do
    true = :ets.insert(@table, {key, Map.merge(overrides(key), Map.new(overrides))})
    schedule_cleanup(key)
  end

  defp schedule_cleanup(key) do
    ExUnit.Callbacks.on_exit({__MODULE__, key}, fn -> :ets.delete(@table, key) end)
  rescue
    # Registration from a process ExUnit does not own; the entry outlives the
    # test, which only matters for pid keys and those are never reused while
    # the owning process is alive.
    ArgumentError -> :ok
  end

  @doc """
  Installs the test transport on a running connection, with `overrides`.
  """
  @spec install(pid(), keyword() | map()) :: :ok
  def install(conn, overrides) when is_pid(conn) do
    :ok = put(conn, overrides)

    _ =
      :sys.replace_state(conn, fn state ->
        %{state | impl: %{state.impl | transport: __MODULE__}}
      end)

    :ok
  end

  @doc """
  Returns `state` with the test transport installed and `overrides` registered
  for the calling process.
  """
  @spec stub(struct(), keyword() | map()) :: struct()
  def stub(state, overrides) do
    :ok = put(self(), overrides)
    %{state | impl: %{state.impl | transport: __MODULE__}}
  end

  @doc """
  Builds a `:__impl__` option value, registering `overrides` for the caller.
  """
  @spec impl(keyword() | map(), keyword() | map()) :: map()
  def impl(modules, overrides \\ []) do
    :ok = put(self(), overrides)
    Map.new(modules)
  end

  @doc """
  Registers identity overrides under `key` and returns the stub module.
  """
  @spec identity(pid() | atom(), keyword() | map()) :: module()
  def identity(key \\ self(), overrides) do
    :ok = put(key, overrides)
    __MODULE__.Identity
  end

  @spec overrides(pid() | atom()) :: map()
  def overrides(key) do
    case :ets.lookup(@table, key) do
      [{^key, overrides}] -> overrides
      [] -> %{}
    end
  end

  @spec dispatch(atom(), module(), atom(), [term()]) :: term()
  def dispatch(key, default_module, default_function, args) do
    case fetch_override(key) do
      {:ok, fun} when is_function(fun) -> apply(fun, args)
      :error -> apply(default_module, default_function, args)
    end
  end

  defp fetch_override(key) do
    with :error <- Map.fetch(overrides(self()), key) do
      case Process.info(self(), :registered_name) do
        {:registered_name, name} when is_atom(name) -> Map.fetch(overrides(name), key)
        _unregistered -> :error
      end
    end
  end

  @behaviour Rebus.Transport

  @impl Rebus.Transport
  def open(domain, type, protocol),
    do: dispatch(:open, Rebus.Transport.Socket, :open, [domain, type, protocol])

  @impl Rebus.Transport
  def connect(socket, address, timeout),
    do: dispatch(:transport_connect, Rebus.Transport.Socket, :connect, [socket, address, timeout])

  @impl Rebus.Transport
  def send(socket, data, flags, timeout),
    do: dispatch(:send, Rebus.Transport.Socket, :send, [socket, data, flags, timeout])

  @impl Rebus.Transport
  def sendmsg(socket, message, flags, timeout),
    do: dispatch(:sendmsg, Rebus.Transport.Socket, :sendmsg, [socket, message, flags, timeout])

  @impl Rebus.Transport
  def recv(socket, length, flags, timeout),
    do: dispatch(:recv, Rebus.Transport.Socket, :recv, [socket, length, flags, timeout])

  @impl Rebus.Transport
  def recvmsg(socket, length, control_size, flags, timeout),
    do:
      dispatch(:recvmsg, Rebus.Transport.Socket, :recvmsg, [
        socket,
        length,
        control_size,
        flags,
        timeout
      ])

  @impl Rebus.Transport
  def cancel(socket, select_info),
    do: dispatch(:cancel, Rebus.Transport.Socket, :cancel, [socket, select_info])

  @impl Rebus.Transport
  def setopt(socket, option, value),
    do: dispatch(:setopt, Rebus.Transport.Socket, :setopt, [socket, option, value])

  @impl Rebus.Transport
  def close(socket), do: dispatch(:close, Rebus.Transport.Socket, :close, [socket])
end

defmodule Rebus.TestImpl.Identity do
  @moduledoc false

  @behaviour Rebus.Identity

  @impl Rebus.Identity
  def auth_id(timeout),
    do: Rebus.TestImpl.dispatch(:auth_id, Rebus.Identity.Posix, :auth_id, [timeout])

  @impl Rebus.Identity
  def username(timeout),
    do: Rebus.TestImpl.dispatch(:username, Rebus.Identity.Posix, :username, [timeout])
end
