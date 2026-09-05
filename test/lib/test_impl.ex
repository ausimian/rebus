defmodule Rebus.TestImpl do
  @moduledoc false

  # Test doubles for the implementation modules a connection selects through
  # its private `:__impl__` option.
  #
  # Every double delegates to the production module unless the *calling*
  # process has registered an override for that callback. Keying on the caller
  # keeps a stub installed on a live connection (overrides registered under the
  # connection pid) separate from a stub used to drive a state value directly
  # from the test process.

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
  Registers callback overrides for a process, merging with any already set.
  """
  @spec put(pid(), keyword() | map()) :: :ok
  def put(pid \\ self(), overrides) when is_pid(pid) do
    true = :ets.insert(@table, {pid, Map.merge(overrides(pid), Map.new(overrides))})
    :ok
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

  @spec overrides(pid()) :: map()
  def overrides(pid) do
    case :ets.lookup(@table, pid) do
      [{^pid, overrides}] -> overrides
      [] -> %{}
    end
  end

  @spec dispatch(atom(), module(), atom(), [term()]) :: term()
  def dispatch(key, default_module, default_function, args) do
    case Map.fetch(overrides(self()), key) do
      {:ok, fun} when is_function(fun) -> apply(fun, args)
      :error -> apply(default_module, default_function, args)
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
