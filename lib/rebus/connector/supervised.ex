defmodule Rebus.Connector.Supervised do
  @moduledoc false

  # The production connector: start the connection under Rebus's dynamic
  # supervisor and wait for it to accept or fail its setup.

  @behaviour Rebus.Connector

  @impl Rebus.Connector
  def connect(%{family: family} = addr, {opts, internal})
      when family in [:inet, :inet6, :local] and is_list(opts) and is_map(internal) do
    connect_ref = make_ref()

    internal =
      internal
      |> Map.put(:addr, addr)
      |> Map.put(:connect_waiter, {self(), connect_ref})

    child_spec = {Rebus.Connection, {opts, internal}}

    case DynamicSupervisor.start_child(Rebus.ConnectionSupervisor, child_spec) do
      {:ok, pid} -> await_connection(pid, connect_ref, Process.monitor(pid))
      {:error, {:already_started, pid}} -> name_collision(pid)
      other -> other
    end
  end

  defp name_collision(pid) do
    if connection_child?(pid),
      do: {:error, {:name_taken, pid}},
      else: {:error, {:name_registered, pid}}
  end

  defp connection_child?(pid) do
    Enum.any?(DynamicSupervisor.which_children(Rebus.ConnectionSupervisor), fn
      {_id, ^pid, _type, _modules} -> true
      _child -> false
    end)
  catch
    :exit, _reason -> false
  end

  defp await_connection(pid, connect_ref, monitor_ref) do
    receive do
      {^connect_ref, {:ok, ^pid}} ->
        Kernel.send(pid, {connect_ref, :accepted})
        await_accepted_connection(pid, connect_ref, monitor_ref)

      {^connect_ref, {:error, reason}} ->
        await_failed_connection(pid, monitor_ref, reason)

      {:DOWN, ^monitor_ref, :process, ^pid, {:shutdown, reason}} ->
        {:error, reason}

      {:DOWN, ^monitor_ref, :process, ^pid, reason} ->
        {:error, reason}
    end
  end

  defp await_failed_connection(pid, monitor_ref, reason) do
    receive do
      {:DOWN, ^monitor_ref, :process, ^pid, _stop_reason} -> {:error, reason}
    end
  end

  defp await_accepted_connection(pid, connect_ref, monitor_ref) do
    receive do
      {^connect_ref, :accepted} ->
        Process.demonitor(monitor_ref, [:flush])
        {:ok, pid}

      {:DOWN, ^monitor_ref, :process, ^pid, {:shutdown, reason}} ->
        {:error, reason}

      {:DOWN, ^monitor_ref, :process, ^pid, reason} ->
        {:error, reason}
    end
  end
end
