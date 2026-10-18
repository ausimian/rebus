defmodule Rebus.Object.Server do
  @moduledoc false

  # The GenServer behind `Rebus.Object.start_link/3`: it exports the object
  # with itself as the handler, then runs the object module's callbacks.

  use GenServer

  require Logger

  alias Rebus.Service
  alias Rebus.Service.Call

  defstruct [:module, :conn, :export, :conn_monitor, :interfaces, :state]

  @impl GenServer
  def init({module, arg, conn, path, interfaces, export_opts}) do
    interfaces = interfaces || module.interfaces()

    # `{:error, reason}` rather than `{:stop, reason}`, so a failed export is
    # returned from start_link without an exit signal to the caller.
    case Service.export(conn, path, interfaces, [handler: self()] ++ export_opts) do
      {:ok, export} ->
        server = %__MODULE__{
          module: module,
          conn: conn,
          export: export,
          conn_monitor: Process.monitor(conn),
          interfaces: interfaces
        }

        init_object(server, arg)

      {:error, reason} ->
        {:error, reason}
    end
  end

  defp init_object(%__MODULE__{} = server, arg) do
    case run_init(server, arg) do
      {:ok, state} ->
        {:ok, %{server | state: state}}

      {:stop, reason} ->
        withdraw(server)
        {:stop, reason}

      :ignore ->
        withdraw(server)
        :ignore

      other ->
        withdraw(server)
        {:stop, {:bad_return_value, other}}
    end
  end

  # A process that fails in init is never terminated, so the export is
  # withdrawn here before the failure propagates, for the same reason
  # terminate/2 withdraws it.
  defp run_init(%__MODULE__{module: module} = server, arg) do
    module.init(arg)
  catch
    kind, reason ->
      withdraw(server)
      :erlang.raise(kind, reason, __STACKTRACE__)
  end

  @impl GenServer
  def handle_info({export, %Call{}} = call, %__MODULE__{export: export} = server) do
    call
    |> Rebus.Object.dispatch(server.module, server.state, interfaces: server.interfaces)
    |> continue(server)
  end

  def handle_info(
        {:DOWN, monitor, :process, _pid, _reason},
        %__MODULE__{conn_monitor: monitor} = server
      ),
      do: {:stop, {:shutdown, :disconnected}, server}

  def handle_info(msg, %__MODULE__{module: module} = server) do
    if function_exported?(module, :handle_info, 2) do
      case module.handle_info(msg, server.state) do
        {:noreply, _state} = result -> continue(result, server)
        {:stop, _reason, _state} = result -> continue(result, server)
        other -> {:stop, {:bad_return_value, other}, server}
      end
    else
      Logger.error(
        "#{inspect(module)} #{inspect(self())} received unexpected message in handle_info/2: " <>
          inspect(msg)
      )

      {:noreply, server}
    end
  end

  # The export would go with the process anyway, but withdrawing it here
  # frees the path before a supervisor starts the next process, which would
  # otherwise race the connection's handling of this one's exit. It comes
  # first, so calls are refused at once rather than queued behind the
  # module's own teardown.
  @impl GenServer
  def terminate(reason, %__MODULE__{module: module} = server) do
    withdraw(server)
    if function_exported?(module, :terminate, 2), do: module.terminate(reason, server.state)
  end

  defp continue({:noreply, state}, server), do: {:noreply, %{server | state: state}}
  defp continue({:stop, reason, state}, server), do: {:stop, reason, %{server | state: state}}

  defp withdraw(%__MODULE__{conn: conn, export: export}), do: Service.unexport(conn, export)
end
