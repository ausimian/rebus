defmodule Rebus.Identity.Posix do
  @moduledoc false

  # The production identity source: `id -u` and `id -un` run through a port
  # under a bounded deadline. The executable lookup and the port opener remain
  # injectable as optional arguments so this module's own failure paths stay
  # covered without any test seam in the connection.

  @behaviour Rebus.Identity

  @max_auth_id_output 64

  @type executable_finder :: (String.t() -> String.t() | nil)
  @type port_opener :: ({:spawn_executable, charlist()}, keyword() -> port())

  @impl Rebus.Identity
  @spec auth_id(pos_integer(), executable_finder(), port_opener()) ::
          {:ok, binary()} | {:error, term()}
  def auth_id(
        timeout,
        executable_finder \\ &System.find_executable/1,
        port_opener \\ &Port.open/2
      )
      when is_integer(timeout) and timeout > 0 and is_function(executable_finder, 1) and
             is_function(port_opener, 2) do
    run(["-u"], timeout, executable_finder, port_opener)
  end

  @impl Rebus.Identity
  @spec username(pos_integer(), executable_finder(), port_opener()) ::
          {:ok, binary()} | {:error, term()}
  def username(
        timeout,
        executable_finder \\ &System.find_executable/1,
        port_opener \\ &Port.open/2
      )
      when is_integer(timeout) and timeout > 0 and is_function(executable_finder, 1) and
             is_function(port_opener, 2) do
    run(["-un"], timeout, executable_finder, port_opener)
  end

  defp run(args, timeout, executable_finder, port_opener) do
    case safely_find_executable(executable_finder) do
      nil -> {:error, :enoent}
      executable -> safely_open_port(executable, args, port_opener, timeout)
    end
  end

  defp safely_find_executable(executable_finder) do
    executable_finder.("id")
  rescue
    _exception -> nil
  catch
    _kind, _reason -> nil
  end

  defp safely_open_port(executable, args, port_opener, timeout) do
    port =
      port_opener.({:spawn_executable, String.to_charlist(executable)}, [
        :binary,
        :exit_status,
        args: args
      ])

    collect_output(port, <<>>, read_deadline(timeout), timeout)
  rescue
    _exception -> {:error, :port_open_failed}
  catch
    _kind, _reason -> {:error, :port_open_failed}
  end

  defp collect_output(port, output, deadline, maximum) do
    case remaining_timeout(deadline, maximum) do
      :expired ->
        safe_close_port(port)
        {:error, :timeout}

      {:ok, timeout} ->
        receive do
          {^port, {:data, data}}
          when is_binary(data) and byte_size(output) + byte_size(data) <= @max_auth_id_output ->
            collect_output(port, output <> data, deadline, maximum)

          {^port, {:data, _data}} ->
            safe_close_port(port)
            {:error, :output_too_large}

          {^port, {:exit_status, 0}} ->
            {:ok, output}

          {^port, {:exit_status, _status}} ->
            {:error, :exit_status}

          {:EXIT, ^port, _reason} ->
            {:error, :port_exit}
        after
          timeout ->
            safe_close_port(port)
            {:error, :timeout}
        end
    end
  end

  defp safe_close_port(port) do
    Port.close(port)
  catch
    _kind, _reason -> :ok
  end

  defp read_deadline(timeout), do: System.monotonic_time(:millisecond) + timeout

  defp remaining_timeout(deadline, maximum) do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, min(remaining, maximum)}
      _expired -> :expired
    end
  end
end
