defmodule Rebus.Identity.Posix do
  @moduledoc false

  # The production identity source. The EXTERNAL uid is the process's
  # effective uid, which is what libdbus and sd-bus send and what `id -u`
  # prints. On Linux it is read from `/proc/self/status` without spawning
  # anything; elsewhere, or when that file is missing or malformed, `id -u`
  # runs through a port. The DBUS_COOKIE_SHA1 username always comes from
  # `id -un`, because only a libc lookup such as `id`'s sees every user
  # database (NSS, LDAP, Directory Services), not just `/etc/passwd`.
  #
  # Trust: `id` is taken from /usr/bin or /bin when either holds it. Those are
  # root-owned system directories, so that `id` belongs to the OS rather than
  # to whatever the environment selects. PATH is consulted only when neither
  # exists, as on NixOS and Guix, and a relative result (from a `.` or empty
  # PATH entry) is refused. That last resort keeps those systems working and
  # is safe: whoever controls this VM's PATH can already run code as this
  # user, and neither value is a credential the bus trusts. The bus checks
  # the EXTERNAL uid against the socket's peer credentials, and
  # DBUS_COOKIE_SHA1 succeeds only for a client that can read the named
  # user's private keyring, so a wrong answer can only make authentication
  # fail.
  #
  # Each lookup runs under one deadline and every result is bounded. The
  # status reader, the trusted-path probe, the PATH lookup and the port opener
  # are injectable options so this module's own failure paths stay covered
  # without any test seam in the connection.

  @behaviour Rebus.Identity

  import Bitwise, only: [band: 2]

  @max_auth_id_output 64

  # `Uid:` is the ninth line of `/proc/self/status`, well inside this prefix.
  # The `Groups:` line after it grows with the process's supplementary
  # groups, so the rest of the file is never read.
  @proc_status "/proc/self/status"
  @max_status_prefix 4096

  @trusted_id_paths ["/usr/bin/id", "/bin/id"]

  @max_uid 4_294_967_295

  @type status_reader :: (-> {:ok, binary()} | {:error, term()})
  @type executable_probe :: (String.t() -> boolean())
  @type executable_finder :: (String.t() -> String.t() | nil)
  @type port_opener :: ({:spawn_executable, charlist()}, keyword() -> port())
  @type option ::
          {:read_status, status_reader()}
          | {:executable?, executable_probe()}
          | {:find_executable, executable_finder()}
          | {:open_port, port_opener()}

  @impl Rebus.Identity
  @spec auth_id(pos_integer(), [option()]) :: {:ok, binary()} | {:error, term()}
  def auth_id(timeout, options \\ [])
      when is_integer(timeout) and timeout > 0 and is_list(options) do
    deadline = read_deadline(timeout)
    options = options(options)

    with {:ok, uid} <- proc_uid(options.read_status),
         {:ok, _remaining} <- remaining_timeout(deadline, timeout) do
      {:ok, uid}
    else
      :expired -> {:error, :timeout}
      :error -> run(["-u"], deadline, timeout, options)
    end
  end

  @impl Rebus.Identity
  @spec username(pos_integer(), [option()]) :: {:ok, binary()} | {:error, term()}
  def username(timeout, options \\ [])
      when is_integer(timeout) and timeout > 0 and is_list(options) do
    run(["-un"], read_deadline(timeout), timeout, options(options))
  end

  defp options(options) do
    options
    |> Keyword.validate!(
      read_status: &read_proc_status/0,
      executable?: &executable_file?/1,
      find_executable: &System.find_executable/1,
      open_port: &Port.open/2
    )
    |> Map.new()
  end

  defp proc_uid(read_status) do
    case safely_read_status(read_status) do
      {:ok, status} when is_binary(status) and byte_size(status) <= @max_status_prefix ->
        status_uid(status)

      _other ->
        :error
    end
  end

  defp safely_read_status(read_status) do
    read_status.()
  rescue
    _exception -> :error
  catch
    _kind, _reason -> :error
  end

  # Only complete lines count, since the prefix may end mid-line. Exactly one
  # `Uid:` line is accepted, carrying the four decimal uids the kernel writes
  # (real, effective, saved, filesystem). Anything else falls back to `id -u`.
  defp status_uid(status) do
    lines = status |> :binary.split("\n", [:global]) |> Enum.drop(-1)

    case for("Uid:" <> fields <- lines, do: fields) do
      [fields] -> effective_uid(String.split(fields, ["\t", " "], trim: true))
      _none_or_many -> :error
    end
  end

  defp effective_uid([_real, effective, _saved, _filesystem] = fields) do
    if Enum.all?(fields, &uid?/1),
      do: {:ok, effective |> String.to_integer() |> Integer.to_string()},
      else: :error
  end

  defp effective_uid(_fields), do: :error

  defp uid?(field) when byte_size(field) in 1..10,
    do: String.match?(field, ~r/\A[0-9]+\z/) and String.to_integer(field) <= @max_uid

  defp uid?(_field), do: false

  defp read_proc_status do
    case :os.type() do
      {:unix, :linux} -> read_prefix(@proc_status, @max_status_prefix)
      _other -> {:error, :unsupported}
    end
  end

  defp read_prefix(path, limit) do
    with {:ok, io} <- :file.open(path, [:read, :binary, :raw]) do
      read_and_close(io, limit)
    end
  end

  defp read_and_close(io, limit) do
    read_chunks(io, limit, [])
  after
    :file.close(io)
  end

  defp read_chunks(_io, 0, acc), do: {:ok, acc |> Enum.reverse() |> IO.iodata_to_binary()}

  defp read_chunks(io, remaining, acc) do
    case :file.read(io, remaining) do
      {:ok, data} -> read_chunks(io, remaining - byte_size(data), [data | acc])
      :eof -> read_chunks(io, 0, acc)
      {:error, reason} -> {:error, reason}
    end
  end

  defp run(args, deadline, timeout, options) do
    case find_id(options) do
      nil ->
        {:error, :enoent}

      executable ->
        case remaining_timeout(deadline, timeout) do
          :expired -> {:error, :timeout}
          {:ok, _remaining} -> safely_open_port(executable, args, options, deadline, timeout)
        end
    end
  end

  defp find_id(%{executable?: executable?, find_executable: find_executable}) do
    Enum.find(@trusted_id_paths, &safely_probe(executable?, &1)) ||
      path_id(find_executable)
  end

  defp safely_probe(executable?, path) do
    executable?.(path) == true
  rescue
    _exception -> false
  catch
    _kind, _reason -> false
  end

  defp executable_file?(path) do
    case File.stat(path) do
      {:ok, %File.Stat{type: :regular, mode: mode}} -> band(mode, 0o111) != 0
      _other -> false
    end
  end

  defp path_id(find_executable) do
    case safely_find_executable(find_executable) do
      path when is_binary(path) -> if Path.type(path) == :absolute, do: path
      _other -> nil
    end
  end

  defp safely_find_executable(find_executable) do
    find_executable.("id")
  rescue
    _exception -> nil
  catch
    _kind, _reason -> nil
  end

  defp safely_open_port(executable, args, %{open_port: open_port}, deadline, timeout) do
    port =
      open_port.({:spawn_executable, String.to_charlist(executable)}, [
        :binary,
        :exit_status,
        args: args
      ])

    collect_output(port, <<>>, deadline, timeout)
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
