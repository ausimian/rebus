defmodule Rebus.Bus do
  @moduledoc """
  Requests and releases well-known bus names, such as `"org.example.Calculator"`.

  A connection to a message bus is known by its unique name, such as
  `":1.42"`, for its whole life. A service also asks the bus for a well-known
  name, which clients address it by. `request_name/4` asks for one and
  `release_name/3` gives it back. Both are calls to the bus driver,
  `org.freedesktop.DBus`, with the driver's numeric answers turned into atoms.

  ## Example

      {:ok, conn} = Rebus.connect(:session)
      {:ok, _ref} = Rebus.Service.export(conn, "/org/example/Calculator", [calculator])

      {:ok, :primary_owner} = Rebus.Bus.request_name(conn, "org.example.Calculator")

      receive do
        {:rebus_name, ^conn, :acquired, "org.example.Calculator"} -> :ok
      end

  ## Export before requesting

  Export a service's objects before requesting its name. The bus announces a
  new owner with `NameOwnerChanged` as soon as the name is granted, and a
  client waiting for that, or one the bus activated, may call at once. A
  connection that exports nothing at the path answers the call with
  `org.freedesktop.DBus.Error.UnknownObject`.

  ## Ownership notices

  The bus tells the owning connection when it gains or loses a name, with the
  `NameAcquired` and `NameLost` signals. `request_name/4` has them delivered
  to its `:owner`, the calling process by default, as

      {:rebus_name, conn, :acquired | :lost, name}

  A name can change hands at any time after it is requested: a connection
  that asks with `:replace_existing` takes it from an owner that allowed
  replacement, and a connection waiting in the queue gets it when the owner
  releases it or disconnects. Each change arrives as a notice, so an owner
  learns that it lost a name, and a queued connection that it gained one.

  The notices for a name continue until `release_name/3` releases it, the
  owner exits, or the connection closes. A first request answered `:exists`
  leaves none behind, and requesting a name again for the same owner does not
  repeat its notices.

  Make one request or release of a name at a time on a connection. When they
  overlap, an earlier one's cleanup can stop the notices a later request set
  up.
  """

  alias Rebus.Connection
  alias Rebus.Message
  alias Rebus.UnixFD
  alias Rebus.WireValue

  @default_timeout 5_000

  @driver "org.freedesktop.DBus"
  @driver_path "/org/freedesktop/DBus"

  @flags %{allow_replacement: 0x1, replace_existing: 0x2, do_not_queue: 0x4}
  @request_results %{1 => :primary_owner, 2 => :in_queue, 3 => :exists, 4 => :already_owner}
  @release_results %{1 => :released, 2 => :non_existent, 3 => :not_owner}

  @typedoc """
  A flag for `request_name/4`.

  - `:allow_replacement` - let a later request with `:replace_existing` take
    the name from this connection.
  - `:replace_existing` - take the name from its current owner, if that owner
    allowed replacement.
  - `:do_not_queue` - answer `:exists` rather than waiting in the queue for a
    name that cannot be taken now.
  """
  @type flag :: :allow_replacement | :replace_existing | :do_not_queue

  @typedoc """
  The bus's answer to `request_name/4`.

  - `:primary_owner` - the connection now owns the name.
  - `:in_queue` - the name is owned elsewhere and the connection waits in its
    queue. It becomes the owner, and its owner receives an `:acquired`
    notice, when the name comes free.
  - `:exists` - the name is owned elsewhere and the connection did not queue
    for it.
  - `:already_owner` - the connection already owned the name.
  """
  @type request_result :: :primary_owner | :in_queue | :exists | :already_owner

  @typedoc """
  The bus's answer to `release_name/3`.

  - `:released` - the connection owned the name, or waited in its queue, and
    no longer does.
  - `:non_existent` - nobody owned the name.
  - `:not_owner` - the name is owned by another connection, and this one was
    not in its queue.
  """
  @type release_result :: :released | :non_existent | :not_owner

  @typedoc """
  The message an owner receives when its connection gains or loses a name.
  """
  @type name_notice :: {:rebus_name, pid(), :acquired | :lost, binary()}

  @typedoc """
  Failure reasons returned by `request_name/4` and `release_name/3`.

  - `:invalid_name`, `:invalid_flags`, `:invalid_owner` and
    `:invalid_timeout` - an invalid argument. Nothing was sent.
  - `:not_a_bus` - the connection was opened with `bus: false` and has no
    bus to ask. Nothing was sent.
  - `{:bus_error, error_name}` - the bus refused the request, for example
    with `org.freedesktop.DBus.Error.AccessDenied` where its policy does not
    let this connection own the name.
  - `:invalid_bus_reply` - the bus answered with something other than one of
    the documented values.
  - Any other reason is a failure of the call to the bus, as for
    `Rebus.call/3`. `:timeout` is delivery-ambiguous: the bus may have
    applied the request.
  """
  @type error_reason ::
          :invalid_name
          | :invalid_flags
          | :invalid_owner
          | :invalid_timeout
          | :not_a_bus
          | :invalid_bus_reply
          | {:bus_error, binary()}
          | Rebus.error_reason()

  @doc """
  Asks the bus for the well-known name `name`.

  `flags` is a list of `t:flag/0` values and defaults to `[]`, which asks for
  the name, queues for it if another connection owns it, and lets nobody take
  it away.

  ## Options

  - `:owner` - the local process that receives the name's
    `t:name_notice/0` messages, or `nil` for none. Defaults to the caller.
  - `:timeout` - milliseconds for the whole request. Defaults to `5000`.

  ## Return values

  - `{:ok, result}` - the bus's answer, a `t:request_result/0`.
  - `{:error, reason}` - see `t:error_reason/0`.

  ## Examples

      {:ok, :primary_owner} = Rebus.Bus.request_name(conn, "org.example.Calculator")

      {:ok, :exists} =
        Rebus.Bus.request_name(other_conn, "org.example.Calculator", [:do_not_queue])
  """
  @spec request_name(pid(), binary(), [flag()], keyword()) ::
          {:ok, request_result()} | {:error, error_reason()}
  def request_name(conn, name, flags \\ [], opts \\ []) when is_pid(conn) and is_list(opts) do
    with :ok <- validate_name(name),
         {:ok, bits} <- encode_flags(flags),
         {:ok, owner} <- validate_owner(Keyword.get(opts, :owner, self())),
         {:ok, deadline} <- deadline(opts),
         :ok <- ensure_bus(conn, deadline),
         {:ok, watch} <- watch(conn, owner, name, deadline) do
      conn
      |> driver_call("RequestName", "su", [name, bits], deadline)
      |> decode_reply(@request_results)
      |> settle_watch(conn, owner, name, watch)
    end
  end

  @doc """
  Releases the well-known name `name`, or leaves its queue.

  The name's ownership notices end once the bus has answered, for every owner
  that `request_name/4` gave the connection. A `:lost` notice the bus sent
  before its answer still arrives.

  ## Options

  - `:timeout` - milliseconds for the whole request. Defaults to `5000`.

  ## Return values

  - `{:ok, result}` - the bus's answer, a `t:release_result/0`.
  - `{:error, reason}` - see `t:error_reason/0`. The notices continue.

  ## Examples

      {:ok, :released} = Rebus.Bus.release_name(conn, "org.example.Calculator")
  """
  @spec release_name(pid(), binary(), keyword()) ::
          {:ok, release_result()} | {:error, error_reason()}
  def release_name(conn, name, opts \\ []) when is_pid(conn) and is_list(opts) do
    with :ok <- validate_name(name),
         {:ok, deadline} <- deadline(opts),
         :ok <- ensure_bus(conn, deadline) do
      case conn
           |> driver_call("ReleaseName", "s", [name], deadline)
           |> decode_reply(@release_results) do
        {:ok, _result} = released ->
          Connection.unwatch_name(conn, name, :all)
          released

        {:error, _reason} = error ->
          error
      end
    end
  end

  defp validate_name(name) do
    if WireValue.valid_well_known_name?(name), do: :ok, else: {:error, :invalid_name}
  end

  defp encode_flags(flags) when is_list(flags) do
    Enum.reduce_while(flags, {:ok, 0}, fn flag, {:ok, bits} ->
      case Map.fetch(@flags, flag) do
        {:ok, bit} -> {:cont, {:ok, Bitwise.bor(bits, bit)}}
        :error -> {:halt, {:error, :invalid_flags}}
      end
    end)
  end

  defp encode_flags(_flags), do: {:error, :invalid_flags}

  defp validate_owner(nil), do: {:ok, nil}
  defp validate_owner(owner) when is_pid(owner) and node(owner) == node(), do: {:ok, owner}
  defp validate_owner(_owner), do: {:error, :invalid_owner}

  defp deadline(opts) do
    case Keyword.get(opts, :timeout, @default_timeout) do
      timeout when is_integer(timeout) and timeout >= 0 ->
        {:ok, System.monotonic_time(:millisecond) + timeout}

      _timeout ->
        {:error, :invalid_timeout}
    end
  end

  defp remaining(deadline), do: max(deadline - System.monotonic_time(:millisecond), 0)

  defp ensure_bus(conn, deadline) do
    if node(conn) == node() do
      case Connection.bus?(conn, remaining(deadline)) do
        true -> :ok
        false -> {:error, :not_a_bus}
        {:error, _reason} = error -> error
      end
    else
      {:error, :remote_connection_unsupported}
    end
  end

  # The watch goes in before RequestName is sent, because the bus sends
  # NameAcquired ahead of its reply.
  defp watch(_conn, nil, _name, _deadline), do: {:ok, nil}

  defp watch(conn, owner, name, deadline),
    do: Connection.watch_name(conn, owner, name, remaining(deadline))

  defp driver_call(conn, member, signature, body, deadline) do
    message =
      Message.new!(:method_call,
        destination: @driver,
        path: @driver_path,
        interface: @driver,
        member: member,
        signature: signature,
        body: body
      )

    Rebus.call(conn, message, remaining(deadline))
  end

  # The reply itself is not returned, so any descriptors it carried are
  # closed here, and an error name is copied out of the frame rather than
  # left pinning it.
  defp decode_reply({:ok, %Message{} = reply}, results) do
    _ = UnixFD.close_all(reply.unix_fds)

    with "u" <- Message.signature(reply),
         [code] <- reply.body,
         {:ok, result} <- Map.fetch(results, code) do
      {:ok, result}
    else
      _invalid -> {:error, :invalid_bus_reply}
    end
  end

  defp decode_reply({:error, {:dbus_error, %Message{} = error}}, _results) do
    _ = UnixFD.close_all(error.unix_fds)
    {:error, {:bus_error, :binary.copy(error.header_fields.error_name)}}
  end

  defp decode_reply({:error, {:reply_dropped, {:error, error_name}}}, _results),
    do: {:error, {:bus_error, :binary.copy(error_name)}}

  defp decode_reply({:error, _reason} = error, _results), do: error

  # A watch this request added comes out again when the bus certainly did not
  # queue the connection for the name. One the bus may have applied stays, as
  # does one an earlier request installed, since a missed `:acquired` would be
  # worse than a watch that never fires.
  defp settle_watch({:ok, :exists} = result, conn, owner, name, :added) do
    Connection.unwatch_name(conn, name, owner)
    result
  end

  defp settle_watch({:error, reason} = error, conn, owner, name, :added) do
    unless ambiguous?(reason), do: Connection.unwatch_name(conn, name, owner)
    error
  end

  defp settle_watch(result, _conn, _owner, _name, _watch), do: result

  defp ambiguous?(:timeout), do: true
  defp ambiguous?(:disconnected), do: true
  defp ambiguous?(:invalid_bus_reply), do: true
  defp ambiguous?(:fd_claim_expired), do: true
  defp ambiguous?({:reply_dropped, :method_return}), do: true
  defp ambiguous?(_reason), do: false
end
