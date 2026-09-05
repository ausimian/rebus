defmodule Rebus.Connection.Handshake do
  @moduledoc false

  # The SASL-style line protocol a D-Bus connection performs on a freshly
  # connected socket, from the initial NUL byte through to BEGIN. It owns no
  # process state: everything it needs arrives in `Options`, every byte moves
  # through the transport module it is given, and the leftover bytes read past
  # BEGIN are handed back to the caller as the start of the message stream.

  alias Rebus.Auth
  alias Rebus.Connection.SocketError

  @max_auth_line_size 1_024
  @max_auth_id_output 64

  defmodule Options do
    @moduledoc false

    use TypedStruct

    # The connection settings the handshake reads. `write_timeout` bounds a
    # single send in addition to the shared setup deadline; the remaining
    # fields select mechanisms and the optional unix-FD extension.

    typedstruct enforce: true do
      field :transport, module()
      field :identity, module()
      field :write_timeout, pos_integer()
      field :allow_anonymous?, boolean(), default: false
      field :unix_fd_transport?, boolean(), default: false
      field :expected_guid, binary() | nil, default: nil
    end
  end

  @type result :: %{guid: binary(), unix_fd_negotiated?: boolean(), rest: binary()}

  @doc false
  @spec run(:socket.socket(), binary(), integer(), pos_integer(), Options.t()) ::
          {:ok, result()} | {:error, term()}
  def run(sock, auth_id, deadline, maximum, %Options{} = options)
      when is_binary(auth_id) and is_integer(deadline) and is_integer(maximum) and maximum > 0 do
    with {:ok, guid, rest} <- authenticate(options, sock, auth_id, deadline, maximum),
         :ok <- verify_expected_guid(guid, options.expected_guid),
         {:ok, unix_fd_negotiated?, rest} <-
           negotiate_unix_fd(options, sock, rest, deadline, maximum),
         :ok <-
           handshake_send_with_deadline(sock, "BEGIN \r\n", options, deadline, maximum) do
      {:ok, %{guid: guid, unix_fd_negotiated?: unix_fd_negotiated?, rest: rest}}
    end
  end

  # EXTERNAL remains the first authentication mechanism. If it is rejected the
  # advertised list determines a bounded, deterministic retry: cookie first,
  # anonymous only when the caller explicitly enabled it. Each mechanism can be
  # attempted once; later REJECTED lists are parsed for protocol safety but do
  # not alter the original mechanism selection.
  defp authenticate(options, sock, auth_id, deadline, maximum) do
    with :ok <-
           handshake_send_with_deadline(
             sock,
             [0, "AUTH EXTERNAL ", auth_id, "\r\n"],
             options,
             deadline,
             maximum
           ),
         {:ok, line, rest} <-
           handshake_recv_with_deadline(options, sock, <<>>, deadline, maximum) do
      case parse_auth_response(line) do
        {:ok, guid} ->
          {:ok, guid, rest}

        {:rejected, mechanisms} ->
          authenticate_rejected(options, sock, auth_id, mechanisms, rest, deadline, maximum)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  defp authenticate_rejected(options, sock, auth_id, mechanisms, rest, deadline, maximum) do
    cond do
      "DBUS_COOKIE_SHA1" in mechanisms ->
        authenticate_cookie(options, sock, auth_id, mechanisms, rest, deadline, maximum)

      options.allow_anonymous? and "ANONYMOUS" in mechanisms ->
        authenticate_anonymous(options, sock, rest, deadline, maximum)

      true ->
        {:error, {:auth_rejected, mechanisms}}
    end
  end

  defp authenticate_cookie(options, sock, auth_id, mechanisms, rest, deadline, maximum) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum) do
      case get_auth_username(timeout, options.identity) do
        {:ok, username} ->
          authenticate_cookie_with_username(
            options,
            sock,
            auth_id,
            username,
            rest,
            deadline,
            maximum
          )

        {:error, :auth_cookie_unavailable} ->
          cookie_unavailable_before_auth(options, sock, mechanisms, rest, deadline, maximum)

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  # A local username is the initial response for DBUS_COOKIE_SHA1. If it cannot
  # be acquired, no cookie mechanism has started: send ANONYMOUS directly only
  # when the caller opted in and the server advertised it. Once AUTH has been
  # sent, no weaker fallback is permitted.
  defp cookie_unavailable_before_auth(
         %Options{allow_anonymous?: true} = options,
         sock,
         mechanisms,
         rest,
         deadline,
         maximum
       ) do
    if "ANONYMOUS" in mechanisms,
      do: authenticate_anonymous(options, sock, rest, deadline, maximum),
      else: {:error, :auth_cookie_unavailable}
  end

  defp cookie_unavailable_before_auth(_options, _sock, _mechanisms, _rest, _deadline, _maximum),
    do: {:error, :auth_cookie_unavailable}

  defp authenticate_cookie_with_username(
         options,
         sock,
         auth_id,
         username,
         rest,
         deadline,
         maximum
       ) do
    with :ok <-
           handshake_send_with_deadline(
             sock,
             ["AUTH DBUS_COOKIE_SHA1 ", Base.encode16(username, case: :lower), "\r\n"],
             options,
             deadline,
             maximum
           ),
         {:ok, line, rest} <-
           handshake_recv_with_deadline(options, sock, rest, deadline, maximum) do
      case line do
        "DATA " <> challenge ->
          authenticate_cookie_data(
            options,
            sock,
            auth_id,
            username,
            challenge,
            rest,
            deadline,
            maximum
          )

        "REJECTED" <> _rest ->
          # A mechanism rejection is terminal: do not silently lower the
          # authentication level after starting DBUS_COOKIE_SHA1.
          case parse_auth_response(line) do
            {:rejected, advertised} -> {:error, {:auth_rejected, advertised}}
            {:error, reason} -> {:error, reason}
          end

        _ ->
          {:error, :auth_failed}
      end
    else
      # Once DBUS_COOKIE_SHA1 AUTH is on the wire, even a local credential
      # failure is terminal. A peer must not be able to steer a client toward
      # ANONYMOUS by offering an unavailable context or cookie ID.
      {:error, :auth_cookie_unavailable} -> {:error, :auth_cookie_unavailable}
      {:error, reason} -> {:error, reason}
    end
  end

  defp authenticate_cookie_data(
         options,
         sock,
         auth_id,
         username,
         challenge,
         rest,
         deadline,
         maximum
       ) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum),
         {:ok, uid} <- auth_id_uid(auth_id),
         {:ok, response} <- cookie_response(username, uid, challenge, timeout),
         :ok <-
           handshake_send_with_deadline(
             sock,
             ["DATA ", response, "\r\n"],
             options,
             deadline,
             maximum
           ),
         {:ok, line, rest} <-
           handshake_recv_with_deadline(options, sock, rest, deadline, maximum) do
      case parse_auth_response(line) do
        {:ok, guid} -> {:ok, guid, rest}
        # A response that reached the server must not be followed by a weaker
        # mechanism, even when anonymous was explicitly enabled.
        {:rejected, _mechanisms} -> {:error, :auth_failed}
        {:error, reason} -> {:error, reason}
      end
    else
      # A received challenge ties the following credential lookup to
      # DBUS_COOKIE_SHA1. Do not emit CANCEL or attempt ANONYMOUS after a
      # missing/ambiguous cookie, including a peer-chosen context or ID.
      {:error, :auth_cookie_unavailable} -> {:error, :auth_cookie_unavailable}
      {:error, reason} -> {:error, reason}
    end
  end

  defp authenticate_anonymous(options, sock, rest, deadline, maximum) do
    with :ok <-
           handshake_send_with_deadline(sock, "AUTH ANONYMOUS\r\n", options, deadline, maximum),
         {:ok, line, rest} <-
           handshake_recv_with_deadline(options, sock, rest, deadline, maximum) do
      case parse_auth_response(line) do
        {:ok, guid} -> {:ok, guid, rest}
        {:rejected, mechanisms} -> {:error, {:auth_rejected, mechanisms}}
        {:error, reason} -> {:error, reason}
      end
    end
  end

  defp parse_auth_response(<<"OK ", guid::binary-size(32)>>) do
    if valid_guid?(guid), do: {:ok, :binary.copy(guid)}, else: {:error, :auth_failed}
  end

  defp parse_auth_response("REJECTED" <> _rest = line) do
    case Auth.parse_rejected(line) do
      {:ok, mechanisms} -> {:rejected, mechanisms}
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_auth_response(_line), do: {:error, :auth_failed}

  defp verify_expected_guid(_guid, nil), do: :ok

  defp verify_expected_guid(guid, expected_guid) do
    if guid_equal?(guid, expected_guid), do: :ok, else: {:error, :guid_mismatch}
  end

  @doc false
  @spec valid_guid?(term()) :: boolean()
  def valid_guid?(guid) when is_binary(guid) and byte_size(guid) == 32, do: hex_guid?(guid)
  def valid_guid?(_guid), do: false

  defp hex_guid?(guid), do: all_bytes?(guid, &hex_byte?/1)

  defp hex_byte?(byte) when byte in ?0..?9 or byte in ?a..?f or byte in ?A..?F, do: true
  defp hex_byte?(_byte), do: false

  defp guid_equal?(<<>>, <<>>), do: true

  defp guid_equal?(<<left, left_rest::binary>>, <<right, right_rest::binary>>) do
    ascii_lower(left) == ascii_lower(right) and guid_equal?(left_rest, right_rest)
  end

  defp guid_equal?(_left, _right), do: false

  defp ascii_lower(byte) when byte in ?A..?Z, do: byte + 32
  defp ascii_lower(byte), do: byte

  @doc false
  @spec get_auth_id(pos_integer(), module()) ::
          {:ok, binary()} | {:error, :auth_id_unavailable | :read_timeout}
  def get_auth_id(timeout, identity)
      when is_integer(timeout) and timeout > 0 and is_atom(identity) do
    case safely_lookup_identity(identity, :auth_id, timeout) do
      {:ok, output} when is_binary(output) and byte_size(output) <= @max_auth_id_output ->
        case String.trim(output) do
          uid when uid != <<>> ->
            if uid_bytes?(uid),
              do: {:ok, :binary.encode_hex(uid)},
              else: {:error, :auth_id_unavailable}

          _ ->
            {:error, :auth_id_unavailable}
        end

      {:error, :timeout} ->
        {:error, :read_timeout}

      _ ->
        {:error, :auth_id_unavailable}
    end
  end

  @doc false
  @spec get_auth_username(pos_integer(), module()) ::
          {:ok, binary()} | {:error, :auth_cookie_unavailable | :read_timeout}
  def get_auth_username(timeout, identity)
      when is_integer(timeout) and timeout > 0 and is_atom(identity) do
    case safely_lookup_identity(identity, :username, timeout) do
      {:ok, output} when is_binary(output) and byte_size(output) <= @max_auth_id_output ->
        username = String.trim(output)

        if valid_auth_username?(username),
          do: {:ok, :binary.copy(username)},
          else: {:error, :auth_cookie_unavailable}

      {:error, :timeout} ->
        {:error, :read_timeout}

      _ ->
        {:error, :auth_cookie_unavailable}
    end
  end

  defp valid_auth_username?(username) when byte_size(username) in 1..64,
    do: all_bytes?(username, &visible_ascii_byte?/1)

  defp valid_auth_username?(_username), do: false

  defp visible_ascii_byte?(byte), do: byte in 0x21..0x7E

  defp auth_id_uid(auth_id) when is_binary(auth_id) do
    with {:ok, uid_bytes} <- Base.decode16(auth_id, case: :mixed),
         {uid, <<>>} <- Integer.parse(uid_bytes),
         true <- uid >= 0 and uid <= 4_294_967_295 do
      {:ok, uid}
    else
      _ -> {:error, :auth_failed}
    end
  end

  # File metadata and reads are local but can still block on a hostile mount.
  # Keep the whole credential operation inside the same setup deadline without
  # retaining either the cookie or server challenge in Connection state.
  defp cookie_response(username, uid, challenge, timeout) do
    ref = make_ref()
    delivery_alias = :erlang.alias([:reply])

    pid =
      spawn_link(fn ->
        send(delivery_alias, {ref, safe_cookie_response(username, uid, challenge)})
      end)

    monitor_ref = Process.monitor(pid)

    await_cookie_response(pid, ref, delivery_alias, monitor_ref, timeout)
  end

  defp safe_cookie_response(username, uid, challenge) do
    Auth.cookie_response(username, uid, challenge)
  rescue
    _exception -> {:error, :auth_cookie_unavailable}
  catch
    _kind, _reason -> {:error, :auth_cookie_unavailable}
  end

  defp await_cookie_response(pid, ref, delivery_alias, monitor_ref, timeout) do
    receive do
      {^ref, result} ->
        result

      {:DOWN, ^monitor_ref, :process, ^pid, _reason} ->
        {:error, :auth_cookie_unavailable}
    after
      timeout ->
        Process.unlink(pid)
        :erlang.unalias(delivery_alias)
        Process.exit(pid, :kill)
        {:error, :read_timeout}
    end
  after
    # The one-shot alias rejects a late worker result atomically. Drain a
    # response queued before unaliasing so a derived digest cannot linger in
    # this GenServer's mailbox after the bounded credential operation ends.
    :erlang.unalias(delivery_alias)
    drain_cookie_response_delivery(ref)
    Process.demonitor(monitor_ref, [:flush])
  end

  defp drain_cookie_response_delivery(ref) do
    receive do
      {^ref, _result} -> :ok
    after
      0 -> :ok
    end
  end

  defp safely_lookup_identity(identity, function, timeout) do
    apply(identity, function, [timeout])
  rescue
    _exception -> {:error, :lookup_failed}
  catch
    _kind, _reason -> {:error, :lookup_failed}
  end

  defp uid_bytes?(uid), do: all_bytes?(uid, &digit_byte?/1)

  defp digit_byte?(byte), do: byte in ?0..?9

  # Walk the binary directly: no intermediate list, and the first byte that
  # fails the predicate ends the walk.
  defp all_bytes?(<<>>, _predicate), do: true

  defp all_bytes?(<<byte, rest::binary>>, predicate),
    do: predicate.(byte) and all_bytes?(rest, predicate)

  defp handshake_recv(transport, sock, buffer, timeout) when is_binary(buffer) do
    receive_auth_line(transport, sock, buffer, read_deadline(timeout), timeout)
  end

  defp handshake_recv_with_deadline(options, sock, buffer, deadline, maximum) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum) do
      handshake_recv(options.transport, sock, buffer, timeout)
    end
  end

  defp handshake_send_with_deadline(sock, data, options, deadline, maximum) do
    with {:ok, timeout} <- remaining_setup_timeout(deadline, maximum) do
      handshake_send(options.transport, sock, data, min(timeout, options.write_timeout))
    end
  end

  # Unix-FD negotiation is an optional authentication extension. A peer's
  # ERROR leaves the ordinary D-Bus connection usable, but FD-bearing messages
  # will be rejected before any bytes are sent. We only issue it on local Unix
  # stream sockets where SCM_RIGHTS is available to OTP.
  defp negotiate_unix_fd(
         %Options{unix_fd_transport?: false},
         _sock,
         rest,
         _deadline,
         _maximum
       ),
       do: {:ok, false, rest}

  defp negotiate_unix_fd(%Options{} = options, sock, rest, deadline, maximum) do
    with :ok <-
           handshake_send_with_deadline(
             sock,
             "NEGOTIATE_UNIX_FD\r\n",
             options,
             deadline,
             maximum
           ),
         {:ok, line, rest} <-
           handshake_recv_with_deadline(options, sock, rest, deadline, maximum) do
      case line do
        "AGREE_UNIX_FD" -> {:ok, true, rest}
        "ERROR" <> _reason -> {:ok, false, rest}
        _ -> {:error, :auth_failed}
      end
    end
  end

  defp receive_auth_line(transport, sock, buffer, deadline, timeout) do
    # Previous reads can contain multiple auth lines. Consume one already in
    # the bounded buffer before touching the socket: the peer may legitimately
    # have closed after coalescing its next response.
    case consume_auth_buffer(buffer) do
      {:ok, _line, _rest} = result ->
        result

      :incomplete ->
        receive_auth_socket_data(transport, sock, buffer, deadline, timeout)

      {:error, :auth_failed} = error ->
        error
    end
  end

  defp receive_auth_socket_data(transport, sock, buffer, deadline, timeout) do
    case remaining_timeout(deadline, timeout) do
      :expired ->
        {:error, :read_timeout}

      {:ok, receive_timeout} ->
        case transport.recv(sock, 0, [], receive_timeout) do
          {:ok, data} ->
            consume_auth_data(transport, sock, buffer, data, deadline, timeout)

          {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
            consume_auth_data(transport, sock, buffer, data, deadline, timeout)

          {:error, :timeout} ->
            {:error, :read_timeout}

          {:error, {:timeout, _data}} ->
            {:error, :read_timeout}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp consume_auth_data(transport, sock, buffer, data, deadline, timeout) do
    case consume_auth_buffer(buffer <> data) do
      {:ok, _line, _rest} = result -> result
      {:error, :auth_failed} = error -> error
      :incomplete -> receive_auth_line(transport, sock, buffer <> data, deadline, timeout)
    end
  end

  defp consume_auth_buffer(buffer) do
    case :binary.match(buffer, "\r\n") do
      {line_size, 2} when line_size <= @max_auth_line_size ->
        line = binary_part(buffer, 0, line_size)
        rest_size = byte_size(buffer) - line_size - 2
        rest = binary_part(buffer, line_size + 2, rest_size)
        {:ok, line, rest}

      {_, 2} ->
        {:error, :auth_failed}

      :nomatch when byte_size(buffer) > @max_auth_line_size ->
        {:error, :auth_failed}

      :nomatch ->
        :incomplete
    end
  end

  defp handshake_send(transport, sock, data, timeout) do
    case transport.send(sock, data, [], timeout) do
      :ok -> :ok
      {:error, reason} -> {:error, SocketError.normalize(reason)}
      _other -> {:error, :send_failed}
    end
  end

  defp read_deadline(timeout) when is_integer(timeout) and timeout > 0 do
    System.monotonic_time(:millisecond) + timeout
  end

  defp remaining_timeout(deadline, maximum) when is_integer(deadline) and maximum > 0 do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, min(remaining, maximum)}
      _ -> :expired
    end
  end

  defp remaining_setup_timeout(deadline, maximum) do
    case remaining_timeout(deadline, maximum) do
      {:ok, timeout} -> {:ok, timeout}
      :expired -> {:error, :read_timeout}
    end
  end
end
