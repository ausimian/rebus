defmodule Rebus.Connection.Handshake do
  @moduledoc false

  # The SASL-style line protocol a D-Bus connection performs on a freshly
  # connected socket, from the initial NUL byte through to BEGIN. It owns no
  # process state: everything it needs arrives in `Options`, every byte moves
  # through the transport module it is given, and the leftover bytes read past
  # BEGIN are handed back to the caller as the start of the message stream.
  #
  # The handshake knows the protocol but no mechanism: each one implements
  # `Rebus.Auth.Mechanism`, and decides what to send and what the server's
  # answers mean for it.

  alias Rebus.Auth
  alias Rebus.Auth.{Anonymous, CookieSha1, External}
  alias Rebus.Connection.SocketError

  @max_auth_line_size 1_024
  @max_auth_id_output 64

  defmodule Options do
    @moduledoc false

    use TypedStruct

    # The connection settings the handshake reads. `write_timeout` bounds a
    # single send in addition to the shared setup deadline, and `clock` is the
    # monotonic clock that deadline was taken from; the remaining fields select
    # mechanisms and the optional unix-FD extension.

    typedstruct enforce: true do
      field :transport, module()
      field :identity, module()
      field :write_timeout, pos_integer()
      field :clock, module(), default: Rebus.Clock.System
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
    # Every send and receive draws on the one setup deadline.
    io = %{sock: sock, options: options, deadline: deadline, maximum: maximum}

    with {:ok, guid, rest} <- authenticate(io, auth_id),
         :ok <- verify_expected_guid(guid, options.expected_guid),
         {:ok, unix_fd_negotiated?, rest} <- negotiate_unix_fd(io, rest),
         :ok <- send_line(io, "BEGIN \r\n") do
      {:ok, %{guid: guid, unix_fd_negotiated?: unix_fd_negotiated?, rest: rest}}
    end
  end

  # EXTERNAL remains the first authentication mechanism. If it is rejected the
  # advertised list determines a bounded, deterministic retry: cookie first,
  # anonymous only when the caller explicitly enabled it. Each mechanism can be
  # attempted once, and only the mechanism being rejected decides whether a
  # rejection allows a fallback at all.
  defp mechanisms(%Options{allow_anonymous?: true}), do: [External, CookieSha1, Anonymous]
  defp mechanisms(%Options{}), do: [External, CookieSha1]

  defp authenticate(io, auth_id) do
    context = %{
      auth_id: auth_id,
      identity: io.options.identity,
      timeout: fn -> remaining_setup_timeout(io) end
    }

    # The NUL byte that opens the connection travels with the first AUTH.
    start(mechanisms(io.options), <<0>>, {:error, :auth_failed}, io, context, <<>>)
  end

  # Sends AUTH for the first candidate able to produce an initial response. A
  # candidate that cannot has sent nothing, so the next one may be tried; when
  # none is left, `failure` is the result.
  defp start([mechanism | candidates], prefix, _failure, io, context, buffer) do
    case mechanism.initial_response(context) do
      {:ok, response, state} ->
        auth = [prefix, "AUTH ", mechanism.name(), initial_response(response), "\r\n"]

        with :ok <- send_line(io, auth) do
          await({mechanism, state, candidates}, io, context, buffer)
        end

      {:unavailable, reason} ->
        start(candidates, prefix, {:error, reason}, io, context, buffer)

      {:error, _reason} = error ->
        error
    end
  end

  defp start([], _prefix, failure, _io, _context, _buffer), do: failure

  defp initial_response(nil), do: []
  defp initial_response(response), do: [" ", response]

  # An attempt is the mechanism in progress, its state, and the mechanisms
  # still allowed after it.
  defp await({mechanism, state, _candidates} = attempt, io, context, buffer) do
    with {:ok, line, rest} <- recv_line(io, buffer) do
      case parse_auth_line(line) do
        {:data, data} ->
          data
          |> mechanism.handle_data(state, context)
          |> follow_data(attempt, io, context, rest)

        {:error, _reason} = error ->
          error

        verdict ->
          verdict
          |> verdict_kind()
          |> mechanism.handle_verdict(state)
          |> follow_verdict(verdict, attempt, io, context, rest)
      end
    end
  end

  defp follow_data({:reply, reply, state}, {mechanism, _state, candidates}, io, context, rest) do
    with :ok <- send_line(io, ["DATA ", reply, "\r\n"]) do
      await({mechanism, state, candidates}, io, context, rest)
    end
  end

  defp follow_data(:cancel, {_mechanism, _state, candidates}, io, context, rest),
    do: cancel(candidates, io, context, rest)

  defp follow_data({:error, _reason} = error, _attempt, _io, _context, _rest), do: error

  defp verdict_kind({:ok, _guid}), do: :ok
  defp verdict_kind(verdict), do: verdict

  defp follow_verdict(:ok, {:ok, guid}, _attempt, _io, _context, rest), do: {:ok, guid, rest}

  defp follow_verdict(
         :fallback,
         {:rejected, mechanisms},
         {_mechanism, _state, candidates},
         io,
         context,
         rest
       ),
       do: fall_back(candidates, mechanisms, io, context, rest)

  defp follow_verdict(:cancel, _verdict, {_mechanism, _state, candidates}, io, context, rest),
    do: cancel(candidates, io, context, rest)

  defp follow_verdict({:error, _reason} = error, _verdict, _attempt, _io, _context, _rest),
    do: error

  # Only REJECTED may follow a CANCEL, and it moves on to the next advertised
  # mechanism as the specification requires: the cancelled mechanism has
  # already abandoned its exchange.
  defp cancel(candidates, io, context, buffer) do
    with :ok <- send_line(io, "CANCEL\r\n"),
         {:ok, line, rest} <- recv_line(io, buffer) do
      case parse_auth_line(line) do
        {:rejected, mechanisms} -> fall_back(candidates, mechanisms, io, context, rest)
        {:error, _reason} = error -> error
        _other -> {:error, :auth_failed}
      end
    end
  end

  # A later REJECTED list only narrows the original order; it never adds a
  # mechanism or reorders them.
  defp fall_back(candidates, mechanisms, io, context, rest) do
    candidates
    |> Enum.filter(&(&1.name() in mechanisms))
    |> start(<<>>, {:error, {:auth_rejected, mechanisms}}, io, context, rest)
  end

  defp parse_auth_line(<<"OK ", guid::binary-size(32)>>) do
    if valid_guid?(guid), do: {:ok, :binary.copy(guid)}, else: {:error, :auth_failed}
  end

  defp parse_auth_line("REJECTED" <> _rest = line) do
    case Auth.parse_rejected(line) do
      {:ok, mechanisms} -> {:rejected, mechanisms}
      {:error, reason} -> {:error, reason}
    end
  end

  defp parse_auth_line("DATA"), do: {:data, <<>>}
  defp parse_auth_line("DATA " <> data), do: {:data, data}
  defp parse_auth_line("ERROR"), do: :error
  defp parse_auth_line("ERROR " <> _explanation), do: :error
  defp parse_auth_line(_line), do: {:error, :auth_failed}

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
        encode_auth_id(String.trim(output))

      {:error, :timeout} ->
        {:error, :read_timeout}

      _ ->
        {:error, :auth_id_unavailable}
    end
  end

  defp encode_auth_id(<<>>), do: {:error, :auth_id_unavailable}

  defp encode_auth_id(uid) do
    if uid_bytes?(uid),
      do: {:ok, :binary.encode_hex(uid)},
      else: {:error, :auth_id_unavailable}
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

  # Unix-FD negotiation is an optional authentication extension. A peer's
  # ERROR leaves the ordinary D-Bus connection usable, but FD-bearing messages
  # will be rejected before any bytes are sent. We only issue it on local Unix
  # stream sockets where SCM_RIGHTS is available to OTP.
  defp negotiate_unix_fd(%{options: %Options{unix_fd_transport?: false}}, rest),
    do: {:ok, false, rest}

  defp negotiate_unix_fd(io, rest) do
    with :ok <- send_line(io, "NEGOTIATE_UNIX_FD\r\n"),
         {:ok, line, rest} <- recv_line(io, rest) do
      case line do
        "AGREE_UNIX_FD" -> {:ok, true, rest}
        "ERROR" <> _reason -> {:ok, false, rest}
        _ -> {:error, :auth_failed}
      end
    end
  end

  defp recv_line(%{options: options, sock: sock} = io, buffer) when is_binary(buffer) do
    with {:ok, timeout} <- remaining_setup_timeout(io) do
      receive_auth_line(options, sock, buffer, read_deadline(options.clock, timeout), timeout)
    end
  end

  defp send_line(%{options: options, sock: sock} = io, data) do
    with {:ok, timeout} <- remaining_setup_timeout(io) do
      handshake_send(options.transport, sock, data, min(timeout, options.write_timeout))
    end
  end

  defp receive_auth_line(options, sock, buffer, deadline, timeout) do
    # Previous reads can contain multiple auth lines. Consume one already in
    # the bounded buffer before touching the socket: the peer may legitimately
    # have closed after coalescing its next response.
    case consume_auth_buffer(buffer) do
      {:ok, _line, _rest} = result ->
        result

      :incomplete ->
        receive_auth_socket_data(options, sock, buffer, deadline, timeout)

      {:error, :auth_failed} = error ->
        error
    end
  end

  defp receive_auth_socket_data(options, sock, buffer, deadline, timeout) do
    case remaining_timeout(options.clock, deadline, timeout) do
      :expired ->
        {:error, :read_timeout}

      {:ok, receive_timeout} ->
        case options.transport.recv(sock, 0, [], receive_timeout) do
          {:ok, data} ->
            consume_auth_data(options, sock, buffer, data, deadline, timeout)

          {:error, {:timeout, data}} when is_binary(data) and byte_size(data) > 0 ->
            consume_auth_data(options, sock, buffer, data, deadline, timeout)

          {:error, :timeout} ->
            {:error, :read_timeout}

          {:error, {:timeout, _data}} ->
            {:error, :read_timeout}

          {:error, reason} ->
            {:error, reason}
        end
    end
  end

  defp consume_auth_data(options, sock, buffer, data, deadline, timeout) do
    case consume_auth_buffer(buffer <> data) do
      {:ok, _line, _rest} = result -> result
      {:error, :auth_failed} = error -> error
      :incomplete -> receive_auth_line(options, sock, buffer <> data, deadline, timeout)
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

  defp read_deadline(clock, timeout) when is_integer(timeout) and timeout > 0 do
    clock.monotonic_time() + timeout
  end

  defp remaining_timeout(clock, deadline, maximum) when is_integer(deadline) and maximum > 0 do
    case deadline - clock.monotonic_time() do
      remaining when remaining > 0 -> {:ok, min(remaining, maximum)}
      _ -> :expired
    end
  end

  defp remaining_setup_timeout(%{
         options: %Options{clock: clock},
         deadline: deadline,
         maximum: maximum
       }) do
    case remaining_timeout(clock, deadline, maximum) do
      {:ok, timeout} -> {:ok, timeout}
      :expired -> {:error, :read_timeout}
    end
  end
end
