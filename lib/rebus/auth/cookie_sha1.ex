defmodule Rebus.Auth.CookieSha1 do
  @moduledoc false

  # DBUS_COOKIE_SHA1 proves the client can read a secret cookie from the
  # user's keyring: the client names the user, the server answers with a
  # keyring context, a cookie ID and a challenge, and the client replies with
  # its own challenge and a SHA-1 digest over both challenges and the cookie.
  # It is the only mechanism a TCP address can authenticate with.
  #
  # Once AUTH has been sent this mechanism never yields to a weaker one: a
  # rejection, a protocol failure or an unavailable credential all end the
  # handshake, so a peer cannot steer a client toward ANONYMOUS.

  @behaviour Rebus.Auth.Mechanism

  alias Rebus.Auth.CookieSha1.Keyring

  require Logger

  @max_cookie_challenge_size 512
  @max_username_output 64

  @type auth_error :: :auth_cookie_unavailable | :auth_failed

  @impl true
  def name, do: "DBUS_COOKIE_SHA1"

  # A local username is the initial response. If it cannot be acquired, no
  # cookie mechanism has started, so the handshake may still move on to
  # ANONYMOUS when the caller opted in and the server advertised it.
  @impl true
  def initial_response(%{identity: identity, timeout: timeout}) do
    with {:ok, timeout} <- timeout.() do
      case get_auth_username(timeout, identity) do
        {:ok, username} ->
          {:ok, Base.encode16(username, case: :lower), {:waiting_for_data, username}}

        {:error, :auth_cookie_unavailable} ->
          {:unavailable, :auth_cookie_unavailable}

        {:error, reason} ->
          {:error, reason}
      end
    end
  end

  # A received challenge ties the following credential lookup to
  # DBUS_COOKIE_SHA1. Do not emit CANCEL or attempt ANONYMOUS after a
  # missing/ambiguous cookie, including a peer-chosen context or ID: every
  # failure here is terminal.
  @impl true
  def handle_data(challenge, {:waiting_for_data, username}, %{auth_id: auth_id, timeout: timeout}) do
    with {:ok, timeout} <- timeout.(),
         {:ok, uid} <- auth_id_uid(auth_id),
         {:ok, response} <- bounded_cookie_response(username, uid, challenge, timeout) do
      {:reply, response, :waiting_for_ok}
    end
  end

  def handle_data(_data, :waiting_for_ok, _context), do: {:error, :auth_failed}

  # A mechanism rejection is terminal: do not silently lower the
  # authentication level after starting DBUS_COOKIE_SHA1. A response that
  # reached the server must not be followed by a weaker mechanism either, even
  # when anonymous was explicitly enabled. OK is only accepted as the answer
  # to that response.
  @impl true
  def handle_verdict(:ok, :waiting_for_ok), do: :ok

  def handle_verdict({:rejected, mechanisms}, {:waiting_for_data, _username}),
    do: {:error, {:auth_rejected, mechanisms}}

  def handle_verdict(_verdict, _state), do: {:error, :auth_failed}

  @doc false
  @spec get_auth_username(pos_integer(), module()) ::
          {:ok, binary()} | {:error, :auth_cookie_unavailable | :read_timeout}
  def get_auth_username(timeout, identity)
      when is_integer(timeout) and timeout > 0 and is_atom(identity) do
    case safely_lookup_identity(identity, :username, timeout) do
      {:ok, output} when is_binary(output) and byte_size(output) <= @max_username_output ->
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

  @doc false
  @spec cookie_response(binary(), non_neg_integer(), binary()) ::
          {:ok, binary()} | {:error, auth_error()}
  def cookie_response(username, uid, encoded_challenge)
      when is_binary(username) and is_integer(uid) and uid >= 0 and
             is_binary(encoded_challenge) do
    with :ok <- validate_username(username),
         {:ok, context, cookie_id, server_challenge} <- parse_cookie_challenge(encoded_challenge),
         {:ok, cookie} <- Keyring.read_cookie(context, cookie_id, uid) do
      client_challenge = :crypto.strong_rand_bytes(16) |> Base.encode16(case: :lower)

      digest =
        :crypto.hash(:sha, [server_challenge, ":", client_challenge, ":", cookie])
        |> Base.encode16(case: :lower)

      response = Base.encode16(client_challenge <> " " <> digest, case: :lower)
      {:ok, response}
    else
      {:error, {:unavailable, reason}} -> cookie_unavailable(reason)
      {:error, :auth_failed} -> {:error, :auth_failed}
      {:error, :auth_cookie_unavailable} -> cookie_unavailable(:internal)
      _ -> {:error, :auth_failed}
    end
  end

  def cookie_response(_username, _uid, _encoded_challenge), do: cookie_unavailable(:internal)

  # The single diagnostic boundary. Only the category atom is interpolated, and
  # only it reaches the Logger metadata, so no path, cookie ID, cookie value,
  # challenge, identity or raw protocol byte can be disclosed by construction.
  # `:auth_failed` is not logged: its inputs are peer-controlled.
  @spec cookie_unavailable(Keyring.unavailable_reason()) :: {:error, :auth_cookie_unavailable}
  defp cookie_unavailable(reason) do
    Logger.warning("D-Bus cookie authentication unavailable reason=#{reason}", reason: reason)
    {:error, :auth_cookie_unavailable}
  end

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
  defp bounded_cookie_response(username, uid, challenge, timeout) do
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
    cookie_response(username, uid, challenge)
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

  defp parse_cookie_challenge(encoded)
       when byte_size(encoded) > 0 and byte_size(encoded) <= @max_cookie_challenge_size * 2 and
              rem(byte_size(encoded), 2) == 0 do
    with {:ok, decoded} <- Base.decode16(encoded, case: :mixed),
         [context, cookie_id, server_challenge] <- :binary.split(decoded, " ", [:global]),
         :ok <- Keyring.validate_context(context),
         :ok <- Keyring.validate_cookie_id(cookie_id),
         :ok <- validate_challenge(server_challenge) do
      {:ok, context, cookie_id, server_challenge}
    else
      _ -> {:error, :auth_failed}
    end
  end

  defp parse_cookie_challenge(_encoded), do: {:error, :auth_failed}

  defp validate_challenge(challenge)
       when byte_size(challenge) in 1..@max_cookie_challenge_size do
    if all_bytes?(challenge, &visible_ascii_byte?/1), do: :ok, else: :error
  end

  defp validate_challenge(_challenge), do: :error

  defp validate_username(username) do
    if valid_auth_username?(username), do: :ok, else: :error
  end

  defp valid_auth_username?(username) when byte_size(username) in 1..64,
    do: all_bytes?(username, &visible_ascii_byte?/1)

  defp valid_auth_username?(_username), do: false

  defp safely_lookup_identity(identity, function, timeout) do
    apply(identity, function, [timeout])
  rescue
    _exception -> {:error, :lookup_failed}
  catch
    _kind, _reason -> {:error, :lookup_failed}
  end

  defp visible_ascii_byte?(byte), do: byte in 0x21..0x7E

  # Walk the binary directly: no intermediate list, and the first byte that
  # fails the predicate ends the walk.
  defp all_bytes?(<<>>, _predicate), do: true

  defp all_bytes?(<<byte, rest::binary>>, predicate),
    do: predicate.(byte) and all_bytes?(rest, predicate)
end
