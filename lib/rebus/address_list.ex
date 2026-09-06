defmodule Rebus.AddressList do
  @moduledoc false

  # The D-Bus bus-address list walk. `Rebus.connect/2` parses a `:system` or
  # `:session` address string into candidates and hands them here; this module
  # owns candidate ordering, TCP resolution, the shared setup deadline and its
  # fair-share slicing, failure diagnostics, and the retryable/terminal error
  # classification.

  alias Rebus.Connection.Handshake

  require Logger

  @default_connection_timeout 5_000
  @max_tcp_addresses_per_family 4
  @minimum_address_attempt_timeout 50

  defmodule Walk do
    @moduledoc false

    # The state threaded through every step of one address-list walk.
    #
    # - `conn` is the `{caller_options, internal_arguments}` pair a candidate
    #   connection is started with. Per-attempt internal arguments are layered
    #   onto it just before each connect, never onto the caller's options.
    # - `deadline` is the single monotonic setup deadline shared by the auth-ID
    #   lookup and every resolver and pre-Hello attempt.
    # - `impl` holds the implementation modules (clock, resolver, connector,
    #   identity) the walk calls out through.
    # - `last_error` is the most recent retryable failure, or `nil` while no
    #   attempt has failed. It shapes both the timeout error and the final one.
    # - `candidate_ordinal` and `ip_ordinal` are the debug-diagnostic positions
    #   of the current D-Bus entry and, within a TCP entry, its resolved IP.
    #   `ip_ordinal` is 0 for a Unix candidate and for TCP resolution itself.
    # - `remaining_candidate_count` is how many connectable D-Bus entries follow
    #   the current one; it is a share of the deadline the current entry must
    #   leave unspent.

    @enforce_keys [:conn, :deadline, :impl]
    defstruct [
      :conn,
      :deadline,
      :impl,
      last_error: nil,
      candidate_ordinal: 1,
      ip_ordinal: 0,
      remaining_candidate_count: 0
    ]

    @type t :: %__MODULE__{
            conn: {keyword(), map()},
            deadline: integer(),
            impl: Rebus.Impl.t(),
            last_error: term(),
            candidate_ordinal: pos_integer(),
            ip_ordinal: non_neg_integer(),
            remaining_candidate_count: non_neg_integer()
          }
  end

  alias Rebus.AddressList.Walk

  @doc """
  Walks a parsed bus-address candidate list until one connection is established.
  """
  @spec connect([Rebus.BusAddress.candidate()], keyword()) ::
          {:ok, pid()} | {:error, term()}
  def connect(candidates, opts)
      when is_list(candidates) and is_list(opts) do
    impl = Rebus.Impl.from_options(opts)

    with {:ok, timeout} <- address_list_timeout(opts) do
      deadline = impl.clock.monotonic_time() + timeout

      case resolve_list_auth_id(candidates, deadline, impl) do
        {:ok, auth_id} ->
          connect_candidates(candidates, %Walk{
            conn: {opts, %{impl: impl, precomputed_auth_id: auth_id}},
            deadline: deadline,
            impl: impl
          })

        {:error, {:address_list_timeout, reason}} ->
          {:error, reason}

        {:error, _reason} = error ->
          error
      end
    end
  end

  defp resolve_list_auth_id([], _deadline, _impl),
    do: {:error, :unsupported_bus_transport}

  defp resolve_list_auth_id(candidates, deadline, impl) do
    candidate_count = connectable_candidate_count(candidates)

    if candidate_count == 0 do
      {:error, :unsupported_bus_transport}
    else
      with {:ok, timeout} <-
             address_attempt_timeout(deadline, candidate_count + 1, impl.clock, nil) do
        Handshake.get_auth_id(timeout, impl.identity)
      end
    end
  end

  defp connect_candidates([], %Walk{last_error: nil}),
    do: {:error, :unsupported_bus_transport}

  defp connect_candidates([], %Walk{last_error: error}),
    do: final_address_list_error(error)

  defp connect_candidates([:unsupported | candidates], %Walk{} = walk),
    do: connect_candidates(candidates, next_candidate(walk))

  defp connect_candidates([candidate | candidates], %Walk{} = walk) do
    case connect_candidate(candidate, candidates, walk) do
      {:ok, _pid} = result ->
        result

      {:error, {:address_list_timeout, reason}} ->
        {:error, reason}

      {:error, reason} = error ->
        if retryable_bus_address_error?(reason) do
          connect_candidates(candidates, walk |> put_last_error(reason) |> next_candidate())
        else
          error
        end
    end
  end

  defp connect_candidate({:local, path, expected_guid}, remaining_candidates, %Walk{} = walk) do
    walk = %{walk | remaining_candidate_count: connectable_candidate_count(remaining_candidates)}

    with {:ok, timeout} <- attempt_timeout(walk, 1 + walk.remaining_candidate_count) do
      %{family: :local, path: path}
      |> connect_with_address_diagnostic(
        put_attempt(walk, timeout, expected_guid),
        :unix,
        timeout
      )
    end
  end

  defp connect_candidate(
         {:tcp, host, port, family, expected_guid},
         remaining_candidates,
         %Walk{} = walk
       ) do
    walk = %{walk | remaining_candidate_count: connectable_candidate_count(remaining_candidates)}

    with {:ok, addresses} <- resolve_tcp_addresses(host, family, walk) do
      connect_tcp_addresses(addresses, {port, expected_guid}, first_ip(walk))
    end
  end

  defp resolve_tcp_addresses(host, family, %Walk{} = walk) do
    families = if family == :unspec, do: [:inet6, :inet], else: [family]

    resolve_tcp_families(host, families, {[], []}, walk)
  end

  # Dialyzer analyses the dev build, where the implementation gate is closed and
  # the resolver is always `Rebus.Resolver.Inet`; from that alone it proves the
  # out-of-contract result clause and the non-atom reason clause unreachable.
  # The behaviour admits any error term, and the test build reaches both.
  @dialyzer {:no_match, [resolve_tcp_families: 4, safe_resolver_reason: 1]}

  # A resolution failure is remembered only for the remaining resolver families:
  # the caller keeps walking with the `last_error` it already held.
  defp resolve_tcp_families(_host, [], {[], reasons}, %Walk{}),
    do: {:error, {:tcp_resolution_failed, List.first(reasons) || :no_addresses}}

  defp resolve_tcp_families(_host, [], {addresses, _reasons}, %Walk{}),
    do: {:ok, addresses}

  defp resolve_tcp_families(host, [family | families], {addresses, reasons}, %Walk{} = walk) do
    with {:ok, timeout} <-
           attempt_timeout(walk, length([family | families]) + walk.remaining_candidate_count) do
      case walk.impl.resolver.getaddrs(host, family, timeout) do
        {:ok, resolved} when is_list(resolved) ->
          resolved_addresses =
            resolved
            |> Enum.take(@max_tcp_addresses_per_family)
            |> Enum.map(&{family, &1})

          resolve_tcp_families(host, families, {addresses ++ resolved_addresses, reasons}, walk)

        {:error, reason} ->
          safe_reason = safe_resolver_reason(reason)
          failure = {:tcp_resolution_failed, safe_reason}
          log_address_attempt(walk, :tcp, timeout, failure)

          resolve_tcp_families(
            host,
            families,
            {addresses, [safe_reason | reasons]},
            put_last_error(walk, failure)
          )

        _other ->
          log_address_attempt(walk, :tcp, timeout, :resolution_failed)

          resolve_tcp_families(
            host,
            families,
            {addresses, [:resolution_failed | reasons]},
            put_last_error(walk, {:tcp_resolution_failed, :resolution_failed})
          )
      end
    end
  end

  defp connect_tcp_addresses([], _endpoint, %Walk{last_error: nil}),
    do: {:error, {:tcp_resolution_failed, :no_addresses}}

  defp connect_tcp_addresses([], _endpoint, %Walk{
         last_error: error,
         remaining_candidate_count: remaining
       })
       when remaining > 0,
       do: {:error, error}

  defp connect_tcp_addresses([], _endpoint, %Walk{last_error: error}),
    do: final_address_list_error(error)

  defp connect_tcp_addresses(
         [{family, address} | addresses],
         {port, expected_guid} = endpoint,
         %Walk{} = walk
       ) do
    attempt_count = length(addresses) + 1 + walk.remaining_candidate_count

    with {:ok, timeout} <- attempt_timeout(walk, attempt_count) do
      %{family: family, addr: address, port: port}
      |> connect_with_address_diagnostic(
        put_attempt(walk, timeout, expected_guid),
        :tcp,
        timeout
      )
      |> retry_next_tcp_address(addresses, endpoint, walk)
    end
  end

  defp retry_next_tcp_address({:ok, _pid} = result, _addresses, _endpoint, _walk), do: result

  defp retry_next_tcp_address({:error, reason} = error, addresses, endpoint, walk) do
    if retryable_bus_address_error?(reason) do
      connect_tcp_addresses(addresses, endpoint, walk |> put_last_error(reason) |> next_ip())
    else
      error
    end
  end

  defp next_candidate(%Walk{} = walk),
    do: %{walk | candidate_ordinal: walk.candidate_ordinal + 1, ip_ordinal: 0}

  defp first_ip(%Walk{} = walk), do: %{walk | ip_ordinal: 1}

  defp next_ip(%Walk{} = walk), do: %{walk | ip_ordinal: walk.ip_ordinal + 1}

  defp put_last_error(%Walk{} = walk, reason), do: %{walk | last_error: reason}

  # The internal arguments a candidate connection is started with never travel
  # in the caller's option list.
  defp put_attempt(%Walk{conn: {conn_opts, internal}} = walk, timeout, expected_guid) do
    internal = Map.merge(internal, %{setup_timeout: timeout, expected_guid: expected_guid})

    %{walk | conn: {conn_opts, internal}}
  end

  defp connectable_candidate_count(candidates) do
    Enum.count(candidates, &connectable_candidate?/1)
  end

  defp connectable_candidate?({:local, _path, _expected_guid}), do: true
  defp connectable_candidate?({:tcp, _host, _port, _family, _expected_guid}), do: true
  defp connectable_candidate?(_candidate), do: false

  defp connect_with_address_diagnostic(address, %Walk{} = walk, transport, timeout) do
    case walk.impl.connector.connect(address, walk.conn) do
      {:error, reason} = error ->
        log_address_attempt(walk, transport, timeout, reason)
        error

      result ->
        result
    end
  end

  # Attempt diagnostics are debug level only, and carry ordinals, transport,
  # slice and a bounded reason. They never carry an address, host, path or GUID.
  defp log_address_attempt(%Walk{} = walk, transport, timeout, reason) do
    safe_reason = safe_address_failure_reason(reason)

    Logger.debug(fn ->
      "D-Bus address attempt candidate=#{walk.candidate_ordinal} ip=#{walk.ip_ordinal} " <>
        "transport=#{transport} slice_ms=#{timeout} reason=#{safe_reason}"
    end)
  end

  defp attempt_timeout(%Walk{} = walk, attempt_count),
    do: address_attempt_timeout(walk.deadline, attempt_count, walk.impl.clock, walk.last_error)

  defp address_attempt_timeout(deadline, attempt_count, clock, last_error) do
    case remaining_address_list_timeout(deadline, clock) do
      {:ok, remaining} ->
        {:ok, fair_address_attempt_timeout(remaining, attempt_count)}

      {:error, :read_timeout} ->
        {:error, {:address_list_timeout, address_list_timeout_error(last_error)}}
    end
  end

  # Each outstanding attempt gets an equal share of what is left of the shared
  # deadline, raised to a floor so a late attempt is not given a useless slice.
  # The floor is the full 50 ms only when the remaining budget can afford it for
  # every outstanding attempt; otherwise it drops to 1 ms. A slice never exceeds
  # the remaining time.
  #
  # `attempt_count` is what the caller still has to try. Before a TCP entry is
  # resolved that is its unresolved families plus the later connectable
  # entries; afterwards it is the capped resolved IPs plus those entries. The
  # auth-ID lookup counts itself alongside every connectable entry.
  defp fair_address_attempt_timeout(remaining, attempt_count) do
    attempt_count = max(attempt_count, 1)
    fair_share = max(1, div(remaining, attempt_count))

    floor =
      if remaining >= @minimum_address_attempt_timeout * attempt_count,
        do: @minimum_address_attempt_timeout,
        else: 1

    min(remaining, max(floor, fair_share))
  end

  defp final_address_list_error(:read_timeout), do: {:error, {:read_timeout, :read_timeout}}

  defp final_address_list_error({:read_timeout, reason}) do
    {:error, {:read_timeout, safe_address_failure_reason(reason)}}
  end

  defp final_address_list_error(error), do: {:error, error}

  defp address_list_timeout_error(nil), do: :read_timeout

  defp address_list_timeout_error(last_error) do
    {:read_timeout, safe_address_failure_reason(last_error)}
  end

  defp safe_address_failure_reason(reason) when is_atom(reason), do: reason
  defp safe_address_failure_reason({:tcp_resolution_failed, _reason}), do: :tcp_resolution_failed
  defp safe_address_failure_reason(_reason), do: :connection_failed

  defp address_list_timeout(opts) do
    case Keyword.fetch(opts, :read_timeout) do
      {:ok, timeout} when is_integer(timeout) and timeout > 0 ->
        {:ok, timeout}

      {:ok, _timeout} ->
        {:error, :invalid_read_timeout}

      :error ->
        case Keyword.get(opts, :timeout, @default_connection_timeout) do
          timeout when is_integer(timeout) and timeout > 0 -> {:ok, timeout}
          _timeout -> {:error, :invalid_timeout}
        end
    end
  end

  defp remaining_address_list_timeout(deadline, clock) do
    case deadline - clock.monotonic_time() do
      timeout when timeout > 0 -> {:ok, timeout}
      _timeout -> {:error, :read_timeout}
    end
  end

  defp safe_resolver_reason(reason) when is_atom(reason), do: reason
  defp safe_resolver_reason(_reason), do: :resolution_failed

  defp retryable_bus_address_error?(:invalid_timeout), do: false
  defp retryable_bus_address_error?(:invalid_read_timeout), do: false
  defp retryable_bus_address_error?(:invalid_write_timeout), do: false
  defp retryable_bus_address_error?(:invalid_allow_anonymous), do: false
  defp retryable_bus_address_error?(:invalid_bus_option), do: false
  defp retryable_bus_address_error?(:invalid_owner), do: false
  defp retryable_bus_address_error?(:invalid_name), do: false
  # A dead owner is a property of the caller's request, not of the entry that
  # reported it, so the entries after it would fail the same way.
  defp retryable_bus_address_error?(:owner_down), do: false
  defp retryable_bus_address_error?(:auth_id_unavailable), do: false
  defp retryable_bus_address_error?(:auth_cookie_unavailable), do: false
  defp retryable_bus_address_error?(:guid_mismatch), do: false
  defp retryable_bus_address_error?({:read_timeout, _reason}), do: false
  defp retryable_bus_address_error?({:name_taken, _pid}), do: false
  defp retryable_bus_address_error?({:name_registered, _pid}), do: false
  defp retryable_bus_address_error?(_reason), do: true
end
