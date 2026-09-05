defmodule Rebus.AddressList do
  @moduledoc false

  # The D-Bus bus-address list walk. `Rebus.connect/2` parses a `:system` or
  # `:session` address string into candidates and hands them here; this module
  # owns candidate ordering, TCP resolution, the shared setup deadline and its
  # fair-share slicing, failure diagnostics, and the retryable/terminal error
  # classification.

  require Logger

  @default_connection_timeout 5_000
  @max_tcp_addresses_per_family 4
  @minimum_address_attempt_timeout 50

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
          connect_bus_candidates(
            candidates,
            {opts, %{impl: impl, precomputed_auth_id: auth_id}},
            deadline,
            impl,
            nil,
            1
          )

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
        Rebus.Connection.get_auth_id(timeout, impl.identity)
      end
    end
  end

  defp connect_bus_candidates(
         [],
         _conn,
         _deadline,
         _impl,
         nil,
         _candidate_ordinal
       ),
       do: {:error, :unsupported_bus_transport}

  defp connect_bus_candidates(
         [],
         _conn,
         _deadline,
         _impl,
         error,
         _candidate_ordinal
       ),
       do: final_address_list_error(error)

  defp connect_bus_candidates(
         [:unsupported | candidates],
         conn,
         deadline,
         impl,
         last_error,
         candidate_ordinal
       ) do
    connect_bus_candidates(
      candidates,
      conn,
      deadline,
      impl,
      last_error,
      candidate_ordinal + 1
    )
  end

  defp connect_bus_candidates(
         [candidate | candidates],
         conn,
         deadline,
         impl,
         last_error,
         candidate_ordinal
       ) do
    case connect_bus_candidate(
           candidate,
           candidates,
           conn,
           deadline,
           impl,
           last_error,
           candidate_ordinal
         ) do
      {:ok, _pid} = result ->
        result

      {:error, {:address_list_timeout, reason}} ->
        {:error, reason}

      {:error, reason} = error ->
        if retryable_bus_address_error?(reason) do
          connect_bus_candidates(
            candidates,
            conn,
            deadline,
            impl,
            reason,
            candidate_ordinal + 1
          )
        else
          error
        end
    end
  end

  defp connect_bus_candidate(
         {:local, path, expected_guid},
         remaining_candidates,
         conn,
         deadline,
         impl,
         last_error,
         candidate_ordinal
       ) do
    with {:ok, timeout} <-
           address_attempt_timeout(
             deadline,
             1 + connectable_candidate_count(remaining_candidates),
             impl.clock,
             last_error
           ) do
      connect_with_address_diagnostic(
        impl,
        %{family: :local, path: path},
        conn
        |> put_internal(:setup_timeout, timeout)
        |> put_internal(:expected_guid, expected_guid),
        candidate_ordinal,
        0,
        :unix,
        timeout
      )
    end
  end

  defp connect_bus_candidate(
         {:tcp, host, port, family, expected_guid},
         remaining_candidates,
         conn,
         deadline,
         impl,
         last_error,
         candidate_ordinal
       ) do
    with {:ok, addresses} <-
           resolve_tcp_addresses(
             host,
             family,
             deadline,
             impl,
             connectable_candidate_count(remaining_candidates),
             last_error,
             candidate_ordinal
           ) do
      connect_tcp_addresses(
        addresses,
        port,
        expected_guid,
        conn,
        deadline,
        impl,
        connectable_candidate_count(remaining_candidates),
        last_error,
        candidate_ordinal,
        1
      )
    end
  end

  defp resolve_tcp_addresses(
         host,
         family,
         deadline,
         impl,
         remaining_candidate_count,
         last_error,
         candidate_ordinal
       ) do
    families = if family == :unspec, do: [:inet6, :inet], else: [family]

    resolve_tcp_families(
      host,
      families,
      deadline,
      impl,
      remaining_candidate_count,
      [],
      [],
      last_error,
      candidate_ordinal
    )
  end

  # Dialyzer analyses the dev build, where the implementation gate is closed and
  # the resolver is always `Rebus.Resolver.Inet`; from that alone it proves the
  # out-of-contract result clause and the non-atom reason clause unreachable.
  # The behaviour admits any error term, and the test build reaches both.
  @dialyzer {:no_match, [resolve_tcp_families: 9, safe_resolver_reason: 1]}

  defp resolve_tcp_families(
         _host,
         [],
         _deadline,
         _impl,
         _remaining_candidate_count,
         [],
         reasons,
         _last_error,
         _candidate_ordinal
       ),
       do: {:error, {:tcp_resolution_failed, List.first(reasons) || :no_addresses}}

  defp resolve_tcp_families(
         _host,
         [],
         _deadline,
         _impl,
         _remaining_candidate_count,
         addresses,
         _reasons,
         _last_error,
         _candidate_ordinal
       ),
       do: {:ok, addresses}

  defp resolve_tcp_families(
         host,
         [family | families],
         deadline,
         impl,
         remaining_candidate_count,
         addresses,
         reasons,
         last_error,
         candidate_ordinal
       ) do
    with {:ok, timeout} <-
           address_attempt_timeout(
             deadline,
             length([family | families]) + remaining_candidate_count,
             impl.clock,
             last_error
           ) do
      case impl.resolver.getaddrs(host, family, timeout) do
        {:ok, resolved} when is_list(resolved) ->
          resolved_addresses =
            resolved
            |> Enum.take(@max_tcp_addresses_per_family)
            |> Enum.map(&{family, &1})

          resolve_tcp_families(
            host,
            families,
            deadline,
            impl,
            remaining_candidate_count,
            addresses ++ resolved_addresses,
            reasons,
            last_error,
            candidate_ordinal
          )

        {:error, reason} ->
          safe_reason = safe_resolver_reason(reason)

          log_address_attempt(
            candidate_ordinal,
            0,
            :tcp,
            timeout,
            {:tcp_resolution_failed, safe_reason}
          )

          resolve_tcp_families(
            host,
            families,
            deadline,
            impl,
            remaining_candidate_count,
            addresses,
            [safe_reason | reasons],
            {:tcp_resolution_failed, safe_reason},
            candidate_ordinal
          )

        _other ->
          log_address_attempt(candidate_ordinal, 0, :tcp, timeout, :resolution_failed)

          resolve_tcp_families(
            host,
            families,
            deadline,
            impl,
            remaining_candidate_count,
            addresses,
            [:resolution_failed | reasons],
            {:tcp_resolution_failed, :resolution_failed},
            candidate_ordinal
          )
      end
    end
  end

  defp connect_tcp_addresses(
         [],
         _port,
         _expected_guid,
         _conn,
         _deadline,
         _impl,
         _remaining_candidate_count,
         nil,
         _candidate_ordinal,
         _ip_ordinal
       ),
       do: {:error, {:tcp_resolution_failed, :no_addresses}}

  defp connect_tcp_addresses(
         [],
         _port,
         _expected_guid,
         _conn,
         _deadline,
         _impl,
         remaining_candidate_count,
         error,
         _candidate_ordinal,
         _ip_ordinal
       )
       when remaining_candidate_count > 0,
       do: {:error, error}

  defp connect_tcp_addresses(
         [],
         _port,
         _expected_guid,
         _conn,
         _deadline,
         _impl,
         _remaining_candidate_count,
         error,
         _candidate_ordinal,
         _ip_ordinal
       ),
       do: final_address_list_error(error)

  defp connect_tcp_addresses(
         [{family, address} | addresses],
         port,
         expected_guid,
         conn,
         deadline,
         impl,
         remaining_candidate_count,
         last_error,
         candidate_ordinal,
         ip_ordinal
       ) do
    with {:ok, timeout} <-
           address_attempt_timeout(
             deadline,
             length(addresses) + 1 + remaining_candidate_count,
             impl.clock,
             last_error
           ) do
      case connect_with_address_diagnostic(
             impl,
             %{family: family, addr: address, port: port},
             conn
             |> put_internal(:setup_timeout, timeout)
             |> put_internal(:expected_guid, expected_guid),
             candidate_ordinal,
             ip_ordinal,
             :tcp,
             timeout
           ) do
        {:ok, _pid} = result ->
          result

        {:error, reason} = error ->
          if retryable_bus_address_error?(reason) do
            connect_tcp_addresses(
              addresses,
              port,
              expected_guid,
              conn,
              deadline,
              impl,
              remaining_candidate_count,
              reason,
              candidate_ordinal,
              ip_ordinal + 1
            )
          else
            error
          end
      end
    end
  end

  # The internal arguments a candidate connection is started with never travel
  # in the caller's option list.
  defp put_internal({conn_opts, internal}, key, value),
    do: {conn_opts, Map.put(internal, key, value)}

  defp connectable_candidate_count(candidates) do
    Enum.count(candidates, &connectable_candidate?/1)
  end

  defp connectable_candidate?({:local, _path, _expected_guid}), do: true
  defp connectable_candidate?({:tcp, _host, _port, _family, _expected_guid}), do: true
  defp connectable_candidate?(_candidate), do: false

  defp connect_with_address_diagnostic(
         impl,
         address,
         conn,
         candidate_ordinal,
         ip_ordinal,
         transport,
         timeout
       ) do
    case impl.connector.connect(address, conn) do
      {:error, reason} = error ->
        log_address_attempt(candidate_ordinal, ip_ordinal, transport, timeout, reason)
        error

      result ->
        result
    end
  end

  defp log_address_attempt(candidate_ordinal, ip_ordinal, transport, timeout, reason) do
    safe_reason = safe_address_failure_reason(reason)

    Logger.debug(fn ->
      "D-Bus address attempt candidate=#{candidate_ordinal} ip=#{ip_ordinal} " <>
        "transport=#{transport} slice_ms=#{timeout} reason=#{safe_reason}"
    end)
  end

  defp address_attempt_timeout(deadline, attempt_count, clock, last_error) do
    case remaining_address_list_timeout(deadline, clock) do
      {:ok, remaining} ->
        {:ok, fair_address_attempt_timeout(remaining, attempt_count)}

      {:error, :read_timeout} ->
        {:error, {:address_list_timeout, address_list_timeout_error(last_error)}}
    end
  end

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
  defp retryable_bus_address_error?(:invalid_name), do: false
  defp retryable_bus_address_error?(:auth_id_unavailable), do: false
  defp retryable_bus_address_error?(:auth_cookie_unavailable), do: false
  defp retryable_bus_address_error?(:guid_mismatch), do: false
  defp retryable_bus_address_error?({:read_timeout, _reason}), do: false
  defp retryable_bus_address_error?({:name_taken, _pid}), do: false
  defp retryable_bus_address_error?({:name_registered, _pid}), do: false
  defp retryable_bus_address_error?(_reason), do: true
end
