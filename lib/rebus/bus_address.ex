defmodule Rebus.BusAddress do
  @moduledoc """
  Parses the D-Bus address-list syntax used by session and system buses.

  `parse/1` accepts a bounded, semicolon-separated list of
  `transport:key=value` entries. It percent-decodes values, but never converts
  input into atoms or returns input in an error. The supported transports are
  `unix` (`path` and `abstract`) and `tcp` (`host`, `port`, and optional
  `family`). A valid `guid` is retained as the expected server identity while
  being ignored for socket selection. Other syntactically valid, unrecognised
  parameters are deliberately discarded: they may be meaningful to a newer
  implementation, but do not change a socket address Rebus supports.

  A syntactically valid unsupported transport is represented by `:unsupported`
  so callers can continue to a later address. Malformed input returns
  `{:error, {:invalid_bus_address, reason}}`, where `reason` is a fixed atom;
  no supplied address data is retained in that result.
  """

  @type socket_family :: :inet | :inet6 | :unspec
  @type expected_guid :: binary() | nil
  @type candidate ::
          {:local, binary(), expected_guid()}
          | {:tcp, binary(), :inet.port_number(), socket_family(), expected_guid()}
          | :unsupported
  @type error_reason ::
          :not_binary
          | :too_long
          | :too_many_addresses
          | :empty_entry
          | :invalid_transport
          | :invalid_entry
          | :too_many_parameters
          | :invalid_key
          | :duplicate_key
          | :invalid_escape
          | :nul_byte
          | :ambiguous_unix_address
          | :missing_path
          | :missing_host
          | :missing_port
          | :invalid_port
          | :invalid_family
          | :invalid_guid

  @max_address_length 4_096
  @max_addresses 16
  @max_parameters 16
  @max_value_length 1_024

  @doc """
  Parses a D-Bus address list without doing DNS or socket I/O.

  The list is bounded to #{@max_address_length} bytes, #{@max_addresses}
  entries, #{@max_parameters} parameters per entry, and #{@max_value_length}
  source bytes per value. Values accept every literal non-NUL, non-percent byte
  that does not act as an address separator; percent escapes remain exact, and
  a decoded NUL is rejected. A single trailing semicolon is accepted, as
  libdbus does; leading, doubled, and otherwise empty entries are rejected. An
  abstract socket is encoded with one leading NUL added by the `abstract`
  transport itself.
  """
  @spec parse(term()) :: {:ok, [candidate()]} | {:error, {:invalid_bus_address, error_reason()}}
  def parse(address) when is_binary(address) do
    if byte_size(address) > @max_address_length do
      error(:too_long)
    else
      address
      |> :binary.split(";", [:global])
      |> drop_trailing_empty_entry()
      |> parse_entries()
    end
  end

  def parse(_address), do: error(:not_binary)

  # Percent-escapes a value for an address entry Rebus builds itself, such as
  # the `$XDG_RUNTIME_DIR/bus` session fallback. Every byte outside the
  # optionally-escaped set `[-0-9A-Za-z_/.\\]` becomes `%XX`, which is what
  # libdbus escapes, so a directory holding `%`, `;`, `,` or `=` still parses.
  @doc false
  @spec escape_value(binary()) :: binary()
  def escape_value(value) when is_binary(value) do
    for <<byte <- value>>, into: "", do: escape_byte(byte)
  end

  defp escape_byte(byte)
       when byte in ?0..?9 or byte in ?A..?Z or byte in ?a..?z or byte in [?-, ?_, ?/, ?., ?\\],
       do: <<byte>>

  defp escape_byte(byte), do: "%" <> Base.encode16(<<byte>>, case: :upper)

  defp drop_trailing_empty_entry(entries) do
    case Enum.reverse(entries) do
      [<<>> | reversed_entries] when reversed_entries != [] -> Enum.reverse(reversed_entries)
      _ -> entries
    end
  end

  defp parse_entries(entries) when length(entries) > @max_addresses,
    do: error(:too_many_addresses)

  defp parse_entries(entries) do
    entries
    |> Enum.reduce_while({:ok, []}, fn entry, {:ok, candidates} ->
      case parse_entry(entry) do
        {:ok, candidate} -> {:cont, {:ok, [candidate | candidates]}}
        {:error, _reason} = error -> {:halt, error}
      end
    end)
    |> reverse_candidates()
  end

  defp reverse_candidates({:ok, candidates}), do: {:ok, Enum.reverse(candidates)}
  defp reverse_candidates(error), do: error

  defp parse_entry(<<>>), do: error(:empty_entry)

  defp parse_entry(entry) do
    case :binary.match(entry, ":") do
      {separator, 1} when separator > 0 ->
        transport = binary_part(entry, 0, separator)
        parameters_size = byte_size(entry) - separator - 1
        parameters = binary_part(entry, separator + 1, parameters_size)

        with :ok <- validate_token(transport, :invalid_transport),
             {:ok, pairs} <- parse_parameters(parameters) do
          build_candidate(transport, pairs)
        end

      _ ->
        error(:invalid_entry)
    end
  end

  defp parse_parameters(<<>>), do: {:ok, %{}}

  defp parse_parameters(parameters) do
    entries = :binary.split(parameters, ",", [:global])

    if length(entries) > @max_parameters do
      error(:too_many_parameters)
    else
      Enum.reduce_while(entries, {:ok, %{}}, &put_parsed_parameter/2)
    end
  end

  defp put_parsed_parameter(parameter, {:ok, pairs}) do
    case parse_parameter(parameter, pairs) do
      {:ok, parsed} -> {:cont, {:ok, parsed}}
      {:error, _reason} = error -> {:halt, error}
    end
  end

  defp parse_parameter(<<>>, _pairs), do: error(:invalid_entry)

  defp parse_parameter(parameter, pairs) do
    case :binary.match(parameter, "=") do
      {separator, 1} when separator > 0 ->
        key = binary_part(parameter, 0, separator)
        value_size = byte_size(parameter) - separator - 1
        value = binary_part(parameter, separator + 1, value_size)

        put_parameter(pairs, key, value)

      _ ->
        error(:invalid_entry)
    end
  end

  defp put_parameter(pairs, key, value) do
    cond do
      byte_size(value) > @max_value_length ->
        error(:too_long)

      Map.has_key?(pairs, key) ->
        error(:duplicate_key)

      true ->
        with :ok <- validate_token(key, :invalid_key),
             {:ok, decoded} <- decode_value(value) do
          {:ok, Map.put(pairs, key, decoded)}
        end
    end
  end

  defp build_candidate("unix", pairs) do
    with {:ok, expected_guid} <- parse_expected_guid(pairs) do
      case {Map.fetch(pairs, "path"), Map.fetch(pairs, "abstract")} do
        {{:ok, path}, :error} when byte_size(path) > 0 ->
          {:ok, {:local, path, expected_guid}}

        {:error, {:ok, abstract}} when byte_size(abstract) > 0 ->
          {:ok, {:local, <<0, abstract::binary>>, expected_guid}}

        {{:ok, _path}, {:ok, _abstract}} ->
          error(:ambiguous_unix_address)

        {{:ok, <<>>}, :error} ->
          error(:missing_path)

        {:error, {:ok, <<>>}} ->
          error(:missing_path)

        {:error, :error} when map_size(pairs) == 0 ->
          error(:missing_path)

        {:error, :error} when is_map_key(pairs, "guid") ->
          error(:missing_path)

        _ ->
          {:ok, :unsupported}
      end
    end
  end

  defp build_candidate("tcp", pairs) do
    with {:ok, expected_guid} <- parse_expected_guid(pairs),
         {:ok, host} <- required_value(pairs, "host", :missing_host),
         {:ok, port} <- parse_port(Map.get(pairs, "port")),
         {:ok, family} <- parse_family(Map.get(pairs, "family")) do
      {:ok, {:tcp, host, port, family, expected_guid}}
    end
  end

  defp build_candidate(_transport, _pairs), do: {:ok, :unsupported}

  defp required_value(pairs, key, missing_reason) do
    case Map.get(pairs, key) do
      value when is_binary(value) and byte_size(value) > 0 -> {:ok, value}
      _ -> error(missing_reason)
    end
  end

  defp parse_port(nil), do: error(:missing_port)

  defp parse_port(value) when is_binary(value) do
    case Integer.parse(value) do
      {port, ""} when port in 1..65_535 -> {:ok, port}
      _ -> error(:invalid_port)
    end
  end

  defp parse_family(nil), do: {:ok, :unspec}
  defp parse_family("ipv4"), do: {:ok, :inet}
  defp parse_family("ipv6"), do: {:ok, :inet6}
  defp parse_family(_family), do: error(:invalid_family)

  defp parse_expected_guid(pairs) do
    case Map.fetch(pairs, "guid") do
      :error ->
        {:ok, nil}

      {:ok, guid} when byte_size(guid) == 32 ->
        if hex_guid?(guid), do: {:ok, :binary.copy(guid)}, else: error(:invalid_guid)

      {:ok, _guid} ->
        error(:invalid_guid)
    end
  end

  defp hex_guid?(<<>>), do: true

  defp hex_guid?(<<byte, rest::binary>>) when byte in ?0..?9 or byte in ?a..?f or byte in ?A..?F,
    do: hex_guid?(rest)

  defp hex_guid?(_guid), do: false

  defp decode_value(value), do: decode_value(value, [])

  defp decode_value(<<>>, bytes), do: {:ok, IO.iodata_to_binary(Enum.reverse(bytes))}

  defp decode_value(<<"%", high, low, rest::binary>>, bytes) do
    case hex_byte(high, low) do
      {:ok, 0} -> error(:nul_byte)
      {:ok, byte} -> decode_value(rest, [byte | bytes])
      :error -> error(:invalid_escape)
    end
  end

  defp decode_value(<<"%", _rest::binary>>, _bytes), do: error(:invalid_escape)

  defp decode_value(<<0, _rest::binary>>, _bytes), do: error(:nul_byte)

  defp decode_value(<<byte, rest::binary>>, bytes),
    do: decode_value(rest, [byte | bytes])

  defp hex_byte(high, low) do
    with {:ok, high} <- hex_digit(high),
         {:ok, low} <- hex_digit(low) do
      {:ok, high * 16 + low}
    else
      :error -> :error
    end
  end

  defp hex_digit(digit) when digit in ?0..?9, do: {:ok, digit - ?0}
  defp hex_digit(digit) when digit in ?a..?f, do: {:ok, digit - ?a + 10}
  defp hex_digit(digit) when digit in ?A..?F, do: {:ok, digit - ?A + 10}
  defp hex_digit(_digit), do: :error

  defp validate_token(<<>>, reason), do: error(reason)

  defp validate_token(token, reason) do
    if token?(token), do: :ok, else: error(reason)
  end

  defp token?(token) do
    for <<byte <- token>>, reduce: true do
      true when byte in ?a..?z or byte in ?A..?Z or byte in ?0..?9 or byte in [?-, ?_] -> true
      _ -> false
    end
  end

  defp error(reason), do: {:error, {:invalid_bus_address, reason}}
end
