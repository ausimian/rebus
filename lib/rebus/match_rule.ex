defmodule Rebus.MatchRule do
  @moduledoc """
  A validated, canonical D-Bus signal match rule.

  Construct rules with `new/1`, then pass them to `Rebus.add_match/3`.
  Rebus deliberately accepts structured criteria instead of raw rule strings,
  so every outbound rule is bounded, correctly quoted, and safe to use for
  client-side filtering when several subscriptions share a connection. Unique
  sender names are locally checked; well-known sender names remain bus-owned
  for broadcast signals. A directed signal matches a well-known sender rule
  only when its sender header names that sender exactly.

  The generated rule always contains `type='signal'`. Supported criteria are
  `:sender`, `:interface`, `:member`, `:path`, `:path_namespace`,
  `:destination`, `:args`, `:arg_paths`, and `:arg0namespace`. `:path` and
  `:path_namespace` are mutually exclusive. `:args` and `:arg_paths` are maps
  or keyword lists keyed by indexes 0 through 63.

  `eavesdrop` is intentionally not accepted. It changes bus policy and routing
  semantics that cannot be safely inferred from an inbound signal; callers who
  need monitoring should use a dedicated monitoring connection instead.
  """

  alias Rebus.Message
  alias Rebus.Signature
  alias Rebus.WireValue

  @max_length 1_024
  @max_argument 63

  @type criteria :: %{
          optional(:sender) => binary(),
          optional(:interface) => binary(),
          optional(:member) => binary(),
          optional(:path) => binary(),
          optional(:path_namespace) => binary(),
          optional(:destination) => binary(),
          optional(:args) => %{non_neg_integer() => binary()},
          optional(:arg_paths) => %{non_neg_integer() => binary()},
          optional(:arg0namespace) => binary()
        }

  @enforce_keys [:criteria, :string]
  defstruct [:criteria, :string]

  @type t :: %__MODULE__{criteria: criteria(), string: binary()}

  @doc """
  Builds a safe signal match rule.

  The rule is limited to #{@max_length} bytes, matching the reference D-Bus
  implementation's match-rule limit. Invalid input returns a stable atom and
  never includes caller-provided rule text.

  ## Examples

      iex> Rebus.MatchRule.new(sender: "org.freedesktop.DBus", member: "NameOwnerChanged")
      {:ok, %Rebus.MatchRule{}}
  """
  @spec new(keyword()) :: {:ok, t()} | {:error, validation_error()}
  def new(opts) when is_list(opts) do
    with :ok <- validate_options(opts),
         {:ok, criteria} <- build_criteria(opts),
         string = encode(criteria),
         :ok <- validate_length(string) do
      {:ok, %__MODULE__{criteria: criteria, string: string}}
    end
  end

  def new(_opts), do: {:error, :invalid_match_rule}

  @doc """
  Builds a match rule, raising `ArgumentError` when it is invalid.
  """
  @spec new!(keyword()) :: t()
  def new!(opts) do
    case new(opts) do
      {:ok, rule} -> rule
      {:error, reason} -> raise ArgumentError, "invalid D-Bus match rule: #{reason}"
    end
  end

  @doc """
  Returns the canonical D-Bus representation used for both AddMatch and
  RemoveMatch.
  """
  @spec to_string(t()) :: binary()
  def to_string(%__MODULE__{string: string}), do: string

  @doc """
  Returns whether an inbound signal matches the criteria that Rebus can safely
  evaluate after the bus has routed it.

  Rebus compares unique-name `:sender` values, `:interface`, `:member`, `:path`, `:destination`,
  `:path_namespace`, `:args`, `:arg_paths`, and `:arg0namespace`. It
  compares unique-name `:sender` values. A well-known sender remains bus-owned
  for broadcast signals because the bus may forward it under the current unique
  owner. A directed signal bypasses bus match routing, so Rebus accepts it for
  a well-known sender only when its sender header equals that well-known name;
  this preserves bus-driver signals while rejecting a peer's unique sender.
  Subscription setup rejects an overlapping rule that would make that
  well-known sender ambiguous locally. This does not emulate bus access policy
  or eavesdropping.
  """
  @spec matches?(t(), Message.t()) :: boolean()
  def matches?(%__MODULE__{criteria: criteria}, %Message{type: :signal} = message) do
    headers = message.header_fields

    well_known_sender_matches?(criteria, headers) and
      header_matches?(criteria, headers) and
      argument_matches?(criteria, message)
  end

  def matches?(_rule, _message), do: false

  @type validation_error ::
          :duplicate_match_option
          | :invalid_match_rule
          | :invalid_match_option
          | :invalid_match_value
          | :invalid_match_argument
          | :conflicting_match_paths
          | :match_rule_too_long

  defp validate_options(opts) do
    if Enum.all?(opts, fn
         {key, _value} when is_atom(key) -> true
         _entry -> false
       end) do
      keys = Keyword.keys(opts)

      cond do
        keys != Enum.uniq(keys) ->
          {:error, :duplicate_match_option}

        Enum.any?(keys, &(&1 not in valid_options())) ->
          {:error, :invalid_match_option}

        Keyword.get(opts, :type, :signal) != :signal ->
          {:error, :invalid_match_value}

        Keyword.has_key?(opts, :path) and Keyword.has_key?(opts, :path_namespace) ->
          {:error, :conflicting_match_paths}

        true ->
          :ok
      end
    else
      {:error, :invalid_match_rule}
    end
  end

  defp valid_options do
    [
      :type,
      :sender,
      :interface,
      :member,
      :path,
      :path_namespace,
      :destination,
      :args,
      :arg_paths,
      :arg0namespace
    ]
  end

  defp build_criteria(opts) do
    with {:ok, criteria} <- put_if_valid(%{}, :sender, opts, &valid_bus_name?/1),
         {:ok, criteria} <- put_if_valid(criteria, :interface, opts, &valid_interface_name?/1),
         {:ok, criteria} <- put_if_valid(criteria, :member, opts, &valid_member_name?/1),
         {:ok, criteria} <- put_if_valid(criteria, :path, opts, &WireValue.valid_object_path?/1),
         {:ok, criteria} <-
           put_if_valid(criteria, :path_namespace, opts, &WireValue.valid_object_path?/1),
         {:ok, criteria} <- put_if_valid(criteria, :destination, opts, &valid_unique_name?/1),
         {:ok, criteria} <- put_arguments(criteria, :args, opts),
         {:ok, criteria} <- put_arguments(criteria, :arg_paths, opts),
         {:ok, criteria} <- put_if_valid(criteria, :arg0namespace, opts, &valid_namespace?/1) do
      {:ok, criteria}
    end
  end

  defp put_if_valid(criteria, key, opts, validator) do
    case Keyword.fetch(opts, key) do
      :error ->
        {:ok, criteria}

      {:ok, value} ->
        if(validator.(value),
          do: {:ok, Map.put(criteria, key, value)},
          else: {:error, :invalid_match_value}
        )
    end
  end

  defp put_arguments(criteria, key, opts) do
    case Keyword.fetch(opts, key) do
      :error ->
        {:ok, criteria}

      {:ok, values} ->
        with {:ok, values} <- normalize_arguments(values),
             true <- Enum.all?(values, fn {_index, value} -> WireValue.valid_string?(value) end) do
          {:ok, Map.put(criteria, key, Map.new(values))}
        else
          false -> {:error, :invalid_match_value}
          {:error, _} = error -> error
        end
    end
  end

  defp normalize_arguments(values) when is_map(values),
    do: normalize_arguments(Map.to_list(values))

  defp normalize_arguments(values) when is_list(values) do
    cond do
      Enum.map(values, fn
        {index, _value} -> index
        _value -> nil
      end)
      |> then(&(&1 != Enum.uniq(&1))) ->
        {:error, :duplicate_match_option}

      Enum.all?(values, fn
        {index, _value} when is_integer(index) and index in 0..@max_argument -> true
        _ -> false
      end) ->
        {:ok, values}

      true ->
        {:error, :invalid_match_argument}
    end
  end

  defp normalize_arguments(_values), do: {:error, :invalid_match_argument}

  defp encode(criteria) do
    ["type='signal'" | encode_criteria(criteria)]
    |> Enum.join(",")
  end

  defp encode_criteria(criteria) do
    fixed =
      for key <- [
            :sender,
            :interface,
            :member,
            :path,
            :path_namespace,
            :destination,
            :arg0namespace
          ],
          value = Map.get(criteria, key),
          not is_nil(value) do
        "#{key}='#{escape(value)}'"
      end

    args = encode_arguments("arg", Map.get(criteria, :args, %{}), "")
    arg_paths = encode_arguments("arg", Map.get(criteria, :arg_paths, %{}), "path")
    fixed ++ args ++ arg_paths
  end

  defp encode_arguments(prefix, values, suffix) do
    values
    |> Enum.sort_by(fn {index, _value} -> index end)
    |> Enum.map(fn {index, value} -> "#{prefix}#{index}#{suffix}='#{escape(value)}'" end)
  end

  # A literal apostrophe ends a quoted section in the D-Bus grammar. Re-open a
  # new quoted section after the grammar's unquoted `\'` escape to preserve it.
  defp escape(value), do: String.replace(value, "'", "'\\''")

  defp validate_length(string) when byte_size(string) <= @max_length, do: :ok
  defp validate_length(_string), do: {:error, :match_rule_too_long}

  defp header_matches?(criteria, headers) do
    Enum.all?(
      [
        {:sender, :sender},
        {:interface, :interface},
        {:member, :member},
        {:path, :path},
        {:destination, :destination}
      ],
      fn {criterion, header} ->
        case Map.fetch(criteria, criterion) do
          :error ->
            true

          {:ok, <<":", _::binary>> = value} when criterion == :sender ->
            Map.get(headers, header) == value

          {:ok, _value} when criterion == :sender ->
            true

          {:ok, value} ->
            Map.get(headers, header) == value
        end
      end
    ) and path_namespace_matches?(criteria, headers)
  end

  # A well-known sender is safely enforced by bus routing only for broadcast
  # signals. Directed signals bypass that routing, so they need an exact sender
  # header match. That permits trusted bus-driver signals without treating a
  # peer's unique sender as the named service.
  defp well_known_sender_matches?(%{sender: <<":", _::binary>>}, _headers), do: true

  defp well_known_sender_matches?(%{sender: sender}, %{destination: _destination} = headers),
    do: Map.get(headers, :sender) == sender

  defp well_known_sender_matches?(%{sender: _sender}, _headers), do: true

  defp well_known_sender_matches?(_criteria, _headers), do: true

  defp path_namespace_matches?(criteria, headers) do
    case Map.fetch(criteria, :path_namespace) do
      :error -> true
      {:ok, namespace} -> path_in_namespace?(Map.get(headers, :path), namespace)
    end
  end

  defp argument_matches?(criteria, %Message{} = message) do
    case Signature.parse(Message.signature(message)) do
      {:ok, types} ->
        exact_argument_matches?(Map.get(criteria, :args, %{}), message.body, types) and
          path_argument_matches?(Map.get(criteria, :arg_paths, %{}), message.body, types) and
          arg0namespace_matches?(Map.get(criteria, :arg0namespace), message.body, types)

      _ ->
        false
    end
  end

  defp exact_argument_matches?(criteria, body, types) do
    Enum.all?(criteria, fn {index, value} ->
      Enum.at(types, index) == {:string, nil} and Enum.at(body, index) == value
    end)
  end

  defp path_argument_matches?(criteria, body, types) do
    Enum.all?(criteria, fn {index, value} ->
      Enum.at(types, index) in [{:string, nil}, {:object_path, nil}] and
        path_argument_matches?(Enum.at(body, index), value)
    end)
  end

  defp path_argument_matches?(argument, value) when is_binary(argument) do
    argument == value or
      (String.ends_with?(argument, "/") and String.starts_with?(value, argument)) or
      (String.ends_with?(value, "/") and String.starts_with?(argument, value))
  end

  defp path_argument_matches?(_argument, _value), do: false

  defp arg0namespace_matches?(nil, _body, _types), do: true

  defp arg0namespace_matches?(namespace, [argument | _body], [{:string, nil} | _types])
       when is_binary(argument) do
    argument == namespace or String.starts_with?(argument, namespace <> ".")
  end

  defp arg0namespace_matches?(_namespace, _body, _types), do: false

  defp path_in_namespace?(path, namespace) when is_binary(path) do
    path == namespace or
      (namespace == "/" and String.starts_with?(path, "/")) or
      String.starts_with?(path, namespace <> "/")
  end

  defp path_in_namespace?(_path, _namespace), do: false

  defp valid_interface_name?(name) when is_binary(name) and byte_size(name) <= 255 do
    WireValue.valid_string?(name) and
      name
      |> String.split(".")
      |> then(fn parts -> length(parts) >= 2 and Enum.all?(parts, &valid_name_element?/1) end)
  end

  defp valid_interface_name?(_name), do: false

  defp valid_member_name?(name) when is_binary(name) and byte_size(name) <= 255,
    do: WireValue.valid_string?(name) and valid_name_element?(name)

  defp valid_member_name?(_name), do: false

  defp valid_bus_name?(name) when is_binary(name) and byte_size(name) <= 255 do
    WireValue.valid_string?(name) and
      if String.starts_with?(name, ":") do
        String.match?(name, ~r/\A:[A-Za-z0-9._-]+\z/)
      else
        name
        |> String.split(".")
        |> then(fn parts ->
          length(parts) >= 2 and Enum.all?(parts, &valid_bus_name_element?/1)
        end)
      end
  end

  defp valid_bus_name?(_name), do: false

  defp valid_unique_name?(name) when is_binary(name) and byte_size(name) <= 255,
    do: WireValue.valid_string?(name) and String.match?(name, ~r/\A:[A-Za-z0-9._-]+\z/)

  defp valid_unique_name?(_name), do: false

  defp valid_namespace?(name) when is_binary(name) and byte_size(name) <= 255 do
    WireValue.valid_string?(name) and
      name
      |> String.split(".")
      |> Enum.all?(&valid_bus_name_element?/1)
  end

  defp valid_namespace?(_name), do: false

  defp valid_bus_name_element?(element),
    do: String.match?(element, ~r/\A[A-Za-z_-][A-Za-z0-9_-]*\z/)

  defp valid_name_element?(element), do: String.match?(element, ~r/\A[A-Za-z_][A-Za-z0-9_]*\z/)
end
