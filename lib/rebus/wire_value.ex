defmodule Rebus.WireValue do
  @moduledoc false

  alias Rebus.Signature

  @spec valid_string?(term()) :: boolean()
  def valid_string?(value),
    do: is_binary(value) and String.valid?(value) and :binary.match(value, <<0>>) == :nomatch

  @spec valid_object_path?(term()) :: boolean()
  def valid_object_path?("/"), do: true

  def valid_object_path?(path) when is_binary(path) do
    valid_string?(path) and String.match?(path, ~r{\A/[A-Za-z0-9_/]*\z}) and
      not String.ends_with?(path, "/") and not String.contains?(path, "//")
  end

  def valid_object_path?(_), do: false

  @spec valid_signature?(term()) :: boolean()
  def valid_signature?(value),
    do: valid_string?(value) and match?({:ok, _}, Signature.parse(value))

  # D-Bus specification, "Valid Names": "The maximum length of a name is 255
  # characters." Every name grammar below is ASCII-only, so a name that
  # satisfies one of them is necessarily valid UTF-8 with no NUL byte; the
  # element scanners subsume `valid_string?/1` rather than calling it.
  @max_name_length 255

  # Interface names: "Composed of 2 or more elements separated by a period
  # ('.') character. All elements must contain at least one character. Each
  # element must only contain the ASCII characters "[A-Z][a-z][0-9]_" and must
  # not begin with a digit. Must not exceed the maximum name length."
  @spec valid_interface_name?(term()) :: boolean()
  def valid_interface_name?(name) when is_binary(name) and byte_size(name) <= @max_name_length,
    do: valid_elements?(name, 2, &valid_name_element?/1)

  def valid_interface_name?(_name), do: false

  # Error names: "Error names have the same restrictions as interface names."
  @spec valid_error_name?(term()) :: boolean()
  def valid_error_name?(name), do: valid_interface_name?(name)

  # Member names: "Must only contain the ASCII characters "[A-Z][a-z][0-9]_"
  # and may not begin with a digit. Must not contain the '.' (period)
  # character. Must not exceed the maximum name length. Must be at least 1
  # byte in length." The element grammar excludes '.' already.
  @spec valid_member_name?(term()) :: boolean()
  def valid_member_name?(name) when is_binary(name) and byte_size(name) <= @max_name_length,
    do: valid_name_element?(name)

  def valid_member_name?(_name), do: false

  # Well-known bus names: bus names not starting with ':'. "Bus names are
  # composed of 1 or more elements separated by a period ('.') character. All
  # elements must contain at least one character. Each element must only
  # contain the ASCII characters "[A-Z][a-z][0-9]_-" [...] Only elements that
  # are part of a unique connection name may begin with a digit [...] Bus
  # names must contain at least one '.' (period) character (and thus at least
  # two elements). Bus names must not begin with a '.' (period) character.
  # Bus names must not exceed the maximum name length."
  @spec valid_well_known_name?(term()) :: boolean()
  def valid_well_known_name?(<<?:, _rest::binary>>), do: false

  def valid_well_known_name?(name) when is_binary(name) and byte_size(name) <= @max_name_length,
    do: valid_elements?(name, 2, &valid_bus_name_element?/1)

  def valid_well_known_name?(_name), do: false

  # Unique connection names: "Bus names that start with a colon (':')
  # character are unique connection names." The same element rules apply, but
  # elements of a unique name may begin with a digit. The leading colon counts
  # towards the maximum name length.
  @spec valid_unique_name?(term()) :: boolean()
  def valid_unique_name?(<<?:, rest::binary>> = name) when byte_size(name) <= @max_name_length,
    do: valid_elements?(rest, 2, &valid_unique_name_element?/1)

  def valid_unique_name?(_name), do: false

  # Bus names are either unique connection names or well-known names.
  @spec valid_bus_name?(term()) :: boolean()
  def valid_bus_name?(name), do: valid_unique_name?(name) or valid_well_known_name?(name)

  # `arg0namespace` match keys hold a well-known bus name prefix, so a single
  # element (e.g. "org") is legal, but a unique name is not a namespace.
  @spec valid_namespace?(term()) :: boolean()
  def valid_namespace?(<<?:, _rest::binary>>), do: false

  def valid_namespace?(name) when is_binary(name) and byte_size(name) <= @max_name_length,
    do: valid_elements?(name, 1, &valid_bus_name_element?/1)

  def valid_namespace?(_name), do: false

  defp valid_elements?(name, min_elements, element?) do
    parts = :binary.split(name, ".", [:global])

    length(parts) >= min_elements and Enum.all?(parts, element?)
  end

  # "[A-Z][a-z][0-9]_", not beginning with a digit.
  defp valid_name_element?(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char == ?_,
       do: valid_name_tail?(rest)

  defp valid_name_element?(_element), do: false

  defp valid_name_tail?(<<>>), do: true

  defp valid_name_tail?(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char == ?_,
       do: valid_name_tail?(rest)

  defp valid_name_tail?(_rest), do: false

  # "[A-Z][a-z][0-9]_-", not beginning with a digit.
  defp valid_bus_name_element?(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char == ?_ or char == ?-,
       do: valid_unique_name_tail?(rest)

  defp valid_bus_name_element?(_element), do: false

  # "[A-Z][a-z][0-9]_-", digits allowed in any position.
  defp valid_unique_name_element?(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char == ?_ or char == ?-,
       do: valid_unique_name_tail?(rest)

  defp valid_unique_name_element?(_element), do: false

  defp valid_unique_name_tail?(<<>>), do: true

  defp valid_unique_name_tail?(<<char, rest::binary>>)
       when char in ?A..?Z or char in ?a..?z or char in ?0..?9 or char == ?_ or char == ?-,
       do: valid_unique_name_tail?(rest)

  defp valid_unique_name_tail?(_rest), do: false

  @spec validate!(atom(), term()) :: :ok
  def validate!(:string, value) do
    if valid_string?(value), do: :ok, else: raise(ArgumentError, "invalid D-Bus string")
  end

  def validate!(:object_path, value) do
    if valid_object_path?(value), do: :ok, else: raise(ArgumentError, "invalid D-Bus object path")
  end

  def validate!(:signature, value) do
    if valid_string?(value) do
      case Signature.parse(value) do
        {:ok, _types} -> :ok
        {:error, :resource_limit} -> raise Rebus.ResourceLimitError, limit: :nesting
        {:error, :invalid_signature} -> raise ArgumentError, "invalid D-Bus signature"
      end
    else
      raise ArgumentError, "invalid D-Bus signature"
    end
  end
end
