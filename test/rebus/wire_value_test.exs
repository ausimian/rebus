defmodule Rebus.WireValueTest do
  use ExUnit.Case, async: true

  alias Rebus.WireValue

  # Boundary fixtures for the D-Bus 255-character maximum name length.
  @dotted_255 "a." <> String.duplicate("b", 253)
  @dotted_256 "a." <> String.duplicate("b", 254)
  @element_255 String.duplicate("m", 255)
  @element_256 String.duplicate("m", 256)
  @unique_255 ":1." <> String.duplicate("0", 252)
  @unique_256 ":1." <> String.duplicate("0", 253)

  @invalid_utf8 <<"org.example.", 0xFF>>
  @embedded_nul <<"org.example.", 0, "Name">>

  @non_binaries [
    {"nil", nil},
    {"an atom", :"org.example.Name"},
    {"an integer", 42},
    {"a charlist", ~c"org.example.Name"},
    {"a tuple", {"org.example.Name"}},
    {"an iolist", ["org.", "example.Name"]}
  ]

  describe "valid_interface_name?/1" do
    # D-Bus specification, "Valid Names": interface names are composed of two
    # or more elements separated by '.'; each element holds only
    # "[A-Z][a-z][0-9]_" and must not begin with a digit; 255 bytes maximum.
    cases = [
      {"the spec's example name", "org.freedesktop.DBus", true},
      {"a nested example name", "org.freedesktop.DBus.Introspectable", true},
      {"the minimum valid name", "a.b", true},
      {"underscore-led elements", "_org._example._Name", true},
      {"digits after the first character", "org.e2.N4me", true},
      {"a single element", "Foo", false},
      {"a single element with a trailing dot", "Foo.", false},
      {"a leading-digit element", "1.foo", false},
      {"a leading-digit trailing element", "foo.1bar", false},
      {"a hyphenated element", "org.example-service.Name", false},
      {"an empty inner element", "a..b", false},
      {"a leading dot", ".a.b", false},
      {"a trailing dot", "a.b.", false},
      {"only a dot", ".", false},
      {"an empty string", "", false},
      {"a name with spaces", "invalid interface name", false},
      {"a colon-prefixed name", ":1.0", false},
      {"exactly 255 bytes", @dotted_255, true},
      {"256 bytes", @dotted_256, false},
      {"invalid UTF-8", @invalid_utf8, false},
      {"an embedded NUL", @embedded_nul, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_interface_name?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_interface_name?(input), description
      end
    end
  end

  describe "valid_error_name?/1" do
    # D-Bus specification, "Valid Names": "Error names have the same
    # restrictions as interface names."
    cases = [
      {"the spec's example name", "org.freedesktop.DBus.Error.Failed", true},
      {"the minimum valid name", "a.b", true},
      {"a single element", "Failed", false},
      {"a leading-digit element", "1.Failed", false},
      {"a hyphenated element", "org.example-service.Failed", false},
      {"an empty inner element", "org..Failed", false},
      {"an empty string", "", false},
      {"exactly 255 bytes", @dotted_255, true},
      {"256 bytes", @dotted_256, false},
      {"invalid UTF-8", @invalid_utf8, false},
      {"an embedded NUL", @embedded_nul, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_error_name?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_error_name?(input), description
      end
    end
  end

  describe "valid_member_name?/1" do
    # D-Bus specification, "Valid Names": member names hold only
    # "[A-Z][a-z][0-9]_", may not begin with a digit, must not contain '.',
    # are at least one byte long and at most 255 bytes long.
    cases = [
      {"the spec's example name", "NameOwnerChanged", true},
      {"a single character", "M", true},
      {"an underscore-led name", "_private", true},
      {"digits after the first character", "Arg0Changed", true},
      {"a leading digit", "1invalid", false},
      {"a hyphenated name", "Name-Owner", false},
      {"a dotted name", "org.example.Name", false},
      {"a trailing dot", "Name.", false},
      {"an empty string", "", false},
      {"a name with spaces", "Name Owner", false},
      {"exactly 255 bytes", @element_255, true},
      {"256 bytes", @element_256, false},
      {"invalid UTF-8", <<"Name", 0xFF>>, false},
      {"an embedded NUL", <<"Na", 0, "me">>, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_member_name?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_member_name?(input), description
      end
    end
  end

  describe "valid_well_known_name?/1" do
    # D-Bus specification, "Valid Names": bus names not starting with ':' are
    # well-known names; they need at least one '.' (and thus two elements),
    # each element holds only "[A-Z][a-z][0-9]_-", only unique-name elements
    # may begin with a digit, and the name is at most 255 bytes long.
    cases = [
      {"the spec's example name", "org.freedesktop.DBus", true},
      {"the minimum valid name", "a.b", true},
      {"a hyphenated element", "org.example-service.Name", true},
      {"a hyphen-led element", "org.-example.Name", true},
      {"an underscore-led element", "org._example.Name", true},
      {"digits after the first character", "org.e2.N4me", true},
      {"a single element", "org", false},
      {"a leading-digit element", "1org.example", false},
      {"a leading-digit trailing element", "org.1example", false},
      {"a unique name", ":1.0", false},
      {"an empty inner element", "org..example", false},
      {"a leading dot", ".org.example", false},
      {"a trailing dot", "org.example.", false},
      {"an empty string", "", false},
      {"a name with spaces", "org example", false},
      {"exactly 255 bytes", @dotted_255, true},
      {"256 bytes", @dotted_256, false},
      {"invalid UTF-8", @invalid_utf8, false},
      {"an embedded NUL", @embedded_nul, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_well_known_name?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_well_known_name?(input), description
      end
    end
  end

  describe "valid_unique_name?/1" do
    # D-Bus specification, "Valid Names": "Bus names that start with a colon
    # (':') character are unique connection names." They obey the bus-name
    # rules, but their elements may begin with a digit.
    cases = [
      {"a typical unique name", ":1.0", true},
      {"a larger unique name", ":1.42", true},
      {"hyphens and underscores", ":1.a-b_c", true},
      {"more than two elements", ":1.0.1", true},
      {"an alphabetic first element", ":a.b", true},
      {"a single element", ":1", false},
      {"a bare colon", ":", false},
      {"an empty first element", ":.1", false},
      {"an empty inner element", ":1..0", false},
      {"a trailing dot", ":1.0.", false},
      {"a missing colon", "1.0", false},
      {"a well-known name", "org.freedesktop.DBus", false},
      {"a colon after the first character", "a:1.0", false},
      {"an empty string", "", false},
      {"a name with spaces", ":1 0", false},
      {"exactly 255 bytes", @unique_255, true},
      {"256 bytes", @unique_256, false},
      {"invalid UTF-8", <<":1.", 0xFF>>, false},
      {"an embedded NUL", <<":1.", 0, "0">>, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_unique_name?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_unique_name?(input), description
      end
    end
  end

  describe "valid_bus_name?/1" do
    # A bus name is either a unique connection name or a well-known name.
    cases = [
      {"a well-known name", "org.freedesktop.DBus", true},
      {"a hyphenated well-known name", "org.example-service.Name", true},
      {"a unique name", ":1.42", true},
      {"a leading-digit unique element", ":1.0", true},
      {"a single-element well-known name", "org", false},
      {"a single-element unique name", ":1", false},
      {"a leading-digit well-known element", "1org.example", false},
      {"an empty string", "", false},
      {"a bare colon", ":", false},
      {"exactly 255 bytes", @dotted_255, true},
      {"256 bytes", @dotted_256, false},
      {"invalid UTF-8", @invalid_utf8, false},
      {"an embedded NUL", @embedded_nul, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_bus_name?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_bus_name?(input), description
      end
    end
  end

  describe "valid_namespace?/1" do
    # An `arg0namespace` match key holds a prefix of a well-known bus name, so
    # a single element is legal, but a unique name is not a namespace.
    cases = [
      {"a single element", "org", true},
      {"two elements", "org.example", true},
      {"a hyphenated element", "org.example-service", true},
      {"an underscore-led element", "_org._example", true},
      {"a leading-digit element", "1org", false},
      {"a leading-digit trailing element", "org.1example", false},
      {"a unique name", ":1.0", false},
      {"a colon-prefixed element", ":org", false},
      {"an empty inner element", "org..example", false},
      {"a leading dot", ".org", false},
      {"a trailing dot", "org.", false},
      {"an empty string", "", false},
      {"a namespace with spaces", "org example", false},
      {"exactly 255 bytes", @element_255, true},
      {"256 bytes", @element_256, false},
      {"invalid UTF-8", @invalid_utf8, false},
      {"an embedded NUL", @embedded_nul, false}
    ]

    for {description, input, expected} <- cases do
      test "#{expected} for #{description}" do
        assert WireValue.valid_namespace?(unquote(input)) == unquote(expected)
      end
    end

    test "false for non-binary input" do
      for {description, input} <- @non_binaries do
        refute WireValue.valid_namespace?(input), description
      end
    end
  end
end
