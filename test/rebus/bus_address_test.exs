defmodule Rebus.BusAddressTest do
  use ExUnit.Case, async: true

  alias Rebus.BusAddress

  @guid "30313233343536373839414243444546"

  describe "parse/1" do
    test "parses a Unix pathname and retains a bounded expected guid" do
      assert {:ok, [{:local, "/run/user/1000/bus", @guid}]} =
               BusAddress.parse("unix:path=/run/user/1000/bus,guid=#{@guid}")
    end

    test "parses an abstract Unix socket as a leading NUL path" do
      assert {:ok, [{:local, <<0, "rebus">>, nil}]} =
               BusAddress.parse("unix:abstract=rebus")
    end

    test "parses TCP endpoints with explicit and unspecified families" do
      assert {:ok, [{:tcp, "127.0.0.1", 12_345, :unspec, nil}]} =
               BusAddress.parse("tcp:host=127.0.0.1,port=12345")

      assert {:ok, [{:tcp, "127.0.0.1", 12_345, :inet, @guid}]} =
               BusAddress.parse("tcp:host=127.0.0.1,port=12345,family=ipv4,guid=#{@guid}")

      assert {:ok, [{:tcp, "::1", 12_345, :inet6, nil}]} =
               BusAddress.parse("tcp:host=%3A%3A1,port=12345,family=ipv6")
    end

    test "preserves order and ignores unrecognised parameters and forms" do
      assert {:ok, [:unsupported, {:local, "/run/dbus/system_bus_socket", nil}]} =
               BusAddress.parse(
                 "unix:runtime=/run/user/1000;unix:path=/run/dbus/system_bus_socket,foo=bar"
               )

      assert {:ok, [{:local, "/run/dbus/system_bus_socket", nil}]} =
               BusAddress.parse("unix:path=/run/dbus/system_bus_socket,foo=bar")

      assert {:ok, [{:tcp, "127.0.0.1", 12_345, :unspec, nil}]} =
               BusAddress.parse("tcp:host=127.0.0.1,port=12345,future=option")
    end

    test "percent-decodes escaped separators and percent signs" do
      assert {:ok, [{:local, "/tmp/a;b,c=d%", nil}]} =
               BusAddress.parse("unix:path=/tmp/a%3Bb%2Cc%3Dd%25")
    end

    test "accepts bounded literal values outside the historical restricted alphabet" do
      control = <<1>>
      path = "/tmp/space :+~@=\u2603" <> control

      assert {:ok, [{:local, ^path, nil}]} =
               BusAddress.parse("unix:path=" <> path)
    end

    test "accepts parameterless unsupported transports but keeps supported forms precise" do
      assert {:ok, [:unsupported]} = BusAddress.parse("autolaunch:")

      assert {:ok, [:unsupported, {:local, "/tmp/bus", nil}]} =
               BusAddress.parse("autolaunch:;unix:path=/tmp/bus")

      assert {:error, {:invalid_bus_address, :missing_path}} = BusAddress.parse("unix:")
      assert {:error, {:invalid_bus_address, :missing_path}} = BusAddress.parse("unix:path=")

      assert {:error, {:invalid_bus_address, :missing_path}} =
               BusAddress.parse("unix:guid=#{@guid}")

      assert {:error, {:invalid_bus_address, :missing_host}} = BusAddress.parse("tcp:")

      assert {:error, {:invalid_bus_address, :missing_host}} =
               BusAddress.parse("tcp:family=ipv4")
    end

    test "rejects malformed entries without echoing input" do
      for {address, reason} <- [
            {"", :empty_entry},
            {"unix", :invalid_entry},
            {"unix:path", :invalid_entry},
            {"unix:path=/tmp/bus,,guid=x", :invalid_entry},
            {"unix:path=/tmp/bus,path=/tmp/other", :duplicate_key},
            {"unix:path=/tmp/%", :invalid_escape},
            {"unix:path=/tmp/%xz", :invalid_escape},
            {"unix:path=/tmp/%00", :nul_byte},
            {<<"unix:path=/tmp/", 0>>, :nul_byte},
            {"unix:path=/tmp/bus,bad%key=x", :invalid_key},
            {"unix:path=/tmp/bus,abstract=other", :ambiguous_unix_address},
            {"tcp:port=1234", :missing_host},
            {"tcp:host=127.0.0.1", :missing_port},
            {"tcp:host=127.0.0.1,port=0", :invalid_port},
            {"tcp:host=127.0.0.1,port=abc", :invalid_port},
            {"tcp:host=127.0.0.1,port=1234,family=inet", :invalid_family}
          ] do
        assert {:error, {:invalid_bus_address, ^reason}} = BusAddress.parse(address)
      end
    end

    test "rejects malformed expected guids without retaining them" do
      for guid <- [
            "",
            "1234",
            String.duplicate("a", 31),
            String.duplicate("a", 33),
            String.duplicate("g", 32)
          ] do
        assert {:error, {:invalid_bus_address, :invalid_guid}} =
                 BusAddress.parse("unix:path=/tmp/bus,guid=#{guid}")
      end

      assert {:error, {:invalid_bus_address, :invalid_guid}} =
               BusAddress.parse("tcp:host=127.0.0.1,port=12345,guid=#{String.duplicate("g", 32)}")
    end

    test "accepts one trailing separator but rejects other empty entries" do
      assert {:ok, [{:local, "/tmp/bus", nil}]} = BusAddress.parse("unix:path=/tmp/bus;")

      for address <- [
            ";unix:path=/tmp/bus",
            "unix:path=/tmp/bus;;",
            "unix:path=/tmp/bus;;tcp:host=x,port=1"
          ] do
        assert {:error, {:invalid_bus_address, :empty_entry}} = BusAddress.parse(address)
      end
    end

    test "is total and bounded for non-binary and oversized input" do
      assert {:error, {:invalid_bus_address, :not_binary}} = BusAddress.parse(:not_an_address)

      assert {:error, {:invalid_bus_address, :too_long}} =
               BusAddress.parse(:binary.copy("x", 4_097))
    end

    test "rejects excessive address and parameter counts" do
      assert {:error, {:invalid_bus_address, :too_many_addresses}} =
               BusAddress.parse(List.duplicate("unix:path=/tmp/bus", 17) |> Enum.join(";"))

      parameters =
        ["path=/tmp/bus" | Enum.map(1..16, &"guid=#{&1}")]
        |> Enum.join(",")

      assert {:error, {:invalid_bus_address, :too_many_parameters}} =
               BusAddress.parse("unix:" <> parameters)
    end
  end
end
