defmodule Rebus.SeamsTest do
  use ExUnit.Case, async: true

  alias Rebus.Clock
  alias Rebus.Impl
  alias Rebus.Resolver

  describe "Rebus.Resolver.Inet" do
    test "resolves an IP literal to itself" do
      assert {:ok, [{127, 0, 0, 1}]} = Resolver.Inet.getaddrs("127.0.0.1", :inet, 1_000)

      assert {:ok, [{0, 0, 0, 0, 0, 0, 0, 1}]} =
               Resolver.Inet.getaddrs("::1", :inet6, 1_000)
    end

    test "reports an unresolvable name without raising" do
      assert {:error, _reason} =
               Resolver.Inet.getaddrs("rebus.invalid.", :inet, 1_000)
    end

    test "contains a resolver failure" do
      assert {:error, :resolution_failed} = Resolver.Inet.getaddrs("127.0.0.1", :inet, :bad)
    end
  end

  describe "Rebus.Clock.System" do
    test "reads a monotonic millisecond clock" do
      first = Clock.System.monotonic_time()
      second = Clock.System.monotonic_time()

      assert is_integer(first)
      assert second >= first
    end
  end

  describe "Rebus.Impl" do
    test "defaults to the production modules" do
      assert %{
               transport: Rebus.Transport.Socket,
               identity: Rebus.Identity.Cached,
               resolver: Rebus.Resolver.Inet,
               clock: Rebus.Clock.System,
               connector: Rebus.Connector.Supervised
             } = Impl.default()
    end

    test "takes only known keys from an override" do
      assert %{transport: Rebus.TestImpl, identity: Rebus.Identity.Cached} =
               Impl.build(transport: Rebus.TestImpl, unknown: :ignored)

      assert Impl.build(:not_an_override) == Impl.default()
    end

    test "reads the private option out of a caller's options" do
      assert %{transport: Rebus.TestImpl} =
               Impl.from_options(timeout: 100, __impl__: %{transport: Rebus.TestImpl})

      assert Impl.from_options(timeout: 100) == Impl.default()
    end
  end
end
