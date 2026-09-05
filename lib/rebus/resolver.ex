defmodule Rebus.Resolver do
  @moduledoc false

  # Host resolution for TCP bus addresses. Split out so the address-list tests
  # can drive resolution order, duplicates and per-family timing without a DNS
  # server.

  @callback getaddrs(host :: binary(), family :: :inet | :inet6, timeout :: pos_integer()) ::
              {:ok, [:inet.ip_address()]} | {:error, term()}
end
