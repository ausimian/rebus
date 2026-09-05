defmodule Rebus.Resolver.Inet do
  @moduledoc false

  # The production resolver: Erlang's `:inet` resolver, with any raise or throw
  # contained so a resolver failure stays a retryable candidate error.

  @behaviour Rebus.Resolver

  @impl Rebus.Resolver
  def getaddrs(host, family, timeout) do
    :inet.getaddrs(:binary.bin_to_list(host), family, timeout)
  catch
    _kind, _reason -> {:error, :resolution_failed}
  end
end
