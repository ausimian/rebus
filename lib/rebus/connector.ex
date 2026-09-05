defmodule Rebus.Connector do
  @moduledoc false

  # Starting one supervised connection to a single resolved address. The
  # address-list walk calls this once per candidate; tests substitute it to
  # observe the per-candidate arguments without opening sockets.
  #
  # The second argument is the connection argument pair: the caller's public
  # options and the internal map that never travels in them.

  @callback connect(address :: map(), {keyword(), map()}) :: {:ok, pid()} | {:error, term()}
end
