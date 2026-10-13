defmodule Rebus.Clock do
  @moduledoc false

  # The monotonic millisecond reading the setup deadline arithmetic is built
  # on, both for the address-list walk and for each connection's own setup.
  # Tests substitute a clock they advance themselves so budget splitting can be
  # asserted exactly rather than approximately.

  @callback monotonic_time() :: integer()
end
