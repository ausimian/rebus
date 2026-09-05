defmodule Rebus.Clock do
  @moduledoc false

  # The monotonic millisecond reading the address-list deadline arithmetic is
  # built on. Tests substitute a clock they advance themselves so budget
  # splitting can be asserted exactly rather than approximately.

  @callback monotonic_time() :: integer()
end
