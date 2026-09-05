defmodule Rebus.Clock.System do
  @moduledoc false

  # The production clock.

  @behaviour Rebus.Clock

  @impl Rebus.Clock
  def monotonic_time, do: System.monotonic_time(:millisecond)
end
