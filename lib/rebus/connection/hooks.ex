defmodule Rebus.Connection.Hooks do
  @moduledoc false

  # The internal slack added to a request's reaper deadline. Production runs
  # the default, which adds none; tests substitute a module that holds a
  # request open past its caller's deadline, so the late reply they assert is
  # scheduled, not raced.

  @callback request_timeout_slack() :: non_neg_integer()
end
