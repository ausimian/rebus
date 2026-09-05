defmodule Rebus.Connection.Hooks do
  @moduledoc false

  # Transition points in the file-descriptor claim lifecycle, and the internal
  # slack added to a request's reaper deadline. Production runs the no-op
  # default; the FD lifecycle tests substitute a module that blocks at a chosen
  # transition so the interleaving they assert is scheduled, not raced.

  @callback fd_claim_handoff() :: any()
  @callback fd_claim_delivery() :: any()
  @callback fd_claim_ack(claim :: map()) :: any()
  @callback request_timeout_slack() :: non_neg_integer()
end
