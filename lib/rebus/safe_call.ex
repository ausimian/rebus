defmodule Rebus.SafeCall do
  @moduledoc false

  # The one place a `GenServer.call/3` exit becomes a return value. Every
  # caller-side entry point into a connection or a match-subscription worker
  # goes through here: a call that times out may first cast a cancellation, so
  # the server forgets a request its caller has stopped waiting for, and any
  # other exit means the server is gone.

  @typedoc """
  How a call answers when it does not complete.

  `:cancel` is cast to the server before a timed-out call answers,
  `:on_timeout` replaces the `{:error, :timeout}` it would otherwise return,
  and `:then` post-processes a successful reply inside the same `try`, so an
  exit raised while doing so is caught here too.
  """
  @type option ::
          {:cancel, term()}
          | {:on_timeout, (-> term())}
          | {:then, (term() -> term())}

  @doc false
  @spec call(GenServer.server(), term(), timeout(), [option()]) :: term()
  def call(server, request, timeout, opts \\ []) do
    reply = GenServer.call(server, request, timeout)

    case Keyword.get(opts, :then) do
      nil -> reply
      then_fun -> then_fun.(reply)
    end
  catch
    :exit, {:timeout, _call} -> timed_out(server, opts)
    :exit, _reason -> {:error, :disconnected}
  end

  defp timed_out(server, opts) do
    case Keyword.get(opts, :cancel) do
      nil -> :ok
      cancellation -> GenServer.cast(server, cancellation)
    end

    case Keyword.get(opts, :on_timeout) do
      nil -> {:error, :timeout}
      on_timeout -> on_timeout.()
    end
  end
end
