defmodule Rebus.SignalHandler do
  @moduledoc false
  @behaviour :gen_event

  alias Rebus.Message

  def child_spec(_args) do
    %{
      id: __MODULE__,
      start: {:gen_event, :start_link, [local: __MODULE__]},
      type: :worker
    }
  end

  def notify(%Message{type: :signal} = msg, from \\ self()) do
    :gen_event.notify(__MODULE__, {msg, from})
  end

  @impl true
  def init({src, sub, ref} = state) when is_pid(src) and is_pid(sub) and is_reference(ref) do
    {:ok, state}
  end

  def init({src, sub, ref, %Rebus.MatchRule{}} = state)
      when is_pid(src) and is_pid(sub) and is_reference(ref) do
    {:ok, state}
  end

  @impl true
  def handle_event({%Message{} = msg, from}, {from, sub, ref} = state) do
    send(sub, {ref, msg})
    {:ok, state}
  end

  def handle_event({%Message{} = msg, from}, {from, sub, ref, rule} = state) do
    if Rebus.MatchRule.matches?(rule, msg), do: send(sub, {ref, msg})
    {:ok, state}
  end

  def handle_event(_, state), do: {:ok, state}

  @impl true
  def handle_info(_, state) do
    {:ok, state}
  end

  @impl true
  def handle_call(_request, state) do
    {:ok, :ok, state}
  end
end
