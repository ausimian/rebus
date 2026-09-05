defmodule Rebus.Connection.FDClaims do
  @moduledoc false

  # The connection-side table of replies whose descriptors have been handed to
  # a caller but not yet acknowledged.
  #
  # A reply carrying descriptors is first acknowledged through a small
  # connection-owned claim. This deliberately avoids treating delivery to a
  # `GenServer.call` alias as ownership transfer: aliases can be deactivated
  # while their process remains alive after a caller-side timeout. Until the
  # caller acknowledges, the connection still owns every descriptor in the
  # retained message and closes it on expiry, cancellation, caller exit or
  # connection shutdown.
  #
  # Like `Rebus.Connection.Writer` this is not a pure structure: it answers
  # `GenServer.reply/2`, calls the lifecycle hooks and owns its own timers.
  # `Process.send_after/3` is used directly because every claim timer targets
  # `self()` — the connection process, which is also the only process that ever
  # calls into this module. What it does not own is the pending-request table,
  # so `open/3` is handed the entry the connection has already popped.
  #
  # `Rebus.Connection.FDClaims.Client` is the other half: the functions that
  # run in the calling process and drive this table by `GenServer.call/3`.

  use TypedStruct

  alias Rebus.Message
  alias Rebus.UnixFD

  require Logger

  # FD delivery starts in a short extension of the request's original absolute
  # deadline. It exists solely to close or hand off a descriptor safely after
  # a reply reaches the boundary of that deadline; it is not a second public
  # request timeout. A definitive resolver may wait longer if a live connection
  # has an acknowledgement queued ahead of it; see `Client.resolve/3`.
  @fd_claim_handoff_grace 100
  @fd_claim_cleanup_grace 250

  typedstruct enforce: true do
    field :claims, %{reference() => map()}, default: %{}
    field :request_index, %{reference() => reference()}, default: %{}
    field :monitor_index, %{reference() => reference()}, default: %{}

    # A claim's terminal outcome, retained briefly so a resolver arriving after
    # the claim itself is gone still learns whether ownership transferred.
    field :outcomes, %{reference() => {:acknowledged | :closed, reference()}}, default: %{}
  end

  @typedoc """
  Everything a claim operation borrows from the connection for one call.
  """
  @type context :: %{required(:hooks) => module()}

  @typedoc """
  A pending method-call entry whose reply carries descriptors.
  """
  @type entry :: %{
          required(:msg) => Message.t(),
          required(:from) => GenServer.from(),
          required(:request_ref) => reference(),
          required(:monitor_ref) => reference(),
          required(:deadline) => integer()
        }

  @spec new() :: t()
  def new, do: %__MODULE__{}

  @doc false
  @spec handoff_grace() :: pos_integer()
  def handoff_grace, do: @fd_claim_handoff_grace

  @doc false
  @spec cleanup_grace() :: pos_integer()
  def cleanup_grace, do: @fd_claim_cleanup_grace

  # A live PID alone cannot prove a GenServer.call alias still accepts
  # messages. Hold FD ownership in a claimed state until Connection.call/3 has
  # consumed the regular-process delivery.
  @doc false
  @spec open(t(), entry(), context()) :: t()
  def open(%__MODULE__{} = claims, entry, context) do
    %{
      msg: msg,
      from: {pid, _tag} = from,
      request_ref: request_ref,
      monitor_ref: monitor_ref,
      deadline: deadline
    } = entry

    claim_ref = make_ref()
    claim_deadline = deadline(deadline)

    timer_ref =
      Process.send_after(
        self(),
        {:fd_claim_timeout, claim_ref},
        timer_timeout(claim_deadline)
      )

    claim = %{
      pid: pid,
      msg: msg,
      request_ref: request_ref,
      monitor_ref: monitor_ref,
      timer_ref: timer_ref,
      delivery_ref: nil,
      delivery_alias: nil,
      deadline: claim_deadline
    }

    context.hooks.fd_claim_handoff()
    GenServer.reply(from, {:fd_claim, claim_ref})

    %{
      claims
      | claims: Map.put(claims.claims, claim_ref, claim),
        request_index: Map.put(claims.request_index, request_ref, claim_ref),
        monitor_index: Map.put(claims.monitor_index, monitor_ref, claim_ref)
    }
  end

  # The public call's reply alias carries only the claim token. The
  # descriptor-bearing message uses a caller-created one-shot alias, which the
  # client explicitly unaliases on every timeout path. That prevents a late
  # internal delivery from reaching application `handle_info/2` after
  # Connection.call/3 has returned.
  @doc false
  @spec claim(t(), reference(), reference(), reference(), pid(), context()) ::
          {:ok | {:error, :fd_claim_expired}, t()}
  def claim(%__MODULE__{} = claims, claim_ref, delivery_ref, delivery_alias, pid, context) do
    case Map.fetch(claims.claims, claim_ref) do
      {:ok, %{pid: ^pid, delivery_ref: nil, msg: msg} = claim}
      when is_reference(delivery_ref) and is_reference(delivery_alias) ->
        if live?(claim) and Process.alive?(pid) do
          claim = rearm(claim_ref, claim)
          context.hooks.fd_claim_delivery()

          deliver_claim(claims, claim_ref, claim, msg, delivery_ref, delivery_alias)
        else
          {{:error, :fd_claim_expired}, drop(claims, claim_ref, close?: true, outcome: :closed)}
        end

      _ ->
        {{:error, :fd_claim_expired}, claims}
    end
  end

  defp deliver_claim(claims, claim_ref, claim, msg, delivery_ref, delivery_alias) do
    if live?(claim) and Process.alive?(claim.pid) do
      send(delivery_alias, {:rebus_fd_reply, claim_ref, delivery_ref, msg})

      claim = %{claim | delivery_ref: delivery_ref, delivery_alias: delivery_alias}

      {:ok, %{claims | claims: Map.put(claims.claims, claim_ref, claim)}}
    else
      {{:error, :fd_claim_expired}, drop(claims, claim_ref, close?: true, outcome: :closed)}
    end
  end

  @doc false
  @spec ack(t(), reference(), reference(), pid(), context()) ::
          {:ok | {:error, :fd_claim_expired}, t()}
  def ack(%__MODULE__{} = claims, claim_ref, delivery_ref, pid, context) do
    case Map.fetch(claims.claims, claim_ref) do
      {:ok, %{pid: ^pid, delivery_ref: ^delivery_ref} = claim} ->
        context.hooks.fd_claim_ack(claim)

        # A call alias timing out does not revoke a queued acknowledgement. It
        # is the resolver's FIFO position after this message that makes its
        # outcome definitive. Never acknowledge after the claim deadline,
        # though: at that point the connection must retain and close the FD.
        if live?(claim) and Process.alive?(pid) do
          {:ok, drop(claims, claim_ref, close?: false, outcome: :acknowledged)}
        else
          {{:error, :fd_claim_expired}, drop(claims, claim_ref, close?: true, outcome: :closed)}
        end

      _ ->
        {{:error, :fd_claim_expired}, claims}
    end
  end

  # This ordered descriptor-free barrier is used only if the bounded ack call
  # times out. It serializes behind a queued acknowledgement: either that ack
  # transferred ownership (and we report it), or this closes the claim.
  # Connection.call/3 waits for this without another finite timeout so it never
  # reports a closed claim while an earlier acknowledgement can still transfer
  # ownership.
  @doc false
  @spec resolve(t(), reference(), reference()) ::
          {:acknowledged | :closed | :fd_claim_expired, t()}
  def resolve(%__MODULE__{} = claims, claim_ref, delivery_ref) do
    case Map.fetch(claims.claims, claim_ref) do
      {:ok, %{delivery_ref: ^delivery_ref}} ->
        {:closed, drop(claims, claim_ref, close?: true, outcome: :closed)}

      _ ->
        {outcome, claims} = take_outcome(claims, claim_ref)
        {outcome || :fd_claim_expired, claims}
    end
  end

  # A caller which abandons the delivery leg closes the retained claim. A
  # duplicate discard is harmless and must not revive ownership.
  @doc false
  @spec discard(t(), reference(), pid()) :: t()
  def discard(%__MODULE__{} = claims, claim_ref, pid) do
    case Map.fetch(claims.claims, claim_ref) do
      {:ok, %{pid: ^pid}} -> drop(claims, claim_ref, close?: true, outcome: :closed)
      _ -> claims
    end
  end

  @doc false
  @spec expire(t(), reference()) :: t()
  def expire(%__MODULE__{} = claims, claim_ref) do
    case Map.fetch(claims.claims, claim_ref) do
      {:ok, _claim} ->
        Logger.warning("D-Bus FD reply claim dropped: :claim_timeout", reason: :claim_timeout)
        drop(claims, claim_ref, close?: true)

      :error ->
        claims
    end
  end

  @doc false
  @spec expire_outcome(t(), reference()) :: t()
  def expire_outcome(%__MODULE__{} = claims, claim_ref) do
    case Map.pop(claims.outcomes, claim_ref) do
      {nil, _outcomes} -> claims
      {_outcome, outcomes} -> %{claims | outcomes: outcomes}
    end
  end

  @doc false
  @spec fetch_by_request(t(), reference()) :: {:ok, reference()} | :error
  def fetch_by_request(%__MODULE__{request_index: index}, request_ref),
    do: Map.fetch(index, request_ref)

  @doc false
  @spec fetch_by_monitor(t(), reference()) :: {:ok, reference()} | :error
  def fetch_by_monitor(%__MODULE__{monitor_index: index}, monitor_ref),
    do: Map.fetch(index, monitor_ref)

  @doc false
  @spec drop(t(), reference(), keyword()) :: t()
  def drop(%__MODULE__{} = claims, claim_ref, opts) do
    case Map.pop(claims.claims, claim_ref) do
      {nil, _claims} ->
        claims

      {claim, remaining} ->
        drop_claim(claims, claim_ref, claim, remaining, opts)
    end
  end

  defp drop_claim(
         %__MODULE__{} = claims,
         claim_ref,
         %{msg: msg, request_ref: request_ref, monitor_ref: monitor_ref, timer_ref: timer_ref},
         remaining,
         opts
       ) do
    _ = Process.cancel_timer(timer_ref)

    close? = Keyword.get(opts, :close?, false)
    if close?, do: close_message_fds(msg)

    unless Keyword.get(opts, :monitor_down?, false) do
      Process.demonitor(monitor_ref, [:flush])
    end

    claims = %{
      claims
      | claims: remaining,
        request_index: Map.delete(claims.request_index, request_ref),
        monitor_index: Map.delete(claims.monitor_index, monitor_ref)
    }

    default_outcome = if close?, do: :closed, else: nil

    case Keyword.get(opts, :outcome, default_outcome) do
      outcome when outcome in [:acknowledged, :closed] ->
        put_outcome(claims, claim_ref, outcome)

      _ ->
        claims
    end
  end

  @doc false
  @spec rearm(reference(), map()) :: map()
  def rearm(claim_ref, %{timer_ref: timer_ref, deadline: deadline} = claim) do
    _ = Process.cancel_timer(timer_ref)

    %{
      claim
      | timer_ref:
          Process.send_after(
            self(),
            {:fd_claim_timeout, claim_ref},
            timer_timeout(deadline)
          )
    }
  end

  @doc false
  @spec put_outcome(t(), reference(), :acknowledged | :closed) :: t()
  def put_outcome(%__MODULE__{} = claims, claim_ref, outcome) do
    {old, outcomes} = Map.pop(claims.outcomes, claim_ref)

    if old, do: Process.cancel_timer(elem(old, 1))

    timer_ref =
      Process.send_after(self(), {:fd_claim_outcome_timeout, claim_ref}, @fd_claim_cleanup_grace)

    %{claims | outcomes: Map.put(outcomes, claim_ref, {outcome, timer_ref})}
  end

  @doc false
  @spec take_outcome(t(), reference()) :: {:acknowledged | :closed | nil, t()}
  def take_outcome(%__MODULE__{} = claims, claim_ref) do
    case Map.pop(claims.outcomes, claim_ref) do
      {nil, _outcomes} ->
        {nil, claims}

      {{outcome, timer_ref}, outcomes} ->
        _ = Process.cancel_timer(timer_ref)
        {outcome, %{claims | outcomes: outcomes}}
    end
  end

  # Closes every retained descriptor without touching the rest of the table.
  # Used from `terminate/2`, where nothing survives the return.
  @doc false
  @spec close_all(t()) :: :ok
  def close_all(%__MODULE__{claims: claims}) do
    Enum.each(claims, fn {_claim_ref, %{msg: msg, timer_ref: timer_ref}} ->
      _ = Process.cancel_timer(timer_ref)
      close_message_fds(msg)
    end)
  end

  # Closes and forgets everything: the connection is going away and no caller
  # can acknowledge a claim any more.
  @doc false
  @spec fail_all(t()) :: t()
  def fail_all(%__MODULE__{} = claims) do
    close_all(claims)

    Enum.each(claims.claims, fn {_claim_ref, %{monitor_ref: monitor_ref}} ->
      Process.demonitor(monitor_ref, [:flush])
    end)

    Enum.each(claims.outcomes, fn {_claim_ref, {_outcome, timer_ref}} ->
      _ = Process.cancel_timer(timer_ref)
    end)

    new()
  end

  @doc false
  @spec deadline(integer()) :: integer()
  def deadline(request_deadline), do: request_deadline + @fd_claim_cleanup_grace

  @doc false
  @spec live?(map()) :: boolean()
  def live?(%{deadline: deadline}) when is_integer(deadline) do
    deadline > System.monotonic_time(:millisecond)
  end

  @doc false
  @spec timer_timeout(integer()) :: non_neg_integer()
  def timer_timeout(deadline) do
    max(0, deadline - System.monotonic_time(:millisecond))
  end

  defp close_message_fds(%Message{unix_fds: fds}), do: UnixFD.close_all(fds)
end
