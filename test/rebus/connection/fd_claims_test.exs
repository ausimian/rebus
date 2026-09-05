defmodule Rebus.Connection.FDClaimsTest do
  # Not async: assertions turn on whether a specific descriptor number is still
  # open, and a concurrently opened descriptor could reuse a closed number.
  use ExUnit.Case, async: false

  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.Hooks
  alias Rebus.Message
  alias Rebus.TestFD
  alias Rebus.UnixFD

  @moduletag skip: TestFD.skip_reason()

  describe "open/3" do
    test "answers the caller with a claim token and retains the descriptor" do
      fd = owned_fd!()
      {claims, claim_ref, %{request_ref: request_ref, monitor_ref: monitor_ref}} = open(fd)

      assert %{^claim_ref => claim} = claims.claims
      assert claim.pid == self()
      assert claim.delivery_ref == nil
      assert claims.request_index == %{request_ref => claim_ref}
      assert claims.monitor_index == %{monitor_ref => claim_ref}
      assert {:ok, ^claim_ref} = FDClaims.fetch_by_request(claims, request_ref)
      assert {:ok, ^claim_ref} = FDClaims.fetch_by_monitor(claims, monitor_ref)

      assert open?(fd)
    end

    test "extends the request deadline by the cleanup grace" do
      fd = owned_fd!()
      deadline = System.monotonic_time(:millisecond) + 60_000
      {claims, claim_ref, _entry} = open(fd, deadline: deadline)

      assert claims.claims[claim_ref].deadline == FDClaims.deadline(deadline)
      FDClaims.close_all(claims)
    end
  end

  describe "claim/6" do
    test "delivers the reply on the caller's alias and records the delivery" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      delivery_ref = make_ref()
      delivery_alias = :erlang.alias([:reply])

      assert {:ok, claims} =
               FDClaims.claim(claims, claim_ref, delivery_ref, delivery_alias, self(), context())

      assert_receive {:rebus_fd_reply, ^claim_ref, ^delivery_ref, %Message{unix_fds: [^fd]}}
      assert claims.claims[claim_ref].delivery_ref == delivery_ref
      assert claims.claims[claim_ref].delivery_alias == delivery_alias
      assert open?(fd)

      :erlang.unalias(delivery_alias)
      FDClaims.close_all(claims)
    end

    test "refuses a second claim of the same reply" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      delivery_alias = :erlang.alias([:reply])

      assert {:ok, claims} =
               FDClaims.claim(claims, claim_ref, make_ref(), delivery_alias, self(), context())

      assert {{:error, :fd_claim_expired}, ^claims} =
               FDClaims.claim(claims, claim_ref, make_ref(), delivery_alias, self(), context())

      :erlang.unalias(delivery_alias)
      FDClaims.close_all(claims)
    end

    test "refuses a claim from a process which is not the caller" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)

      assert {{:error, :fd_claim_expired}, ^claims} =
               FDClaims.claim(claims, claim_ref, make_ref(), make_ref(), other_pid(), context())

      assert open?(fd)
    end

    test "closes the descriptor when the claim arrives after its deadline" do
      fd = owned_fd!()

      {claims, claim_ref, _entry} =
        open(fd, deadline: System.monotonic_time(:millisecond) - FDClaims.cleanup_grace() - 1)

      assert {{:error, :fd_claim_expired}, claims} =
               FDClaims.claim(claims, claim_ref, make_ref(), make_ref(), self(), context())

      assert claims.claims == %{}
      assert {:closed, _claims} = FDClaims.take_outcome(claims, claim_ref)
      assert closed?(fd)
    end

    test "refuses an unknown claim token without touching the table" do
      claims = FDClaims.new()

      assert {{:error, :fd_claim_expired}, ^claims} =
               FDClaims.claim(claims, make_ref(), make_ref(), make_ref(), self(), context())
    end
  end

  describe "ack/5" do
    test "transfers ownership and leaves the descriptor open" do
      fd = owned_fd!()
      {claims, claim_ref, %{request_ref: request_ref, monitor_ref: monitor_ref}} = open(fd)
      {claims, delivery_ref, delivery_alias} = claim(claims, claim_ref)

      assert {:ok, claims} = FDClaims.ack(claims, claim_ref, delivery_ref, self(), context())

      assert claims.claims == %{}
      assert claims.request_index == %{}
      assert claims.monitor_index == %{}
      assert :error = FDClaims.fetch_by_request(claims, request_ref)
      assert :error = FDClaims.fetch_by_monitor(claims, monitor_ref)
      assert {:acknowledged, _claims} = FDClaims.take_outcome(claims, claim_ref)

      # The whole point of the handoff: the caller owns this descriptor now.
      assert open?(fd)
      :erlang.unalias(delivery_alias)
    end

    test "refuses an acknowledgement quoting the wrong delivery token" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      {claims, _delivery_ref, delivery_alias} = claim(claims, claim_ref)

      assert {{:error, :fd_claim_expired}, ^claims} =
               FDClaims.ack(claims, claim_ref, make_ref(), self(), context())

      assert open?(fd)
      :erlang.unalias(delivery_alias)
    end

    test "closes the descriptor rather than acknowledging past the deadline" do
      fd = owned_fd!()
      delivery_ref = make_ref()

      claims = expired_claim(fd, delivery_ref: delivery_ref)

      [claim_ref] = Map.keys(claims.claims)

      assert {{:error, :fd_claim_expired}, claims} =
               FDClaims.ack(claims, claim_ref, delivery_ref, self(), context())

      assert claims.claims == %{}
      assert {:closed, _claims} = FDClaims.take_outcome(claims, claim_ref)
      assert closed?(fd)
    end

    test "runs the ack hook with the claim under acknowledgement" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      {claims, delivery_ref, delivery_alias} = claim(claims, claim_ref)
      parent = self()

      hooks = fn claim -> send(parent, {:acked, claim.delivery_ref}) end

      assert {:ok, _claims} =
               FDClaims.ack(claims, claim_ref, delivery_ref, self(), %{hooks: hook_module(hooks)})

      assert_receive {:acked, ^delivery_ref}
      assert open?(fd)
      :erlang.unalias(delivery_alias)
    end
  end

  describe "resolve/3" do
    test "reports the acknowledgement queued ahead of it, exactly once" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      {claims, delivery_ref, delivery_alias} = claim(claims, claim_ref)
      {:ok, claims} = FDClaims.ack(claims, claim_ref, delivery_ref, self(), context())

      assert {:acknowledged, claims} = FDClaims.resolve(claims, claim_ref, delivery_ref)
      assert {:fd_claim_expired, _claims} = FDClaims.resolve(claims, claim_ref, delivery_ref)

      assert open?(fd)
      :erlang.unalias(delivery_alias)
    end

    test "closes a claim which no acknowledgement reached" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      {claims, delivery_ref, delivery_alias} = claim(claims, claim_ref)

      assert {:closed, claims} = FDClaims.resolve(claims, claim_ref, delivery_ref)
      assert claims.claims == %{}
      assert closed?(fd)

      :erlang.unalias(delivery_alias)
    end

    test "reports an unknown token as expired" do
      assert {:fd_claim_expired, _claims} =
               FDClaims.resolve(FDClaims.new(), make_ref(), make_ref())
    end
  end

  describe "discard/3" do
    test "closes the descriptor and ignores a duplicate discard" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)

      claims = FDClaims.discard(claims, claim_ref, self())

      assert claims.claims == %{}
      assert closed?(fd)
      assert FDClaims.discard(claims, claim_ref, self()) == claims
    end

    test "ignores a discard from a process which is not the caller" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)

      assert FDClaims.discard(claims, claim_ref, other_pid()) == claims
      assert open?(fd)
    end
  end

  describe "expire/2" do
    test "closes the descriptor and logs the drop" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)

      log =
        ExUnit.CaptureLog.capture_log(fn ->
          claims = FDClaims.expire(claims, claim_ref)
          assert claims.claims == %{}
        end)

      assert log =~ "D-Bus FD reply claim dropped: :claim_timeout"
      assert closed?(fd)
    end

    test "ignores a timeout for a claim which is already gone" do
      claims = FDClaims.new()

      assert FDClaims.expire(claims, make_ref()) == claims
    end

    test "fires its own timer at the claim deadline" do
      fd = owned_fd!()

      {claims, claim_ref, _entry} =
        open(fd, deadline: System.monotonic_time(:millisecond) - FDClaims.cleanup_grace())

      assert_receive {:fd_claim_timeout, ^claim_ref}, 500
      FDClaims.close_all(claims)
    end
  end

  describe "drop/3" do
    test "closes on a caller DOWN without demonitoring the fired monitor" do
      fd = owned_fd!()
      {claims, claim_ref, %{monitor_ref: monitor_ref}} = open(fd)

      assert {:ok, ^claim_ref} = FDClaims.fetch_by_monitor(claims, monitor_ref)

      claims = FDClaims.drop(claims, claim_ref, close?: true, monitor_down?: true)

      assert claims.claims == %{}
      assert :error = FDClaims.fetch_by_monitor(claims, monitor_ref)
      assert closed?(fd)
    end

    test "closes on cancellation and records the outcome" do
      fd = owned_fd!()
      {claims, claim_ref, %{request_ref: request_ref}} = open(fd)

      {:ok, ^claim_ref} = FDClaims.fetch_by_request(claims, request_ref)
      claims = FDClaims.drop(claims, claim_ref, close?: true)

      assert claims.claims == %{}
      assert {:closed, _claims} = FDClaims.take_outcome(claims, claim_ref)
      assert closed?(fd)
    end

    test "cancels the claim timer" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      timer_ref = claims.claims[claim_ref].timer_ref

      claims = FDClaims.drop(claims, claim_ref, close?: true)

      assert claims.claims == %{}
      assert Process.read_timer(timer_ref) == false
      assert closed?(fd)
    end

    test "ignores an unknown claim token" do
      claims = FDClaims.new()

      assert FDClaims.drop(claims, make_ref(), close?: true) == claims
    end
  end

  describe "rearm/2" do
    test "replaces the pending timer" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      claim = claims.claims[claim_ref]

      rearmed = FDClaims.rearm(claim_ref, claim)

      assert rearmed.timer_ref != claim.timer_ref
      assert Process.read_timer(claim.timer_ref) == false
      assert is_integer(Process.read_timer(rearmed.timer_ref))

      Process.cancel_timer(rearmed.timer_ref)
      FDClaims.close_all(claims)
    end
  end

  describe "outcomes" do
    test "retains an outcome until it is taken" do
      claim_ref = make_ref()
      claims = FDClaims.put_outcome(FDClaims.new(), claim_ref, :acknowledged)

      assert {:acknowledged, claims} = FDClaims.take_outcome(claims, claim_ref)
      assert claims.outcomes == %{}
      assert {nil, ^claims} = FDClaims.take_outcome(claims, claim_ref)
    end

    test "replaces an outcome and cancels the timer it supersedes" do
      claim_ref = make_ref()
      claims = FDClaims.put_outcome(FDClaims.new(), claim_ref, :acknowledged)
      {_outcome, timer_ref} = claims.outcomes[claim_ref]

      claims = FDClaims.put_outcome(claims, claim_ref, :closed)

      assert Process.read_timer(timer_ref) == false
      assert {:closed, _claims} = FDClaims.take_outcome(claims, claim_ref)
    end

    test "forgets a retained outcome after the cleanup grace" do
      claim_ref = make_ref()
      claims = FDClaims.put_outcome(FDClaims.new(), claim_ref, :closed)

      assert_receive {:fd_claim_outcome_timeout, ^claim_ref},
                     FDClaims.cleanup_grace() * 4

      claims = FDClaims.expire_outcome(claims, claim_ref)

      assert claims.outcomes == %{}
      assert FDClaims.expire_outcome(claims, claim_ref) == claims
    end
  end

  describe "close_all/1" do
    test "closes every retained descriptor and cancels every claim timer" do
      first = owned_fd!()
      second = owned_fd!()
      {claims, first_ref, _entry} = open(first)
      {claims, second_ref, _entry} = open(claims, second)

      timers = Enum.map([first_ref, second_ref], &claims.claims[&1].timer_ref)

      assert :ok = FDClaims.close_all(claims)

      assert closed?(first)
      assert closed?(second)
      assert Enum.all?(timers, &(Process.read_timer(&1) == false))
    end
  end

  describe "fail_all/1" do
    test "closes everything, cancels outcome timers and empties the table" do
      fd = owned_fd!()
      {claims, claim_ref, _entry} = open(fd)
      claims = FDClaims.put_outcome(claims, make_ref(), :acknowledged)
      [{_ref, {_outcome, outcome_timer}}] = Map.to_list(claims.outcomes)
      timer_ref = claims.claims[claim_ref].timer_ref

      assert FDClaims.fail_all(claims) == FDClaims.new()

      assert closed?(fd)
      assert Process.read_timer(timer_ref) == false
      assert Process.read_timer(outcome_timer) == false
    end
  end

  describe "deadline arithmetic" do
    test "spends the graces the protocol documents" do
      assert FDClaims.handoff_grace() == 100
      assert FDClaims.cleanup_grace() == 250
      assert FDClaims.deadline(1_000) == 1_250
    end

    test "reports liveness against the absolute claim deadline" do
      now = System.monotonic_time(:millisecond)

      assert FDClaims.live?(%{deadline: now + 60_000})
      refute FDClaims.live?(%{deadline: now - 1})
    end

    test "never asks for a negative timer" do
      now = System.monotonic_time(:millisecond)

      assert FDClaims.timer_timeout(now - 10_000) == 0
      assert FDClaims.timer_timeout(now + 60_000) > 0
    end
  end

  defp context, do: %{hooks: Hooks.Default}

  defp open(fd), do: open(FDClaims.new(), fd, [])
  defp open(%FDClaims{} = claims, fd), do: open(claims, fd, [])
  defp open(fd, opts) when is_integer(fd), do: open(FDClaims.new(), fd, opts)

  defp open(%FDClaims{} = claims, fd, opts) do
    tag = make_ref()

    entry = %{
      msg: fd_reply(fd),
      from: {self(), tag},
      request_ref: make_ref(),
      monitor_ref: Process.monitor(self()),
      deadline: Keyword.get(opts, :deadline, System.monotonic_time(:millisecond) + 60_000)
    }

    claims = FDClaims.open(claims, entry, context())

    assert_receive {^tag, {:fd_claim, claim_ref}}

    {claims, claim_ref, entry}
  end

  defp claim(claims, claim_ref) do
    delivery_ref = make_ref()
    delivery_alias = :erlang.alias([:reply])

    {:ok, claims} =
      FDClaims.claim(claims, claim_ref, delivery_ref, delivery_alias, self(), context())

    assert_receive {:rebus_fd_reply, ^claim_ref, ^delivery_ref, %Message{}}

    {claims, delivery_ref, delivery_alias}
  end

  # A claim whose absolute deadline has already passed, built directly so the
  # test does not have to wait one out.
  defp expired_claim(fd, opts) do
    claim_ref = make_ref()
    request_ref = make_ref()
    monitor_ref = Process.monitor(self())

    claim = %{
      pid: self(),
      msg: fd_reply(fd),
      request_ref: request_ref,
      monitor_ref: monitor_ref,
      timer_ref: Process.send_after(self(), {:fd_claim_timeout, claim_ref}, 60_000),
      delivery_ref: Keyword.get(opts, :delivery_ref),
      delivery_alias: nil,
      deadline: System.monotonic_time(:millisecond) - 1
    }

    %FDClaims{
      claims: %{claim_ref => claim},
      request_index: %{request_ref => claim_ref},
      monitor_index: %{monitor_ref => claim_ref}
    }
  end

  defp fd_reply(fd) do
    Message.new!(:method_return, reply_serial: 1, signature: "h", body: [0], fds: [fd])
  end

  defp hook_module(fun) do
    Process.put(:fd_claim_ack_hook, fun)
    __MODULE__.AckHooks
  end

  defp other_pid do
    pid = spawn(fn -> Process.sleep(:infinity) end)
    on_exit(fn -> Process.exit(pid, :kill) end)
    pid
  end

  # `UnixFD.close/1` answers `{:error, :ebadf}` for a descriptor which is
  # already closed, so it is both the openness assertion and the cleanup. The
  # `:ebadf` answer only means "closed" while nothing else can reopen the
  # number, which is why this module stays `async: false`.
  defp open?(fd), do: UnixFD.close(fd) == :ok

  defp closed?(fd), do: UnixFD.close(fd) == {:error, :ebadf}

  # A descriptor this test owns outright: a dup received over SCM_RIGHTS, so
  # closing it cannot disturb a socket or file the VM still holds.
  defp owned_fd!, do: TestFD.dup!()

  defmodule AckHooks do
    @moduledoc false
    @behaviour Rebus.Connection.Hooks

    @impl Rebus.Connection.Hooks
    def fd_claim_handoff, do: :ok

    @impl Rebus.Connection.Hooks
    def fd_claim_delivery, do: :ok

    @impl Rebus.Connection.Hooks
    def fd_claim_ack(claim), do: Process.get(:fd_claim_ack_hook).(claim)

    @impl Rebus.Connection.Hooks
    def request_timeout_slack, do: 0
  end
end
