defmodule Rebus.UnixFDHandleTest do
  # Not async: the reuse and GC tests depend on which descriptor number the
  # kernel hands out next, which async tests opening sockets would disturb.
  use ExUnit.Case, async: false

  alias Rebus.{TestFD, UnixFD}

  @moduletag skip: TestFD.skip_reason()

  describe "close/1" do
    test "closes once and answers :closed after that" do
      {:ok, read, write} = UnixFD.__pipe__()

      assert :ok = UnixFD.close(read)
      assert {:error, :closed} = UnixFD.close(read)
      assert {:error, :closed} = UnixFD.close(read)
      assert {:ok, 1} = UnixFD.__close_count__(read)
      refute UnixFD.open?(read)

      assert UnixFD.open?(write)
      assert :ok = UnixFD.close(write)
    end

    test "any holder may close a handle" do
      {:ok, read, write} = UnixFD.__pipe__()

      task = Task.async(fn -> UnixFD.close(read) end)

      assert :ok = Task.await(task)
      refute UnixFD.open?(read)
      assert {:ok, 1} = UnixFD.__close_count__(read)
      assert :ok = UnixFD.close(write)
    end
  end

  describe "garbage collection" do
    # Live counts are compared with <=, not ==: a handle left over from an
    # earlier test may be freed meanwhile, which lowers the count.

    test "closes the descriptor of a handle nothing refers to" do
      baseline = UnixFD.__live_count__()
      {fd, identity} = lost_pipe_end()

      assert eventually(fn ->
               :erlang.garbage_collect()
               UnixFD.__fd_identity__(fd) != {:ok, identity}
             end)

      assert eventually(fn -> UnixFD.__live_count__() <= baseline end)
    end

    test "leaks no handle when many are dropped" do
      baseline = UnixFD.__live_count__()

      for _ <- 1..200 do
        {:ok, _read, _write} = UnixFD.__pipe__()
      end

      :erlang.garbage_collect()

      assert eventually(fn -> UnixFD.__live_count__() <= baseline end)
    end

    test "closes the handles of a process that exits holding them" do
      baseline = UnixFD.__live_count__()
      parent = self()

      {pid, ref} =
        spawn_monitor(fn ->
          {:ok, read, _write} = UnixFD.__pipe__()
          {:ok, fd} = UnixFD.with_fd(read, & &1)
          {:ok, identity} = UnixFD.__fd_identity__(fd)
          send(parent, {:read_end, fd, identity})
        end)

      assert_receive {:read_end, fd, identity}
      assert_receive {:DOWN, ^ref, :process, ^pid, :normal}

      # The owner's exit closes the descriptors before its heap, and with it
      # the handles, is freed, so wait for the handles themselves.
      assert eventually(fn -> UnixFD.__live_count__() <= baseline end)
      refute UnixFD.__fd_identity__(fd) == {:ok, identity}
    end
  end

  describe "owner exit" do
    test "closes the descriptor while aliases remain in other processes" do
      {owner, handle} = owned_elsewhere()
      holder = spawn_holder(handle)

      send(owner, :exit)

      assert eventually(fn -> not UnixFD.open?(handle) end)
      assert_closed_once(handle, [owner])
      assert {:error, :closed} = UnixFD.close(handle)
      assert {:error, :closed} = UnixFD.owner(handle)

      # The alias in another process sees the same closed handle.
      send(holder, {:open?, self()})
      assert_receive {:open?, false}
    end

    test "closes the descriptor when the owner is killed" do
      {owner, handle} = owned_elsewhere()

      Process.exit(owner, :kill)

      assert eventually(fn -> not UnixFD.open?(handle) end)
      assert_closed_once(handle, [owner])
    end

    test "defers close(2) until a pin in another process is released" do
      {owner, handle} = owned_elsewhere()
      pinner = pin_in_process(handle)

      send(owner, :exit)

      assert eventually(fn -> not UnixFD.open?(handle) end)
      assert {:ok, 0} = UnixFD.__close_count__(handle)

      send(pinner, :release)

      assert_closed_once(handle, [owner, pinner])
    end
  end

  describe "descriptor-number reuse" do
    test "a stale handle never touches the number's next occupant" do
      {:ok, stale, stale_write} = UnixFD.__pipe__()
      {:ok, number} = UnixFD.with_fd(stale, & &1)

      assert :ok = UnixFD.close(stale)
      {fresh, fresh_write} = pipe_reusing!(number)
      {:ok, fresh_identity} = UnixFD.__fd_identity__(number)

      assert {:error, :closed} = UnixFD.close(stale)

      assert {:error, :closed} =
               UnixFD.with_fd(stale, fn _ -> flunk("ran on a closed handle") end)

      assert {:error, :closed} = UnixFD.dup(stale)
      assert {:error, :closed} = UnixFD.controlling_process(stale, self())
      assert {:error, :closed} = UnixFD.owner(stale)

      assert {:ok, ^fresh_identity} = UnixFD.__fd_identity__(number)
      assert {:ok, ^number} = UnixFD.with_fd(fresh, & &1)
      assert UnixFD.open?(fresh)

      Enum.each([fresh, fresh_write, stale_write], &UnixFD.close/1)
    end

    test "collecting a closed handle leaves the number's next occupant open" do
      baseline = UnixFD.__live_count__()
      parent = self()

      holder =
        spawn(fn ->
          {:ok, read, write} = UnixFD.__pipe__()
          :ok = UnixFD.close(write)
          {:ok, number} = UnixFD.with_fd(read, & &1)
          :ok = UnixFD.close(read)
          send(parent, {:closed, number})

          # Uses `read` afterwards, so it stays live until this process exits.
          receive do
            :drop -> UnixFD.open?(read)
          end
        end)

      assert_receive {:closed, number}
      {fresh, fresh_write} = pipe_reusing!(number)
      {:ok, fresh_identity} = UnixFD.__fd_identity__(number)

      ref = Process.monitor(holder)
      send(holder, :drop)
      assert_receive {:DOWN, ^ref, :process, ^holder, :normal}
      assert eventually(fn -> UnixFD.__live_count__() <= baseline + 2 end)

      assert {:ok, ^fresh_identity} = UnixFD.__fd_identity__(number)
      assert UnixFD.open?(fresh)
      Enum.each([fresh, fresh_write], &UnixFD.close/1)
    end
  end

  describe "with_fd/2" do
    test "passes the descriptor number and returns the function's result" do
      {:ok, read, write} = UnixFD.__pipe__()
      {:ok, read_fd} = UnixFD.with_fd(read, & &1)

      assert {:ok, {:ok, _identity}} = UnixFD.with_fd(read, &UnixFD.__fd_identity__/1)
      assert {:ok, :done} = UnixFD.with_fd(write, fn fd -> if fd != read_fd, do: :done end)

      Enum.each([read, write], &UnixFD.close/1)
    end

    test "unpins when the function raises" do
      {:ok, read, write} = UnixFD.__pipe__()

      assert_raise RuntimeError, fn -> UnixFD.with_fd(read, fn _ -> raise "boom" end) end

      assert :ok = UnixFD.close(read)
      assert {:ok, 1} = UnixFD.__close_count__(read)
      assert :ok = UnixFD.close(write)
    end

    test "keeps the descriptor open until the function returns" do
      {:ok, read, write} = UnixFD.__pipe__()
      {:ok, {:ok, identity}} = UnixFD.with_fd(read, &UnixFD.__fd_identity__/1)

      assert {:ok, {:ok, ^identity}} =
               UnixFD.with_fd(read, fn fd ->
                 :ok = UnixFD.close(read)
                 refute UnixFD.open?(read)
                 {:ok, 0} = UnixFD.__close_count__(read)
                 UnixFD.__fd_identity__(fd)
               end)

      assert {:ok, 1} = UnixFD.__close_count__(read)
      assert :ok = UnixFD.close(write)
    end

    test "releases the pin of a process killed inside the function" do
      {:ok, read, write} = UnixFD.__pipe__()
      pinner = pin_in_process(read)

      assert :ok = UnixFD.close(read)
      assert {:ok, 0} = UnixFD.__close_count__(read)

      Process.exit(pinner, :kill)

      assert_closed_once(read, [pinner])
      assert :ok = UnixFD.close(write)
    end
  end

  describe "controlling_process/2" do
    test "moves ownership, after which the old owner's exit leaves it open" do
      {owner, handle} = owned_elsewhere()
      new_owner = spawn_holder(handle)

      send(owner, {:transfer, new_owner, self()})
      assert_receive {:transferred, :ok}
      assert {:ok, ^new_owner} = UnixFD.owner(handle)

      send(owner, :exit)
      ref = Process.monitor(owner)
      assert_receive {:DOWN, ^ref, :process, _, _}
      assert UnixFD.open?(handle)

      Process.exit(new_owner, :kill)
      assert eventually(fn -> not UnixFD.open?(handle) end)
      assert_closed_once(handle, [new_owner])
    end

    test "answers :not_owner to any other process, including a former owner" do
      {:ok, read, write} = UnixFD.__pipe__()
      holder = spawn_holder(read)

      assert :ok = UnixFD.controlling_process(read, holder)
      assert {:error, :not_owner} = UnixFD.controlling_process(read, self())
      assert {:ok, ^holder} = UnixFD.owner(read)

      Process.exit(holder, :kill)
      assert eventually(fn -> not UnixFD.open?(read) end)
      assert :ok = UnixFD.close(write)
    end

    test "is a no-op when the target already owns the handle" do
      {:ok, read, write} = UnixFD.__pipe__()
      me = self()

      assert :ok = UnixFD.controlling_process(read, me)
      assert {:ok, ^me} = UnixFD.owner(read)
      Enum.each([read, write], &UnixFD.close/1)
    end

    test "closes the descriptor when the target is not alive" do
      {:ok, read, write} = UnixFD.__pipe__()
      {dead, ref} = spawn_monitor(fn -> :ok end)
      assert_receive {:DOWN, ^ref, :process, ^dead, _}

      assert {:error, :noproc} = UnixFD.controlling_process(read, dead)
      refute UnixFD.open?(read)
      assert {:ok, 1} = UnixFD.__close_count__(read)
      assert :ok = UnixFD.close(write)
    end

    test "refuses a pid on another node and leaves the handle as it was" do
      {:ok, read, write} = UnixFD.__pipe__()
      me = self()

      assert {:error, :not_local} = UnixFD.controlling_process(read, remote_pid())
      assert {:ok, ^me} = UnixFD.owner(read)
      Enum.each([read, write], &UnixFD.close/1)
    end

    test "answers :closed for a closed handle" do
      {:ok, read, write} = UnixFD.__pipe__()
      :ok = UnixFD.close(read)

      assert {:error, :closed} = UnixFD.controlling_process(read, self())
      assert :ok = UnixFD.close(write)
    end
  end

  describe "dup/1" do
    test "returns an independent handle owned by the caller" do
      {:ok, read, write} = UnixFD.__pipe__()
      {:ok, {:ok, identity}} = UnixFD.with_fd(read, &UnixFD.__fd_identity__/1)

      # The task that duplicated it owned the copy, and has exited.
      {:ok, copy} = Task.await(Task.async(fn -> UnixFD.dup(read) end))
      assert eventually(fn -> not UnixFD.open?(copy) end)

      {:ok, copy} = UnixFD.dup(read)
      me = self()
      assert {:ok, ^me} = UnixFD.owner(copy)
      assert {:ok, {:ok, ^identity}} = UnixFD.with_fd(copy, &UnixFD.__fd_identity__/1)

      :ok = UnixFD.close(read)
      assert UnixFD.open?(copy)
      assert {:ok, {:ok, ^identity}} = UnixFD.with_fd(copy, &UnixFD.__fd_identity__/1)

      Enum.each([copy, write], &UnixFD.close/1)
    end
  end

  describe "handles that are not local" do
    test "answer :not_local to every operation" do
      for handle <- [%UnixFD{ref: remote_ref()}, %UnixFD{ref: make_ref()}] do
        assert {:error, :not_local} = UnixFD.close(handle)
        assert {:error, :not_local} = UnixFD.owner(handle)
        assert {:error, :not_local} = UnixFD.controlling_process(handle, self())
        assert {:error, :not_local} = UnixFD.with_fd(handle, fn _ -> flunk("ran") end)
        assert {:error, :not_local} = UnixFD.dup(handle)
        assert {:error, :not_local} = UnixFD.pin(handle)
        refute UnixFD.open?(handle)
        assert inspect(handle) == "#Rebus.UnixFD<not_local>"
      end
    end
  end

  describe "Inspect" do
    test "shows the state and owner, never the descriptor number" do
      {:ok, read, write} = UnixFD.__pipe__()

      assert inspect(read) == "#Rebus.UnixFD<open, owner: #{inspect(self())}>"

      :ok = UnixFD.close(read)
      assert inspect(read) == "#Rebus.UnixFD<closed>"
      :ok = UnixFD.close(write)
    end
  end

  describe "internal wrap/2" do
    test "wraps a raw descriptor for a given owner" do
      fd = TestFD.dup!()
      holder = spawn_holder(nil)

      assert {:ok, handle} = UnixFD.wrap(fd, holder)
      assert {:ok, ^holder} = UnixFD.owner(handle)
      assert {:ok, ^fd} = UnixFD.with_fd(handle, & &1)

      Process.exit(holder, :kill)
      assert eventually(fn -> not UnixFD.open?(handle) end)
      assert_closed_once(handle, [holder])
    end

    test "closes the descriptor when the owner is dead or remote" do
      for owner <- [dead_pid(), remote_pid()] do
        fd = TestFD.dup!()
        {:ok, identity} = UnixFD.__fd_identity__(fd)

        assert {:error, reason} = UnixFD.wrap(fd, owner)
        assert reason in [:noproc, :not_local]
        refute UnixFD.__fd_identity__(fd) == {:ok, identity}
      end
    end

    test "refuses a number that is not an open descriptor" do
      assert {:error, :invalid_descriptor} = UnixFD.wrap(-1, self())
      assert {:error, :invalid_descriptor} = UnixFD.wrap(2 ** 31, self())
      assert {:error, :invalid_descriptor} = UnixFD.wrap(2 ** 70, self())
      assert {:error, :invalid_descriptor} = UnixFD.__fd_identity__(2 ** 70)
      assert {:error, :ebadf} = UnixFD.wrap(1_000_000, self())
    end
  end

  describe "internal pin/1 and unpin/1" do
    test "defers close(2) to the last unpin, and unpinning twice is harmless" do
      {:ok, read, write} = UnixFD.__pipe__()
      {:ok, first, fd} = UnixFD.pin(read)
      {:ok, second, ^fd} = UnixFD.pin(read)

      :ok = UnixFD.close(read)
      assert {:error, :closed} = UnixFD.pin(read)

      assert :ok = UnixFD.unpin(first)
      assert :ok = UnixFD.unpin(first)
      assert {:ok, 0} = UnixFD.__close_count__(read)

      assert :ok = UnixFD.unpin(second)
      assert {:ok, 1} = UnixFD.__close_count__(read)
      assert :ok = UnixFD.close(write)
    end

    test "unpin/1 does nothing for anything that is not a pin token" do
      {:ok, read, write} = UnixFD.__pipe__()
      %UnixFD{ref: handle_ref} = read

      for not_a_token <- [make_ref(), remote_ref(), handle_ref, :not_a_ref] do
        assert :ok = UnixFD.unpin(not_a_token)
      end

      assert UnixFD.open?(read)
      Enum.each([read, write], &UnixFD.close/1)
    end

    test "releases a pin whose token is garbage collected" do
      {:ok, read, write} = UnixFD.__pipe__()

      {pid, ref} =
        spawn_monitor(fn ->
          {:ok, _token, _fd} = UnixFD.pin(read)
        end)

      assert_receive {:DOWN, ^ref, :process, ^pid, :normal}
      :ok = UnixFD.close(read)

      assert_closed_once(read, [pid])
      assert :ok = UnixFD.close(write)
    end
  end

  describe "races" do
    test "concurrent closes close(2) exactly once" do
      for _ <- 1..50 do
        {:ok, read, write} = UnixFD.__pipe__()

        results =
          1..8
          |> Enum.map(fn _ -> Task.async(fn -> UnixFD.close(read) end) end)
          |> Task.await_many()

        assert Enum.count(results, &(&1 == :ok)) == 1
        assert Enum.count(results, &(&1 == {:error, :closed})) == 7
        assert {:ok, 1} = UnixFD.__close_count__(read)
        :ok = UnixFD.close(write)
      end
    end

    test "close, use and owner exit together close(2) exactly once" do
      for _ <- 1..50 do
        {owner, handle} = owned_elsewhere()

        users =
          for _ <- 1..4 do
            Task.async(fn ->
              UnixFD.with_fd(handle, fn fd -> UnixFD.__fd_identity__(fd) end)
            end)
          end

        closer = Task.async(fn -> UnixFD.close(handle) end)
        Process.exit(owner, :kill)

        for result <- Task.await_many(users) do
          assert match?({:ok, {:ok, _}}, result) or result == {:error, :closed}
        end

        assert Task.await(closer) in [:ok, {:error, :closed}]
        assert_closed_once(handle, [owner])
        refute UnixFD.open?(handle)
      end
    end

    test "a transfer racing the owner's death closes(2) exactly once" do
      for _ <- 1..50 do
        {owner, handle} = owned_elsewhere()
        target = spawn_holder(handle)

        send(owner, {:transfer, target, self()})
        Process.exit(owner, :kill)

        receive do
          {:transferred, :ok} -> :ok
        after
          0 -> :ok
        end

        # Whichever won, killing the target leaves nothing to own it.
        Process.exit(target, :kill)

        assert eventually(fn -> not UnixFD.open?(handle) end)
        assert_closed_once(handle, [owner, target])
      end
    end

    test "a transfer racing a close closes(2) exactly once" do
      for _ <- 1..50 do
        {:ok, read, write} = UnixFD.__pipe__()
        target = spawn_holder(read)

        closer = Task.async(fn -> UnixFD.close(read) end)
        transfer = UnixFD.controlling_process(read, target)

        assert transfer in [:ok, {:error, :closed}]
        assert Task.await(closer) == :ok
        assert {:ok, 1} = UnixFD.__close_count__(read)

        Process.exit(target, :kill)
        :ok = UnixFD.close(write)
      end
    end
  end

  # Waits for close(2), then for every process whose exit could still close
  # the handle, and checks that close(2) ran exactly once, so that a late
  # second close would not go unseen.
  defp assert_closed_once(handle, pids) do
    assert eventually(fn -> UnixFD.__close_count__(handle) == {:ok, 1} end)

    for pid <- pids do
      ref = Process.monitor(pid)
      assert_receive {:DOWN, ^ref, :process, ^pid, _reason}
    end

    assert {:ok, 1} = UnixFD.__close_count__(handle)
  end

  # A pipe read end created, measured and dropped, so only GC can close it.
  defp lost_pipe_end do
    {:ok, read, write} = UnixFD.__pipe__()
    :ok = UnixFD.close(write)
    {:ok, fd} = UnixFD.with_fd(read, & &1)
    {:ok, identity} = UnixFD.__fd_identity__(fd)
    {fd, identity}
  end

  # A pipe read end owned by another process, which exits on `:exit` and
  # transfers the handle on `{:transfer, pid, reply_to}`.
  defp owned_elsewhere do
    parent = self()

    owner =
      spawn(fn ->
        {:ok, read, write} = UnixFD.__pipe__()
        :ok = UnixFD.close(write)
        send(parent, {:handle, self(), read})
        owner_loop(read)
      end)

    assert_receive {:handle, ^owner, handle}
    {owner, handle}
  end

  defp owner_loop(handle) do
    receive do
      :exit ->
        :ok

      {:transfer, pid, reply_to} ->
        send(reply_to, {:transferred, UnixFD.controlling_process(handle, pid)})
        owner_loop(handle)
    end
  end

  # A process that holds an alias of `handle` until killed.
  defp spawn_holder(handle) do
    spawn(fn -> holder_loop(handle) end)
  end

  defp holder_loop(handle) do
    receive do
      {:open?, reply_to} ->
        send(reply_to, {:open?, UnixFD.open?(handle)})
        holder_loop(handle)
    end
  end

  # A process blocked inside with_fd/2 on `handle` until it receives
  # `:release`.
  defp pin_in_process(handle) do
    parent = self()

    pid =
      spawn(fn ->
        UnixFD.with_fd(handle, fn _fd ->
          send(parent, {:pinned, self()})

          receive do
            :release -> :ok
          end
        end)
      end)

    assert_receive {:pinned, ^pid}
    pid
  end

  # Opens pipes until one end has `number`, closing the rest.
  defp pipe_reusing!(number, attempts \\ 20)

  defp pipe_reusing!(_number, 0), do: flunk("the kernel did not reuse the descriptor number")

  defp pipe_reusing!(number, attempts) do
    {:ok, read, write} = UnixFD.__pipe__()
    {:ok, read_fd} = UnixFD.with_fd(read, & &1)
    {:ok, write_fd} = UnixFD.with_fd(write, & &1)

    cond do
      read_fd == number ->
        {read, write}

      write_fd == number ->
        {write, read}

      true ->
        result = pipe_reusing!(number, attempts - 1)
        Enum.each([read, write], &UnixFD.close/1)
        result
    end
  end

  defp dead_pid do
    {pid, ref} = spawn_monitor(fn -> :ok end)
    assert_receive {:DOWN, ^ref, :process, ^pid, _}
    pid
  end

  @remote_node "elsewhere@nohost"

  # A pid and a reference from another node, as distribution would decode
  # them: NEW_PID_EXT and NEWER_REFERENCE_EXT in the external term format.
  defp remote_pid do
    :erlang.binary_to_term(
      <<131, 88, 119, byte_size(@remote_node), @remote_node::binary, 1::32, 0::32, 1::32>>
    )
  end

  defp remote_ref do
    :erlang.binary_to_term(
      <<131, 90, 3::16, 119, byte_size(@remote_node), @remote_node::binary, 1::32, 1::32, 2::32,
        3::32>>
    )
  end

  defp eventually(fun, attempts \\ 400)

  defp eventually(fun, attempts) when attempts > 0 do
    if fun.() do
      true
    else
      Process.sleep(5)
      eventually(fun, attempts - 1)
    end
  end

  defp eventually(_fun, 0), do: false
end
