defmodule Rebus.Connection.RightsTest do
  use ExUnit.Case, async: true

  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Rights
  alias Rebus.Message

  @max_read_chunk 65_536

  # Every descriptor here is a plain integer: nothing in this module opens,
  # closes or dereferences one, which is exactly the property under test.

  describe "decode_rights_data/1" do
    test "decodes a well-formed payload in wire order" do
      assert {:ok, [3, 4, 5]} = Rights.decode_rights_data(rights_data([3, 4, 5]))
    end

    test "decodes an empty payload" do
      assert {:ok, []} = Rights.decode_rights_data(<<>>)
    end

    test "rejects a negative descriptor but retains what it decoded" do
      assert {:error, [3, -1]} = Rights.decode_rights_data(rights_data([3, -1]))
    end

    test "rejects a ragged tail and retains every complete descriptor" do
      assert {:error, [3, 4]} = Rights.decode_rights_data(rights_data([3, 4]) <> <<0, 0>>)
    end

    test "rejects a payload that is only a ragged tail" do
      assert {:error, []} = Rights.decode_rights_data(<<0, 0, 0>>)
    end
  end

  describe "extract_rights_fds/1" do
    test "concatenates descriptors across cmsgs in order" do
      ctrl = [rights_cmsg([3, 4]), rights_cmsg([5])]

      assert {:ok, [3, 4, 5]} = Rights.extract_rights_fds(ctrl)
    end

    test "ignores cmsgs which are not SCM_RIGHTS" do
      ctrl = [
        %{level: :socket, type: :timestamp, data: <<0::64>>},
        rights_cmsg([7]),
        %{level: :ip, type: :ttl, data: <<64>>}
      ]

      assert {:ok, [7]} = Rights.extract_rights_fds(ctrl)
    end

    test "fails closed on a non-binary payload but keeps scanning later cmsgs" do
      ctrl = [%{level: :socket, type: :rights, data: :not_a_binary}, rights_cmsg([9, 10])]

      assert {:error, :invalid_unix_fds, [9, 10]} = Rights.extract_rights_fds(ctrl)
    end

    test "retains descriptors decoded before a malformed tail" do
      ctrl = [%{level: :socket, type: :rights, data: rights_data([3]) <> <<1>>}, rights_cmsg([4])]

      assert {:error, :invalid_unix_fds, [3, 4]} = Rights.extract_rights_fds(ctrl)
    end

    test "accepts exactly the descriptor limit" do
      fds = Enum.to_list(1..Message.max_unix_fds())

      assert {:ok, ^fds} = Rights.extract_rights_fds([rights_cmsg(fds)])
    end

    test "reports the descriptor limit across cmsgs and retains every descriptor" do
      fds = Enum.to_list(1..(Message.max_unix_fds() + 1))
      {head, tail} = Enum.split(fds, Message.max_unix_fds())

      assert {:error, :unix_fd_limit, ^fds} =
               Rights.extract_rights_fds([rights_cmsg(head), rights_cmsg(tail)])
    end

    test "returns no descriptors for empty control data" do
      assert {:ok, []} = Rights.extract_rights_fds([])
    end
  end

  describe "recvmsg_fds/2" do
    test "passes a clean result through untouched" do
      assert {:ok, [3]} = Rights.recvmsg_fds([rights_cmsg([3])], [])
    end

    test "fails closed on MSG_CTRUNC, retaining the descriptors it did decode" do
      assert {:error, :unix_fd_truncated, [3]} = Rights.recvmsg_fds([rights_cmsg([3])], [:ctrunc])
    end

    test "lets MSG_CTRUNC take precedence over a malformed payload" do
      ctrl = [%{level: :socket, type: :rights, data: rights_data([3]) <> <<1>>}]

      assert {:error, :unix_fd_truncated, [3]} = Rights.recvmsg_fds(ctrl, [:ctrunc])
    end
  end

  describe "decide/3" do
    test "holds descriptors for the frames beginning in their bytes" do
      assert {:frame, "abc", [3]} = Rights.decide({:ok, [3]}, "abc", context())
    end

    test "delivers bytes without descriptors unchanged" do
      assert {:frame, "abc", []} = Rights.decide({:ok, []}, "abc", context())
    end

    test "delivers empty bytes without descriptors" do
      assert {:frame, <<>>, []} = Rights.decide({:ok, []}, <<>>, context())
    end

    test "quarantines rights received before the transport was negotiated" do
      assert {:quarantine, "abc", [3], :unix_fd_not_negotiated} =
               Rights.decide({:ok, [3]}, "abc", context(negotiated?: false))
    end

    test "fails closed on unnegotiated rights with no bytes to quarantine" do
      assert {:stop, :invalid_unix_fds, [3]} =
               Rights.decide({:ok, [3]}, <<>>, context(negotiated?: false))
    end

    test "fails closed on a rights-only result" do
      assert {:stop, :invalid_unix_fds, [3]} = Rights.decide({:ok, [3]}, <<>>, context())
    end

    test "taints the frame being assembled when no frame begins in the bytes" do
      assert {:taint, "abc", [3]} =
               Rights.decide({:ok, [3]}, "abc", context(starts_frame?: false))
    end

    test "fails closed on truncated control data even with recoverable bytes" do
      assert {:stop, :unix_fd_truncated, [3]} =
               Rights.decide({:error, :unix_fd_truncated, [3]}, "abc", context())
    end

    test "quarantines a malformed control result for the frames beginning in its bytes" do
      assert {:quarantine, "abc", [3], :unix_fd_limit} =
               Rights.decide({:error, :unix_fd_limit, [3]}, "abc", context())
    end

    test "taints the frame being assembled with a malformed result inside it" do
      assert {:taint, "abc", [3]} =
               Rights.decide({:error, :unix_fd_limit, [3]}, "abc", context(starts_frame?: false))
    end

    test "fails closed on a malformed control result with no bytes" do
      assert {:stop, :invalid_unix_fds, [3]} =
               Rights.decide({:error, :unix_fd_limit, [3]}, <<>>, context())
    end
  end

  describe "decode/3" do
    test "reads iodata and control data from a recvmsg result" do
      message = %{iov: ["ab", "c"], ctrl: [rights_cmsg([3])], flags: []}

      assert {:ok, "abc", rights, []} = Rights.decode(message, Rights.new(), context())
      assert Rights.fds(rights) == [3]
      assert rights.batches == [%{start: 0, stop: 3, count: 1, reason: nil}]
    end

    test "holds nothing for a result without descriptors" do
      message = %{iov: ["abc"], ctrl: [], flags: []}

      assert {:ok, "abc", rights, []} = Rights.decode(message, Rights.new(), context())
      assert rights == Rights.new()
    end

    test "places a batch after the bytes already buffered" do
      frame = encoded_frame()
      inbound = Inbound.new(binary_part(frame, 0, 16))
      data = binary_part(frame, 16, byte_size(frame) - 16) <> "next"
      message = %{iov: [data], ctrl: [rights_cmsg([3])], flags: []}

      assert {:ok, ^data, rights, []} =
               Rights.decode(message, Rights.new(), context(inbound: inbound))

      stop = 16 + byte_size(data)
      assert [%{start: 16, stop: ^stop, count: 1, reason: nil}] = rights.batches
    end

    test "taints the frame being assembled when no frame begins in the bytes" do
      frame = encoded_frame()
      inbound = Inbound.new(binary_part(frame, 0, 16))
      message = %{iov: [binary_part(frame, 16, 1)], ctrl: [rights_cmsg([4])], flags: []}

      assert {:ok, _data, rights, [4]} =
               Rights.decode(message, Rights.new(), context(inbound: inbound))

      assert rights.tainted?
      refute Rights.holding?(rights)
      assert rights.batches == []
    end

    test "quarantines rejected rights for the frames beginning in the bytes" do
      fds = Enum.to_list(3..19)
      message = %{iov: ["abc"], ctrl: [rights_cmsg(fds)], flags: []}

      assert {:ok, "abc", rights, ^fds} = Rights.decode(message, Rights.new(), context())
      refute Rights.holding?(rights)
      assert Rights.quarantined?(rights)
      assert rights.batches == [%{start: 0, stop: 3, count: 0, reason: :unix_fd_limit}]
    end

    test "validates control data before iodata so descriptors cannot leak" do
      message = %{iov: [:not_iodata], ctrl: [rights_cmsg([3])], flags: []}

      assert {:stop, :invalid_unix_fds, [3]} = Rights.decode(message, Rights.new(), context())
    end

    test "fails closed on an oversized read, naming the descriptors to close" do
      message = %{
        iov: [:binary.copy("x", @max_read_chunk + 1)],
        ctrl: [rights_cmsg([3])],
        flags: []
      }

      assert {:stop, :message_too_large, [3]} = Rights.decode(message, Rights.new(), context())
    end

    test "fails closed on a recvmsg result with no iodata" do
      message = %{ctrl: [rights_cmsg([3])], flags: []}

      assert {:stop, :invalid_unix_fds, [3]} = Rights.decode(message, Rights.new(), context())
    end

    test "fails closed on a recvmsg result with no flags" do
      message = %{ctrl: [rights_cmsg([3])]}

      assert {:stop, :invalid_unix_fds, [3]} = Rights.decode(message, Rights.new(), context())
    end

    test "fails closed on an unrecognised recvmsg result" do
      assert {:stop, :invalid_unix_fds, []} = Rights.decode(:garbage, Rights.new(), context())
    end
  end

  describe "attach/4" do
    test "attaches retained descriptors to the frame that completed" do
      rights = Rights.retain(Rights.new(), [3])

      assert {:ok, %Message{unix_fds: [3]}, attached} =
               Rights.attach(rights, message(1), 40, true)

      refute Rights.holding?(attached)
      assert attached.offset == 40
    end

    test "drops a tainted frame and names the descriptors it claimed" do
      rights = Rights.new() |> Rights.retain([3]) |> Rights.taint()

      assert {:error, :invalid_unix_fds, [3], attached} =
               Rights.attach(rights, message(1), 40, true)

      refute Rights.holding?(attached)
      refute attached.tainted?
    end

    test "drops a tainted frame that declares no descriptors" do
      rights = Rights.taint(Rights.new())

      assert {:error, :invalid_unix_fds, [], attached} =
               Rights.attach(rights, message(0), 40, true)

      refute attached.tainted?
    end

    test "rejects a descriptor-bearing frame when the transport was not negotiated" do
      rights = Rights.retain(Rights.new(), [3])

      assert {:error, :unix_fd_not_negotiated, [3], _attached} =
               Rights.attach(rights, message(1), 40, false)
    end

    test "accepts a descriptor-free frame when the transport was not negotiated" do
      assert {:ok, %Message{unix_fds: []}, _attached} =
               Rights.attach(Rights.new(), message(0), 40, false)
    end

    test "rejects a frame declaring more descriptors than its batch holds" do
      rights = Rights.retain(Rights.new(), [3])

      assert {:error, :invalid_unix_fds, [3], attached} =
               Rights.attach(rights, message(2), 40, true)

      refute Rights.holding?(attached)
    end

    test "rejects a frame declaring descriptors when none arrived with it" do
      assert {:error, :invalid_unix_fds, [], _attached} =
               Rights.attach(Rights.new(), message(1), 40, true)
    end

    test "claims in order across the frames beginning in one read" do
      rights = received([3, 4, 5], 100)

      assert {:ok, %Message{unix_fds: []}, rights} = Rights.attach(rights, message(0), 20, true)
      assert {:ok, %Message{unix_fds: [3]}, rights} = Rights.attach(rights, message(1), 30, true)

      assert {:ok, %Message{unix_fds: [4, 5]}, rights} =
               Rights.attach(rights, message(2), 50, true)

      refute Rights.holding?(rights)
      assert {[], %Rights{batches: []}} = Rights.expire(rights)
    end

    test "never carries a batch into a frame that begins after its bytes" do
      rights = received([3], 30)

      assert {:ok, %Message{unix_fds: []}, rights} = Rights.attach(rights, message(0), 30, true)
      # The next frame begins where the read ended, so the batch is closed
      # unclaimed rather than offered to it.
      assert {[3], rights} = Rights.expire(rights)
      refute Rights.holding?(rights)

      assert {:error, :invalid_unix_fds, [], _rights} =
               Rights.attach(rights, message(1), 30, true)
    end

    test "keeps a batch while a frame may still begin in it" do
      rights = received([3], 30)

      assert {:ok, %Message{unix_fds: []}, rights} = Rights.attach(rights, message(0), 20, true)
      assert {[], rights} = Rights.expire(rights)
      assert {:ok, %Message{unix_fds: [3]}, _rights} = Rights.attach(rights, message(1), 60, true)
    end

    test "drops a claimant of a quarantined batch but not a frame declaring none" do
      {:ok, _data, rights, _rejected} =
        Rights.decode(
          %{iov: [:binary.copy("x", 60)], ctrl: [rights_cmsg(Enum.to_list(3..19))], flags: []},
          Rights.new(),
          context()
        )

      assert {:ok, %Message{unix_fds: []}, rights} = Rights.attach(rights, message(0), 30, true)

      assert {:error, :unix_fd_limit, [], _rights} =
               Rights.attach(rights, message(1), 30, true)
    end
  end

  describe "skip/2" do
    test "closes the rest of a skipped frame's batch and poisons it" do
      rights = received([3, 4], 60)

      assert {[3, 4], rights} = Rights.skip(rights, 30)
      refute Rights.holding?(rights)
      assert Rights.quarantined?(rights)

      assert {:error, :invalid_unix_fds, [], _rights} =
               Rights.attach(rights, message(1), 30, true)
    end

    test "advances past a frame outside any batch" do
      assert {[], %Rights{offset: 30}} = Rights.skip(Rights.new(), 30)
    end
  end

  describe "buffer" do
    test "retains descriptors and reports holding them" do
      refute Rights.holding?(Rights.new())
      assert Rights.fds(Rights.new()) == []

      rights = Rights.retain(Rights.new(), [3])
      assert Rights.holding?(rights)
      assert Rights.fds(rights) == [3]
    end

    test "leaves the buffer untouched when there is nothing to retain" do
      rights = Rights.retain(Rights.new(), [3])

      assert Rights.retain(rights, []) == rights
    end

    test "taints a buffer without adding descriptors" do
      assert Rights.new() |> Rights.taint() |> Rights.fds() == []
      assert Rights.taint(Rights.new()).tainted?
      assert Rights.quarantined?(Rights.taint(Rights.new()))
      refute Rights.quarantined?(Rights.retain(Rights.new(), [3]))
    end
  end

  describe "drop_reason/1" do
    test "passes recoverable reasons through" do
      for reason <- [:invalid_unix_fds, :unix_fd_not_negotiated, :unix_fd_limit] do
        assert Rights.drop_reason(reason) == reason
      end
    end

    test "maps anything else onto the generic reason" do
      assert Rights.drop_reason(:something_else) == :invalid_unix_fds
    end
  end

  defp context(overrides \\ []) do
    %{negotiated?: true, starts_frame?: true, inbound: Inbound.new(), max_bytes: @max_read_chunk}
    |> Map.merge(Map.new(overrides))
  end

  # Rights holding one batch received with `size` bytes into an empty buffer,
  # so every frame beginning in those bytes may claim from it.
  defp received(fds, size) do
    message = %{iov: [:binary.copy("x", size)], ctrl: [rights_cmsg(fds)], flags: []}
    {:ok, _data, rights, []} = Rights.decode(message, Rights.new(), context())
    rights
  end

  defp encoded_frame do
    {:ok, encoded} = Message.encode(%{message(0) | serial: 1})
    IO.iodata_to_binary(encoded)
  end

  defp rights_data(fds), do: for(fd <- fds, into: <<>>, do: <<fd::native-signed-32>>)

  defp rights_cmsg(fds), do: %{level: :socket, type: :rights, data: rights_data(fds)}

  defp message(0) do
    Message.new!(:signal, path: "/test", interface: "test.interface", member: "NoFD")
  end

  defp message(count) do
    Message.new!(:signal,
      path: "/test",
      interface: "test.interface",
      member: "FD",
      signature: String.duplicate("h", count),
      body: List.duplicate(0, count),
      fds: List.duplicate(0, count)
    )
    |> Map.put(:unix_fds, [])
  end
end
