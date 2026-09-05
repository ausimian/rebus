defmodule Rebus.Connection.RightsTest do
  use ExUnit.Case, async: true

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
    test "delivers descriptors with the frame they arrived with" do
      assert {:frame, "abc", [3]} = Rights.decide({:ok, [3]}, "abc", context())
    end

    test "delivers bytes without descriptors unchanged" do
      assert {:frame, "abc", []} = Rights.decide({:ok, []}, "abc", context())
    end

    test "delivers empty bytes without descriptors" do
      assert {:frame, <<>>, []} = Rights.decide({:ok, []}, <<>>, context())
    end

    test "quarantines rights received before the transport was negotiated" do
      assert {:quarantine, "abc", [3]} =
               Rights.decide({:ok, [3]}, "abc", context(negotiated?: false))
    end

    test "fails closed on unnegotiated rights with no bytes to quarantine" do
      assert {:stop, :invalid_unix_fds, [3]} =
               Rights.decide({:ok, [3]}, <<>>, context(negotiated?: false))
    end

    test "fails closed on a rights-only result" do
      assert {:stop, :invalid_unix_fds, [3]} = Rights.decide({:ok, [3]}, <<>>, context())
    end

    test "quarantines rights arriving mid-frame" do
      assert {:quarantine, "abc", [3]} =
               Rights.decide({:ok, [3]}, "abc", context(frame_pending?: true))
    end

    test "fails closed on truncated control data even with recoverable bytes" do
      assert {:stop, :unix_fd_truncated, [3]} =
               Rights.decide({:error, :unix_fd_truncated, [3]}, "abc", context())
    end

    test "quarantines a malformed control result frame-locally" do
      assert {:quarantine, "abc", [3]} =
               Rights.decide({:error, :unix_fd_limit, [3]}, "abc", context())
    end

    test "fails closed on a malformed control result with no bytes" do
      assert {:stop, :invalid_unix_fds, [3]} =
               Rights.decide({:error, :unix_fd_limit, [3]}, <<>>, context())
    end
  end

  describe "decode/3" do
    test "reads iodata and control data from a recvmsg result" do
      message = %{iov: ["ab", "c"], ctrl: [rights_cmsg([3])], flags: []}

      assert {:frame, "abc", [3]} = Rights.decode(message, Rights.new(), context())
    end

    test "treats retained descriptors as a frame in progress" do
      message = %{iov: ["abc"], ctrl: [rights_cmsg([4])], flags: []}
      rights = Rights.retain(Rights.new(), [3])

      assert {:quarantine, "abc", [4]} = Rights.decode(message, rights, context())
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

  describe "attach/3" do
    test "attaches retained descriptors to the frame that completed" do
      rights = Rights.retain(Rights.new(), [3])

      assert {:ok, %Message{unix_fds: [3]}, attached} = Rights.attach(rights, message(1), true)
      refute Rights.holding?(attached)
    end

    test "drops a quarantined frame and names its descriptors" do
      rights = Rights.new() |> Rights.retain([3]) |> Rights.taint()

      assert {:error, :invalid_unix_fds, [3], attached} = Rights.attach(rights, message(1), true)
      refute Rights.holding?(attached)
      refute attached.tainted?
    end

    test "rejects a descriptor-bearing frame when the transport was not negotiated" do
      rights = Rights.retain(Rights.new(), [3])

      assert {:error, :unix_fd_not_negotiated, [3], _attached} =
               Rights.attach(rights, message(1), false)
    end

    test "accepts a descriptor-free frame when the transport was not negotiated" do
      assert {:ok, %Message{unix_fds: []}, _attached} =
               Rights.attach(Rights.new(), message(0), false)
    end

    test "rejects a count which disagrees with the header field" do
      rights = Rights.retain(Rights.new(), [3, 4])

      assert {:error, :invalid_unix_fds, [3, 4], _attached} =
               Rights.attach(rights, message(1), true)
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
    %{negotiated?: true, frame_pending?: false, max_bytes: @max_read_chunk}
    |> Map.merge(Map.new(overrides))
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
