defmodule Rebus.TestFD do
  @moduledoc false

  # Raw descriptors for tests which drive code that closes them.
  #
  # Never take a descriptor with `:socket.getopt(sock, {:otp, :fd})` on a
  # socket the test keeps and then let the code under test close the number.
  # The `:socket` handle still owns it, so once the number is closed the kernel
  # is free to hand it to the next socket opened anywhere in the VM - a
  # concurrently running async test, say - which is then closed underneath its
  # owner, and the original handle's eventual close or garbage collection
  # closes whatever now occupies the number. Both failure modes have been seen
  # in this suite as `:ebadf` and `:econnrefused` in unrelated tests.
  #
  # `dup!/0` avoids that by receiving a descriptor over SCM_RIGHTS: the kernel
  # installs a fresh number in this process which no `:socket` handle refers
  # to, so the code under test is free to close it.

  @doc """
  A raw descriptor nothing else in the VM owns.

  A throwaway socket is sent over a local socket pair with SCM_RIGHTS and the
  received dup is kept; the pair and the throwaway are closed through their own
  handles. The caller (or the code under test) owns the returned number and is
  responsible for closing it.
  """
  @spec dup!() :: non_neg_integer()
  def dup! do
    # `/tmp` rather than `System.tmp_dir!/0`: macOS caps a local socket path at
    # around 104 bytes, and `$TMPDIR` there is already most of that.
    path = Path.join("/tmp", "rebus-test-fd-#{System.unique_integer([:positive])}")
    {:ok, listener} = :socket.open(:local, :stream, :default)
    :ok = :socket.bind(listener, %{family: :local, path: path})
    :ok = :socket.listen(listener, 1)
    {:ok, sender} = :socket.open(:local, :stream, :default)
    {:ok, address} = :socket.sockname(listener)
    :ok = :socket.connect(sender, address)
    {:ok, receiver} = :socket.accept(listener, 1_000)

    # A socket opened purely to be duplicated, so the pair carrying it is not
    # itself the thing the caller ends up owning.
    {:ok, throwaway} = :socket.open(:local, :stream, :default)
    {:ok, fd} = :socket.getopt(throwaway, {:otp, :fd})

    :ok =
      :socket.sendmsg(
        sender,
        %{
          iov: ["x"],
          ctrl: [%{level: :socket, type: :rights, data: <<fd::native-signed-32>>}]
        },
        [],
        1_000
      )

    {:ok, %{ctrl: ctrl}} = :socket.recvmsg(receiver, 0, 256, [], 1_000)

    [%{data: <<received::native-signed-32>>}] =
      Enum.filter(ctrl, &match?(%{level: :socket, type: :rights}, &1))

    Enum.each([sender, receiver, listener, throwaway], &:socket.close/1)
    File.rm(path)

    received
  end

  @doc """
  The `skip` tag value for tests which need `dup!/0`.

  Answers `false` where SCM_RIGHTS is supported, and the reason to skip
  otherwise.
  """
  @spec skip_reason() :: false | String.t()
  def skip_reason do
    if :os.type() in [{:unix, :linux}, {:unix, :darwin}],
      do: false,
      else: "SCM_RIGHTS coverage is supported on Linux and macOS"
  end
end
