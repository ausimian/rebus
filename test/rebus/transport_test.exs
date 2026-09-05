defmodule Rebus.TransportTest do
  use ExUnit.Case, async: true

  alias Rebus.Transport.Socket

  test "delegates every callback to the OTP socket module" do
    {:ok, listener} = :socket.open(:inet, :stream, :default)
    on_exit(fn -> _ = :socket.close(listener) end)
    :ok = :socket.bind(listener, %{family: :inet, addr: {127, 0, 0, 1}, port: 0})
    :ok = :socket.listen(listener)
    {:ok, listen_addr} = :socket.sockname(listener)

    assert {:ok, client} = Socket.open(:inet, :stream, :default)
    assert Socket.setopt(client, {:otp, :rcvbuf}, 65_536) == :ok
    assert Socket.connect(client, listen_addr, 1_000) == :ok

    {:ok, peer} = :socket.accept(listener, 1_000)
    on_exit(fn -> _ = :socket.close(peer) end)

    assert Socket.send(client, "ping", [], 1_000) == :ok
    assert {:ok, "ping"} = Socket.recv(peer, 4, [], 1_000)

    assert Socket.sendmsg(peer, %{iov: ["pong"]}, [], 1_000) == :ok
    assert {:ok, %{iov: ["pong"]}} = Socket.recvmsg(client, 4, 0, [], 1_000)

    assert {:select, {:select_info, :recv, _handle} = select_info} =
             Socket.recv(client, 1, [], :nowait)

    assert Socket.cancel(client, select_info) == :ok

    assert Socket.close(client) == :ok
    assert {:error, :closed} = Socket.send(client, "gone", [], 1_000)
  end
end
