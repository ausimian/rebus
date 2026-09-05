defmodule Rebus.Transport.Socket do
  @moduledoc false

  # The production transport: OTP's `:socket` module, unmodified.

  @behaviour Rebus.Transport

  @impl Rebus.Transport
  def open(domain, type, protocol), do: :socket.open(domain, type, protocol)

  @impl Rebus.Transport
  def connect(socket, address, timeout), do: :socket.connect(socket, address, timeout)

  @impl Rebus.Transport
  def send(socket, data, flags_or_continuation, timeout),
    do: :socket.send(socket, data, flags_or_continuation, timeout)

  @impl Rebus.Transport
  def sendmsg(socket, message, flags_or_continuation, timeout),
    do: :socket.sendmsg(socket, message, flags_or_continuation, timeout)

  @impl Rebus.Transport
  def recv(socket, length, flags, timeout), do: :socket.recv(socket, length, flags, timeout)

  @impl Rebus.Transport
  def recvmsg(socket, length, control_size, flags, timeout),
    do: :socket.recvmsg(socket, length, control_size, flags, timeout)

  @impl Rebus.Transport
  def cancel(socket, select_info), do: :socket.cancel(socket, select_info)

  @impl Rebus.Transport
  def setopt(socket, option, value), do: :socket.setopt(socket, option, value)

  @impl Rebus.Transport
  def close(socket), do: :socket.close(socket)
end
