defmodule Rebus.ScriptedTransport do
  @moduledoc false

  # A transport whose "socket" is an agent holding scripted results and a log of
  # what was written to it. Neither the handshake nor the writer inspects the
  # socket term it is handed, so a line-protocol or write-queue test needs no
  # real socket: `start/1` returns the agent and `sent/1` returns everything
  # written so far.
  #
  # `start/1` takes either a bare list, which scripts `recv/4` alone, or a map
  # of `:recv`, `:send` and `:sendmsg` scripts. A script step is either a
  # function, applied to the payload of the call it answers, or a plain term.
  # A plain `recv/4` step that is a binary is delivered as `{:ok, binary}`; any
  # other term is returned unchanged.
  #
  # An exhausted `recv/4` script reports `{:error, :closed}`, exactly as a peer
  # that hung up would; an exhausted `send/4` or `sendmsg/4` script accepts the
  # whole payload. Every write attempt and every cancellation is recorded in
  # order and readable with `writes/1`.

  @behaviour Rebus.Transport

  @spec start([term()] | map()) :: pid()
  def start(script) when is_list(script), do: start(%{recv: script})

  def start(script) when is_map(script) do
    state = %{
      recv: Map.get(script, :recv, []),
      send: Map.get(script, :send, []),
      sendmsg: Map.get(script, :sendmsg, []),
      sent: [],
      writes: []
    }

    {:ok, agent} = Agent.start_link(fn -> state end)
    agent
  end

  @doc """
  Returns every byte handed to `send/4`, in order.
  """
  @spec sent(pid()) :: binary()
  def sent(sock) do
    Agent.get(sock, fn %{sent: sent} -> sent |> Enum.reverse() |> IO.iodata_to_binary() end)
  end

  @doc """
  Returns the write attempts made on `sock`, in order, as `{:send, data}`,
  `{:sendmsg, message}` and `{:cancel, select_info}` entries.
  """
  @spec writes(pid()) :: [{:send | :sendmsg | :cancel, term()}]
  def writes(sock), do: Agent.get(sock, &Enum.reverse(&1.writes))

  @doc """
  Returns the `recv/4` script steps `sock` has not replayed yet.
  """
  @spec remaining(pid()) :: [term()]
  def remaining(sock), do: Agent.get(sock, & &1.recv)

  @impl Rebus.Transport
  def send(sock, data, _flags, _timeout) do
    data = IO.iodata_to_binary(data)

    Agent.get_and_update(sock, fn state ->
      state = %{state | sent: [data | state.sent], writes: [{:send, data} | state.writes]}

      case state.send do
        [step | script] -> {apply_step(step, data), %{state | send: script}}
        [] -> {:ok, state}
      end
    end)
  end

  @impl Rebus.Transport
  def sendmsg(sock, message, _flags, _timeout) do
    Agent.get_and_update(sock, fn state ->
      state = %{state | writes: [{:sendmsg, message} | state.writes]}

      case state.sendmsg do
        [step | script] -> {apply_step(step, message), %{state | sendmsg: script}}
        [] -> {:ok, state}
      end
    end)
  end

  @impl Rebus.Transport
  def cancel(sock, select_info) do
    Agent.update(sock, fn state ->
      %{state | writes: [{:cancel, select_info} | state.writes]}
    end)
  end

  @impl Rebus.Transport
  def recv(sock, _length, _flags, _timeout) do
    Agent.get_and_update(sock, fn
      %{recv: [step | script]} = state -> {recv_step(step), %{state | recv: script}}
      %{recv: []} = state -> {{:error, :closed}, state}
    end)
  end

  defp recv_step(data) when is_binary(data), do: {:ok, data}
  defp recv_step(result), do: result

  defp apply_step(step, payload) when is_function(step, 1), do: step.(payload)
  defp apply_step(step, _payload), do: step

  @impl Rebus.Transport
  def open(_domain, _type, _protocol), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def connect(_sock, _address, _timeout), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def recvmsg(_sock, _length, _control_size, _flags, _timeout), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def setopt(_sock, _option, _value), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def close(sock), do: Agent.stop(sock)
end
