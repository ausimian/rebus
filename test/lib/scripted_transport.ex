defmodule Rebus.ScriptedTransport do
  @moduledoc false

  # A transport whose "socket" is an agent holding a scripted list of receive
  # results and the bytes written to it. The handshake never inspects the
  # socket term it is handed, so a line-protocol test needs no real socket:
  # `start/1` returns the agent, `recv/4` replays the next scripted step and
  # `sent/1` returns everything written so far.
  #
  # A script step is either a binary, delivered as `{:ok, binary}`, or any
  # other term, returned to the caller unchanged. An exhausted script reports
  # `{:error, :closed}`, exactly as a peer that hung up would.

  @behaviour Rebus.Transport

  @spec start([term()]) :: pid()
  def start(script) when is_list(script) do
    {:ok, agent} = Agent.start_link(fn -> %{script: script, sent: []} end)
    agent
  end

  @doc """
  Returns every byte written to `sock`, in order.
  """
  @spec sent(pid()) :: binary()
  def sent(sock) do
    Agent.get(sock, fn %{sent: sent} -> sent |> Enum.reverse() |> IO.iodata_to_binary() end)
  end

  @doc """
  Returns the script steps `sock` has not replayed yet.
  """
  @spec remaining(pid()) :: [term()]
  def remaining(sock), do: Agent.get(sock, & &1.script)

  @impl Rebus.Transport
  def send(sock, data, _flags, _timeout) do
    Agent.update(sock, fn state ->
      %{state | sent: [IO.iodata_to_binary(data) | state.sent]}
    end)
  end

  @impl Rebus.Transport
  def recv(sock, _length, _flags, _timeout) do
    Agent.get_and_update(sock, fn
      %{script: [step | script]} = state -> {step(step), %{state | script: script}}
      %{script: []} = state -> {{:error, :closed}, state}
    end)
  end

  defp step(data) when is_binary(data), do: {:ok, data}
  defp step(result), do: result

  @impl Rebus.Transport
  def open(_domain, _type, _protocol), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def connect(_sock, _address, _timeout), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def sendmsg(_sock, _message, _flags, _timeout), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def recvmsg(_sock, _length, _control_size, _flags, _timeout), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def cancel(_sock, _select_info), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def setopt(_sock, _option, _value), do: {:error, :not_scripted}

  @impl Rebus.Transport
  def close(sock), do: Agent.stop(sock)
end
