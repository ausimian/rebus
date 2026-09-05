defmodule Rebus.Connection.Peer do
  @moduledoc false

  # The service side of an otherwise client-only connection. Rebus has no
  # service-side API, so every inbound method call is answered here rather than
  # handed to an application: `org.freedesktop.DBus.Peer` is implemented and
  # everything else is refused with `UnknownMethod`.
  #
  # Like `Rebus.Connection.Dispatch` this works on the connection struct
  # directly — the machine-id cache and the writer queue both live there — but
  # it decides nothing about framing, so it answers with a connection rather
  # than a `t:Rebus.Connection.Dispatch.result/0`.

  alias Rebus.Connection
  alias Rebus.Connection.Writer
  alias Rebus.MachineId
  alias Rebus.Message

  require Logger

  # Every D-Bus connection is expected to implement org.freedesktop.DBus.Peer;
  # dbus-daemon, busctl and d-feet all call it. Every other inbound method call
  # is refused so a caller fails immediately instead of waiting for its own
  # timeout.
  @peer_interface "org.freedesktop.DBus.Peer"
  @unknown_method_error "org.freedesktop.DBus.Error.UnknownMethod"
  @failed_error "org.freedesktop.DBus.Error.Failed"
  @unknown_method_message "Method not handled by this connection"
  @machine_id_unavailable_message "Machine ID unavailable"

  # A caller that asked for no reply gets none.
  @doc false
  @spec answer(Message.t(), Connection.t()) :: Connection.t()
  def answer(%Message{flags: flags} = msg, %Connection{} = state) do
    cond do
      :no_reply_expected in flags ->
        state

      Writer.replies_saturated?(state.writer) ->
        %{state | writer: Writer.refuse_reply(state.writer)}

      true ->
        {reply_opts, state} = method_call_reply(msg, state)
        queue_method_call_reply(reply_opts, msg, state)
    end
  end

  defp method_call_reply(%Message{header_fields: header_fields}, %Connection{} = state) do
    interface = Map.get(header_fields, :interface)
    member = Map.get(header_fields, :member)

    case {interface, member} do
      {interface, "Ping"} when interface in [nil, @peer_interface] ->
        {[type: :method_return], state}

      {interface, "GetMachineId"} when interface in [nil, @peer_interface] ->
        machine_id_reply(state)

      _other ->
        {unknown_method_reply(), state}
    end
  end

  defp machine_id_reply(%Connection{} = state) do
    case machine_id(state) do
      {{:ok, id}, state} ->
        {[type: :method_return, signature: "s", body: [id]], state}

      {{:error, :unavailable}, state} ->
        {[
           type: :error,
           error_name: @failed_error,
           signature: "s",
           body: [@machine_id_unavailable_message]
         ], state}
    end
  end

  # The reply body never echoes caller-supplied data: a fixed sentence keeps
  # peer-controlled bytes out of the frames this connection emits.
  defp unknown_method_reply do
    [
      type: :error,
      error_name: @unknown_method_error,
      signature: "s",
      body: [@unknown_method_message]
    ]
  end

  # The machine id is read on first use and then cached for the connection's
  # life, including a negative result: a peer that floods GetMachineId must not
  # turn into a stream of file reads.
  defp machine_id(%Connection{machine_id: nil} = state) do
    case MachineId.read() do
      {:ok, id} -> {{:ok, id}, %{state | machine_id: id}}
      {:error, :unavailable} -> {{:error, :unavailable}, %{state | machine_id: :unavailable}}
    end
  end

  defp machine_id(%Connection{machine_id: :unavailable} = state),
    do: {{:error, :unavailable}, state}

  defp machine_id(%Connection{machine_id: id} = state), do: {{:ok, id}, state}

  defp queue_method_call_reply(reply_opts, %Message{} = msg, %Connection{} = state) do
    {type, reply_opts} = Keyword.pop!(reply_opts, :type)
    reply_opts = reply_opts ++ [reply_serial: msg.serial] ++ reply_destination(msg)

    case Message.new(type, reply_opts) do
      {:ok, reply} ->
        writer =
          Writer.queue(state.writer, %{
            kind: :reply,
            from: nil,
            msg: reply,
            deadline: System.monotonic_time(:millisecond) + state.write_timeout,
            request_ref: make_ref()
          })

        kick_writes(%{state | writer: writer})

      {:error, reason} ->
        Logger.warning("D-Bus internal reply dropped: #{inspect(reason)}", reason: reason)
        state
    end
  end

  # A bus sets `sender` on every forwarded frame; a peer-to-peer caller has no
  # name, and its reply carries no destination. The name is copied so a small
  # retained header cannot pin the receive buffer it was parsed from.
  defp reply_destination(%Message{header_fields: %{sender: sender}}) when is_binary(sender),
    do: [destination: :binary.copy(sender)]

  defp reply_destination(%Message{}), do: []

  # The inbound path owns its own continuation (it must keep parsing coalesced
  # frames and re-arm the reader), so it cannot return the writer's. This
  # self-message starts the writer instead, after the current callback returns.
  defp kick_writes(%Connection{} = state) do
    send(self(), :advance_writes)
    state
  end
end
