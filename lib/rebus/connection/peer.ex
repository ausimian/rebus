defmodule Rebus.Connection.Peer do
  @moduledoc false

  # `org.freedesktop.DBus.Peer`, which every D-Bus connection is expected to
  # implement on every path; dbus-daemon, busctl and d-feet all call it.
  # `Rebus.Connection.Service` routes Peer calls here and queues the reply
  # this builds, so this module decides only what the answer is.
  #
  # Like `Rebus.Connection.Dispatch` this works on the connection struct
  # directly, because the machine-id cache lives there.

  alias Rebus.Connection
  alias Rebus.MachineId
  alias Rebus.Message

  @members ["Ping", "GetMachineId"]
  @unknown_method_error "org.freedesktop.DBus.Error.UnknownMethod"
  @failed_error "org.freedesktop.DBus.Error.Failed"
  @unknown_method_message "Method not handled by this connection"
  @machine_id_unavailable_message "Machine ID unavailable"

  # The members a call without an interface header falls back to.
  @doc false
  @spec member?(binary()) :: boolean()
  def member?(member), do: member in @members

  # The options for the reply to a Peer call, which the caller addresses and
  # queues.
  @doc false
  @spec reply(Message.t(), Connection.t()) :: {keyword(), Connection.t()}
  def reply(%Message{header_fields: header_fields}, %Connection{} = state) do
    case Map.get(header_fields, :member) do
      "Ping" -> {[type: :method_return], state}
      "GetMachineId" -> machine_id_reply(state)
      _other -> {unknown_method_reply(), state}
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
end
