defmodule Rebus.Connection.Service do
  @moduledoc false

  # The service side of a connection: routing each inbound method call to the
  # handler of the object it addresses, and every answer the connection gives
  # itself.
  #
  # A call for `org.freedesktop.DBus.Peer` is answered by
  # `Rebus.Connection.Peer` on any path, and `Introspect` from the export
  # table on any path it resolves. Anything else is resolved against the
  # export table, checked against the interface declaration, bounded by
  # `max_pending_calls`, and only then sent to the handler as
  # `{export_ref, %Rebus.Service.Call{}}`. The connection never runs
  # application code; it answers every call it refuses with a fixed-text
  # D-Bus error, and so the handler sees only calls that name a declared
  # member with arguments of the declared types.
  #
  # Calls that expect a reply are recorded in `Rebus.Connection.InboundCalls`
  # until their handler answers, they expire, or their export goes away. A
  # call sent with `NO_REPLY_EXPECTED` is not recorded: it carries no
  # descriptors in this slice, so nothing would be left to release, and
  # `Rebus.Service` answers it as a no-op in the handler process.
  #
  # Like `Rebus.Connection.Dispatch` this works on the connection struct
  # directly, but it decides nothing about framing, so it answers with a
  # connection.

  alias Rebus.Connection
  alias Rebus.Connection.InboundCalls
  alias Rebus.Connection.Objects
  alias Rebus.Connection.Objects.Export
  alias Rebus.Connection.Peer
  alias Rebus.Connection.Writer
  alias Rebus.Interface.Method
  alias Rebus.Message
  alias Rebus.Service.Call

  require Logger

  @peer_interface "org.freedesktop.DBus.Peer"
  @introspectable_interface "org.freedesktop.DBus.Introspectable"

  # Every refusal carries a fixed sentence. Member, interface and path names
  # are grammar-checked, but they are never echoed: one policy is easier to
  # audit than a list of fields that happen to be safe.
  @errors %{
    unknown_object:
      {"org.freedesktop.DBus.Error.UnknownObject", "No object is exported at this path"},
    unknown_interface:
      {"org.freedesktop.DBus.Error.UnknownInterface", "Interface not implemented by this object"},
    unknown_method:
      {"org.freedesktop.DBus.Error.UnknownMethod", "Method not implemented by this object"},
    invalid_args:
      {"org.freedesktop.DBus.Error.InvalidArgs", "Arguments do not match the method signature"},
    limits_exceeded:
      {"org.freedesktop.DBus.Error.LimitsExceeded", "Too many method calls are pending"},
    handler_exited: {"org.freedesktop.DBus.Error.Failed", "Handler exited"},
    unexported: {"org.freedesktop.DBus.Error.Failed", "Object unexported"},
    expired: {"org.freedesktop.DBus.Error.NoReply", "Handler did not reply in time"}
  }

  @type error :: unquote(Enum.reduce(Map.keys(@errors), &{:|, [], [&1, &2]}))

  # Routes one inbound method call. Descriptors that arrived with it have
  # already been closed by the caller.
  @doc false
  @spec answer(Message.t(), Connection.t()) :: Connection.t()
  def answer(%Message{} = msg, %Connection{} = state) do
    case route(msg, state.objects) do
      {:ok, export, interface, method} ->
        deliver(msg, export, interface, method, state)

      :peer ->
        answer_internally(msg, state, &Peer.reply/2)

      :introspect ->
        answer_internally(msg, state, &introspect/2)

      {:error, error} ->
        answer_internally(msg, state, fn _msg, state -> {error_reply(error), state} end)
    end
  end

  # Adds an export. The handler is monitored for as long as it lasts.
  @doc false
  @spec export(Connection.t(), map()) ::
          {{:ok, reference()} | {:error, {:already_exported, binary(), binary()}}, Connection.t()}
  def export(%Connection{} = state, %{ref: ref} = spec) do
    case Objects.add(state.objects, spec) do
      {:ok, objects} -> {{:ok, ref}, %{state | objects: objects}}
      {:error, _} = error -> {error, state}
    end
  end

  # Removes an export and fails every call still waiting on it. Removing an
  # export that is already gone is a no-op.
  @doc false
  @spec unexport(Connection.t(), reference()) :: Connection.t()
  def unexport(%Connection{} = state, ref) do
    case Objects.remove(state.objects, ref) do
      {nil, _objects} ->
        state

      {%Export{} = export, objects} ->
        fail_export(%{state | objects: objects}, export, :unexported)
    end
  end

  # A `:DOWN` for the monitor on an export's handler unexports it, failing
  # its calls; `:error` means the monitor is not one of the export table's.
  @doc false
  @spec handler_down(Connection.t(), reference()) :: {:ok, Connection.t()} | :error
  def handler_down(%Connection{} = state, monitor_ref) do
    case Objects.remove_by_monitor(state.objects, monitor_ref) do
      {nil, _objects} ->
        :error

      {%Export{} = export, objects} ->
        {:ok, fail_export(%{state | objects: objects}, export, :handler_exited)}
    end
  end

  # An expiry timer fired. One that fires after its call completed finds
  # nothing, which is expected: cancelling a timer cannot recall a message it
  # has already sent.
  @doc false
  @spec expire(Connection.t(), reference()) :: Connection.t()
  def expire(%Connection{} = state, ref) do
    case InboundCalls.expire(state.inbound_calls, ref) do
      {nil, _calls} -> state
      {entry, calls} -> fail(%{state | inbound_calls: calls}, entry, :expired)
    end
  end

  # The handler answered a call with `msg`. The call leaves the table, and
  # the reply is addressed from what the table retained rather than from
  # anything the handler supplied, so it can only answer the serial it was
  # handed and only to the peer that sent it.
  @doc false
  @spec complete(Connection.t(), reference(), Message.t()) ::
          {:ok, Message.t(), Connection.t()} | {:error, :unknown_call}
  def complete(%Connection{} = state, ref, %Message{} = msg) do
    case InboundCalls.complete(state.inbound_calls, ref) do
      {nil, _calls} ->
        {:error, :unknown_call}

      {entry, calls} ->
        {:ok, address(msg, entry.serial, entry.sender), %{state | inbound_calls: calls}}
    end
  end

  defp address(%Message{header_fields: fields} = msg, serial, sender) do
    fields =
      case sender do
        nil -> Map.delete(fields, :destination)
        sender -> Map.put(fields, :destination, sender)
      end

    %{msg | header_fields: Map.put(fields, :reply_serial, serial)}
  end

  # Routing order: Peer on any path; then the object at the path, where
  # `Introspect` is answered on objects and intermediate nodes alike; then,
  # for a call without an interface, the one exported interface that declares
  # the member, with Peer's own members tried only when none does; then the
  # argument signature. Every step that refuses names the error it answers with.
  defp route(%Message{header_fields: fields} = msg, %Objects{} = objects) do
    case Map.get(fields, :interface) do
      @peer_interface ->
        :peer

      interface ->
        objects
        |> Objects.resolve(fields.path)
        |> resolve_member(interface, fields.member)
        |> check_args(msg)
    end
  end

  # `Introspectable` cannot be exported, so no object's interfaces include it.
  defp resolve_member(resolution, @introspectable_interface, "Introspect")
       when resolution != :unknown,
       do: :introspect

  defp resolve_member({:object, _interfaces}, @introspectable_interface, _member),
    do: {:error, :unknown_method}

  defp resolve_member({:object, interfaces}, nil, member) do
    declared =
      for {name, %Export{} = export} <- interfaces,
          %Method{} = method <- [export.interfaces[name].methods[member]],
          do: {:ok, export, name, method}

    case declared do
      [routed] -> routed
      [] -> peer_or(member, :unknown_method)
      _ambiguous -> {:error, :unknown_method}
    end
  end

  defp resolve_member({:object, interfaces}, interface, member) do
    with {:ok, %Export{} = export} <- fetch(interfaces, interface, :unknown_interface),
         {:ok, method} <- fetch(export.interfaces[interface].methods, member, :unknown_method) do
      {:ok, export, interface, method}
    end
  end

  defp resolve_member(:intermediate, nil, member), do: peer_or(member, :unknown_interface)
  defp resolve_member(:intermediate, _interface, _member), do: {:error, :unknown_interface}
  defp resolve_member(:unknown, nil, member), do: peer_or(member, :unknown_object)
  defp resolve_member(:unknown, _interface, _member), do: {:error, :unknown_object}

  defp fetch(map, key, error) do
    case Map.fetch(map, key) do
      {:ok, value} -> {:ok, value}
      :error -> {:error, error}
    end
  end

  # A call without an interface header for `Ping` or `GetMachineId` has
  # always reached Peer, whatever its path.
  defp peer_or(member, error), do: if(Peer.member?(member), do: :peer, else: {:error, error})

  # The decoder has already read the body under the signature the frame
  # declares, so an equal signature means every value has the declared type.
  defp check_args({:ok, _export, _interface, %Method{in_signature: signature}} = routed, msg) do
    if Message.signature(msg) == signature, do: routed, else: {:error, :invalid_args}
  end

  defp check_args(:introspect, msg),
    do: if(Message.signature(msg) == "", do: :introspect, else: {:error, :invalid_args})

  defp check_args(refused, _msg), do: refused

  defp deliver(%Message{} = msg, %Export{} = export, interface, %Method{} = method, state) do
    cond do
      no_reply?(msg) ->
        send_call(msg, export, interface, method, make_ref(), nil)
        state

      InboundCalls.size(state.inbound_calls) >= state.max_pending_calls ->
        answer_internally(msg, state, fn _msg, state -> {error_reply(:limits_exceeded), state} end)

      true ->
        {entry, calls} =
          InboundCalls.insert(
            state.inbound_calls,
            %{
              ref: make_ref(),
              serial: msg.serial,
              sender: Map.get(msg.header_fields, :sender),
              export: export.ref,
              handler: export.handler,
              unix_fds: []
            },
            export.call_timeout
          )

        send_call(msg, export, interface, method, entry.ref, entry.deadline)
        %{state | inbound_calls: calls}
    end
  end

  # Descriptors are closed before routing in this slice, so neither the call
  # nor the message it carries lists any.
  defp send_call(%Message{} = msg, %Export{} = export, interface, method, ref, deadline) do
    fields = msg.header_fields
    no_reply? = no_reply?(msg)

    call = %Call{
      conn: self(),
      export: export.ref,
      ref: ref,
      serial: msg.serial,
      sender: Map.get(fields, :sender),
      path: fields.path,
      interface: interface,
      member: fields.member,
      signature: Message.signature(msg),
      out_signature: method.out_signature,
      body: msg.body,
      flags: msg.flags,
      no_reply?: no_reply?,
      unix_fds: [],
      deadline: deadline,
      message: %{msg | unix_fds: []}
    }

    send(export.handler, {export.ref, call})
  end

  defp no_reply?(%Message{flags: flags}), do: :no_reply_expected in flags

  defp fail_export(%Connection{} = state, %Export{ref: ref}, error) do
    {entries, calls} = InboundCalls.pop_export(state.inbound_calls, ref)
    Enum.reduce(entries, %{state | inbound_calls: calls}, &fail(&2, &1, error))
  end

  # Only calls that expect a reply are recorded, so every failed call is
  # answered.
  defp fail(%Connection{} = state, %InboundCalls.Entry{} = entry, error) do
    if Writer.replies_saturated?(state.writer) do
      %{state | writer: Writer.refuse_reply(state.writer)}
    else
      queue_reply(error_reply(error), entry.serial, entry.sender, state)
    end
  end

  # A caller that asked for no reply gets none, including an error.
  defp answer_internally(%Message{} = msg, %Connection{} = state, build) do
    cond do
      no_reply?(msg) ->
        state

      Writer.replies_saturated?(state.writer) ->
        %{state | writer: Writer.refuse_reply(state.writer)}

      true ->
        {reply_opts, state} = build.(msg, state)
        queue_reply(reply_opts, msg.serial, reply_destination(msg), state)
    end
  end

  # The table resolved the path when the call was routed, and nothing has
  # changed it since.
  defp introspect(%Message{header_fields: %{path: path}}, %Connection{} = state) do
    {:ok, xml, objects} = Objects.introspect(state.objects, path)
    {[type: :method_return, signature: "s", body: [xml]], %{state | objects: objects}}
  end

  defp error_reply(error) do
    {name, text} = Map.fetch!(@errors, error)
    [type: :error, error_name: name, signature: "s", body: [text]]
  end

  # A bus sets `sender` on every forwarded frame; a peer-to-peer caller has no
  # name, and its reply carries no destination. The name is copied so a small
  # retained header cannot pin the receive buffer it was parsed from.
  defp reply_destination(%Message{header_fields: %{sender: sender}}) when is_binary(sender),
    do: :binary.copy(sender)

  defp reply_destination(%Message{}), do: nil

  # Replies the connection originates take the capped `:reply` path, which
  # has no caller: a peer that floods calls into a stalled socket has its
  # replies dropped rather than growing the write queue without bound.
  defp queue_reply(reply_opts, serial, destination, %Connection{} = state) do
    {type, reply_opts} = Keyword.pop!(reply_opts, :type)
    reply_opts = reply_opts ++ [reply_serial: serial] ++ destination_opts(destination)

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

  defp destination_opts(nil), do: []
  defp destination_opts(destination), do: [destination: destination]

  # The inbound path owns its own continuation (it must keep parsing coalesced
  # frames and re-arm the reader), and the other callers here answer from a
  # callback that has its own return, so neither can take the writer's. This
  # self-message starts the writer instead, after the current callback returns.
  defp kick_writes(%Connection{} = state) do
    send(self(), :advance_writes)
    state
  end
end
