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
  # `org.freedesktop.DBus.Properties` is checked against the declaration of
  # the interface it names, and goes to the handler that exported that
  # interface. Only reading the values is left to the handler: the target
  # interface, the property, its access and, for `Set`, the type of the new
  # value have all been checked by then.
  #
  # Calls that expect a reply are recorded in `Rebus.Connection.InboundCalls`
  # until their handler answers, they expire, or their export goes away. So
  # is a call sent with `NO_REPLY_EXPECTED` that carries descriptors, so that
  # something releases them, but nothing is ever sent for it. One that
  # carries none is not recorded, and `Rebus.Service` answers it as a no-op
  # in the handler process.
  #
  # The descriptors that arrive with a call are the connection's until the
  # call completes: a handler borrows them, and the table closes them when
  # the call leaves it. A call that reaches no handler has them closed as it
  # is answered. `take_fds/5` hands them over through the FD-claim protocol
  # of `Rebus.Connection.FDClaims`, which then owns them until the taker
  # acknowledges.
  #
  # A local call made with `Rebus.Service.invoke/7` takes the same route and
  # is recorded the same way, but every answer to it, the handler's or the
  # connection's own, goes to the process waiting for it rather than to the
  # socket.
  #
  # Like `Rebus.Connection.Dispatch` this works on the connection struct
  # directly, but it decides nothing about framing, so it answers with a
  # connection.

  alias Rebus.Connection
  alias Rebus.Connection.FDClaims
  alias Rebus.Connection.InboundCalls
  alias Rebus.Connection.InboundCalls.Entry
  alias Rebus.Connection.Objects
  alias Rebus.Connection.Objects.Export
  alias Rebus.Connection.Peer
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Writer
  alias Rebus.Interface
  alias Rebus.Interface.Method
  alias Rebus.Interface.Property
  alias Rebus.Message
  alias Rebus.Service.Call
  alias Rebus.UnixFD

  require Logger

  @peer_interface "org.freedesktop.DBus.Peer"
  @introspectable_interface "org.freedesktop.DBus.Introspectable"
  @properties Interface.properties()
  @properties_interface @properties.name

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
    invalid_value:
      {"org.freedesktop.DBus.Error.InvalidArgs", "Value does not match the property signature"},
    unknown_property:
      {"org.freedesktop.DBus.Error.UnknownProperty", "Property not declared by this interface"},
    property_read_only: {"org.freedesktop.DBus.Error.PropertyReadOnly", "Property is read-only"},
    # The specification names no error for reading a write-only property;
    # this is GDBus's answer.
    property_not_readable: {"org.freedesktop.DBus.Error.InvalidArgs", "Property is not readable"},
    limits_exceeded:
      {"org.freedesktop.DBus.Error.LimitsExceeded", "Too many method calls are pending"},
    handler_exited: {"org.freedesktop.DBus.Error.Failed", "Handler exited"},
    unexported: {"org.freedesktop.DBus.Error.Failed", "Object unexported"},
    expired: {"org.freedesktop.DBus.Error.NoReply", "Handler did not reply in time"}
  }

  @type error :: unquote(Enum.reduce(Map.keys(@errors), &{:|, [], [&1, &2]}))

  # Who is waiting for the answer to a call: a peer, answered on the socket,
  # or a local process, answered directly.
  @typep caller :: :remote | {:local, GenServer.from(), reference()}

  # Routes one inbound method call, which brings the descriptors that
  # arrived with it: a handler borrows them, and anything else closes them.
  @doc false
  @spec answer(Message.t(), Connection.t()) :: Connection.t()
  def answer(%Message{} = msg, %Connection{} = state), do: route_call(msg, :remote, state)

  # Routes a local call, which `from` waits for. The call is recorded under
  # the caller's request reference, so a caller that stops waiting can
  # withdraw it with `cancel/2`.
  @doc false
  @spec invoke(Message.t(), GenServer.from(), reference(), Connection.t()) :: Connection.t()
  def invoke(%Message{} = msg, from, request_ref, %Connection{} = state)
      when is_reference(request_ref),
      do: route_call(msg, {:local, from, request_ref}, state)

  # A local caller stopped waiting. Its call leaves the table unanswered, and
  # an answer from the handler afterwards finds nothing, as after expiry.
  @doc false
  @spec cancel(Connection.t(), reference()) :: {:ok, Connection.t()} | :error
  def cancel(%Connection{} = state, request_ref) do
    case InboundCalls.fetch(state.inbound_calls, request_ref) do
      {:ok, %Entry{from: from}} when from != nil ->
        {_entry, calls} = InboundCalls.complete(state.inbound_calls, request_ref)
        {:ok, %{state | inbound_calls: calls}}

      _remote_or_missing ->
        :error
    end
  end

  @spec route_call(Message.t(), caller(), Connection.t()) :: Connection.t()
  defp route_call(%Message{} = msg, caller, %Connection{} = state) do
    case route(msg, state.objects) do
      {:ok, export, interface, method} ->
        deliver(msg, caller, export, interface, method, state)

      :peer ->
        answer_internally(msg, caller, state, &Peer.reply/2)

      :introspect ->
        answer_internally(msg, caller, state, &introspect/2)

      :no_properties ->
        answer_internally(msg, caller, state, fn _msg, state ->
          {[type: :method_return, signature: "a{sv}", body: [[]]], state}
        end)

      {:error, error} ->
        answer_internally(msg, caller, state, fn _msg, state -> {error_reply(error), state} end)
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
  # handed and only to the peer that sent it. A reply to a peer is returned
  # for the caller to write; one to a local call is delivered here, and
  # `:answered` says so. Descriptors cannot be handed to a local caller, so a
  # reply that carries any is refused and the call stays open.
  #
  # A reply is written after the call completes, so it cannot carry a
  # descriptor the connection still owns: one lent to a pending call, this
  # one or another, held by an FD claim that has not settled, or received
  # for a frame not yet complete. Completing
  # a call, or a claim failing, closes it before the frame is written, and
  # the reply would carry a closed descriptor or whatever reused its number.
  # Such a reply is refused, and the call stays open for the handler to take
  # them first.
  @doc false
  @spec complete(Connection.t(), reference(), Message.t()) ::
          {:ok, Message.t(), Connection.t()}
          | {:answered, Connection.t()}
          | {:error, :unknown_call | :unix_fd_unsupported | :unix_fd_borrowed}
  def complete(%Connection{} = state, ref, %Message{} = msg) do
    case InboundCalls.fetch(state.inbound_calls, ref) do
      :error ->
        {:error, :unknown_call}

      {:ok, %Entry{no_reply?: true}} ->
        {:error, :unknown_call}

      {:ok, %Entry{from: from}} when from != nil and msg.unix_fds != [] ->
        {:error, :unix_fd_unsupported}

      {:ok, %Entry{}} ->
        if lent?(state, msg.unix_fds),
          do: {:error, :unix_fd_borrowed},
          else: complete_entry(state, ref, msg)
    end
  end

  # The scan is bounded by `max_pending_calls`, each call's descriptor limit,
  # the open claims and the receive buffer's own bound, and only a reply that
  # carries descriptors pays it.
  defp lent?(%Connection{}, []), do: false

  defp lent?(%Connection{} = state, fds) do
    owned =
      MapSet.new(
        InboundCalls.fds(state.inbound_calls) ++
          FDClaims.fds(state.fd_claims) ++ Rights.fds(state.inbound_fds)
      )

    Enum.any?(fds, &MapSet.member?(owned, &1))
  end

  defp complete_entry(%Connection{} = state, ref, %Message{} = msg) do
    {%Entry{} = entry, calls} = InboundCalls.complete(state.inbound_calls, ref)
    reply = address(msg, entry.serial, entry.sender)
    state = %{state | inbound_calls: calls}

    case entry.from do
      nil ->
        {:ok, reply, state}

      from ->
        GenServer.reply(from, local_result(reply))
        {:answered, state}
    end
  end

  # The handler answered a call that expects no reply. Nothing is sent, but
  # the call leaves the table, which releases its descriptors.
  @doc false
  @spec release(Connection.t(), reference()) :: {:ok, Connection.t()} | {:error, :unknown_call}
  def release(%Connection{} = state, ref) do
    case InboundCalls.fetch(state.inbound_calls, ref) do
      {:ok, %Entry{no_reply?: true}} ->
        {_entry, calls} = InboundCalls.complete(state.inbound_calls, ref)
        {:ok, %{state | inbound_calls: calls}}

      _expects_reply_or_missing ->
        {:error, :unknown_call}
    end
  end

  # Hands a pending call's descriptors to `from` through an FD claim, which
  # owns them from here on: it closes them on every way out but the taker's
  # acknowledgement. The call stays pending, with nothing left for its
  # completion to close, and the claim monitors the taker rather than the
  # handler, since any process holding the call may take them.
  @doc false
  @spec take_fds(Connection.t(), reference(), GenServer.from(), integer(), reference()) ::
          {:ok, Connection.t()} | {:error, :unknown_call}
  def take_fds(%Connection{} = state, ref, {pid, _tag} = from, deadline, request_ref)
      when is_reference(request_ref) do
    case InboundCalls.take_fds(state.inbound_calls, ref) do
      {:ok, %Entry{serial: serial, unix_fds: fds}, calls} ->
        # The claim delivers a message, so the descriptors travel in one
        # that is never sent anywhere.
        carrier = %{Message.new!(:method_return, reply_serial: serial) | unix_fds: fds}

        claims =
          FDClaims.open(
            state.fd_claims,
            %{
              msg: carrier,
              from: from,
              request_ref: request_ref,
              monitor_ref: Process.monitor(pid),
              deadline: deadline
            },
            Connection.fd_claims_context(state)
          )

        {:ok, %{state | inbound_calls: calls, fd_claims: claims}}

      :error ->
        {:error, :unknown_call}
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

      @properties_interface ->
        objects
        |> Objects.resolve(fields.path)
        |> resolve_property(fields.member, msg)

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

  # A Properties call names its target interface, and for `Get` and `Set` the
  # property, in its body, which can only be read once the signature is known
  # to be the declared one. It is routed to the export serving the target
  # interface, which an object that declares no readable property there is
  # spared: `GetAll` then answers an empty dictionary without it.
  defp resolve_property({:object, interfaces}, member, %Message{body: body} = msg) do
    with {:ok, %Method{} = method} <- fetch(@properties.methods, member, :unknown_method),
         :ok <- check(Message.signature(msg) == method.in_signature, :invalid_args),
         [target | args] = body,
         {:ok, %Export{} = export} <- fetch(interfaces, target, :unknown_interface),
         :ok <- check_property(member, export.interfaces[target], args) do
      {:ok, export, @properties_interface, method}
    end
  end

  defp resolve_property(resolution, member, _msg),
    do: resolve_member(resolution, @properties_interface, member)

  defp check_property("GetAll", %Interface{properties: properties}, []) do
    if Enum.any?(properties, fn {_name, property} -> property.access != :write end),
      do: :ok,
      else: :no_properties
  end

  defp check_property("Get", %Interface{} = interface, [name]) do
    with {:ok, %Property{} = property} <- fetch_property(interface, name) do
      check(property.access != :write, :property_not_readable)
    end
  end

  # Variants decode as `{signature, value}`.
  defp check_property("Set", %Interface{} = interface, [name, {signature, _value}]) do
    with {:ok, %Property{} = property} <- fetch_property(interface, name),
         :ok <- check(property.access != :read, :property_read_only) do
      check(signature == property.signature, :invalid_value)
    end
  end

  defp fetch_property(%Interface{properties: properties}, name),
    do: fetch(properties, name, :unknown_property)

  defp check(true, _error), do: :ok
  defp check(false, error), do: {:error, error}

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

  # A call that expects no reply and carries no descriptors leaves nothing
  # to answer or release, so it is sent without being recorded or bounded. A
  # recorded one beyond the bound is refused, which for a call that expects
  # no reply means it is dropped, its descriptors closed.
  defp deliver(%Message{} = msg, caller, %Export{} = export, interface, %Method{} = method, state) do
    cond do
      no_reply?(msg) and msg.unix_fds == [] ->
        send_call(msg, export, interface, method, make_ref(), nil)
        state

      InboundCalls.size(state.inbound_calls) >= state.max_pending_calls ->
        answer_internally(msg, caller, state, fn _msg, state ->
          {error_reply(:limits_exceeded), state}
        end)

      true ->
        {entry, calls} =
          InboundCalls.insert(
            state.inbound_calls,
            Map.merge(local_entry(caller), %{
              serial: msg.serial,
              sender: Map.get(msg.header_fields, :sender),
              export: export.ref,
              handler: export.handler,
              unix_fds: msg.unix_fds,
              no_reply?: no_reply?(msg)
            }),
            export.call_timeout
          )

        send_call(msg, export, interface, method, entry.ref, entry.deadline)
        %{state | inbound_calls: calls}
    end
  end

  # The call and the message it carries both list the borrowed descriptors.
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
      unix_fds: msg.unix_fds,
      deadline: deadline,
      message: msg
    }

    send(export.handler, {export.ref, call})
  end

  defp local_entry(:remote), do: %{ref: make_ref()}
  defp local_entry({:local, from, request_ref}), do: %{ref: request_ref, from: from}

  defp no_reply?(%Message{flags: flags}), do: :no_reply_expected in flags

  defp fail_export(%Connection{} = state, %Export{ref: ref}, error) do
    {entries, calls} = InboundCalls.pop_export(state.inbound_calls, ref)
    Enum.reduce(entries, %{state | inbound_calls: calls}, &fail(&2, &1, error))
  end

  # A failed call that expects no reply is recorded only for its
  # descriptors, which leaving the table has released, so nothing is sent.
  defp fail(%Connection{} = state, %Entry{no_reply?: true}, _error), do: state

  defp fail(%Connection{} = state, %Entry{from: from} = entry, error) when from != nil do
    answer_local(from, error_reply(error), entry.serial, entry.sender)
    state
  end

  defp fail(%Connection{} = state, %Entry{} = entry, error) do
    if Writer.replies_saturated?(state.writer) do
      %{state | writer: Writer.refuse_reply(state.writer)}
    else
      queue_reply(error_reply(error), entry.serial, entry.sender, state)
    end
  end

  # A call answered here reaches no handler, so nothing borrows its
  # descriptors and they are closed first, whether or not an answer is sent.
  defp answer_internally(%Message{unix_fds: fds} = msg, caller, %Connection{} = state, build) do
    _ = UnixFD.close_all(fds)
    respond(msg, caller, state, build)
  end

  # A local caller is answered directly; nothing is written, so the cap on
  # replies the connection writes does not apply.
  defp respond(%Message{} = msg, {:local, from, _request_ref}, state, build) do
    {reply_opts, state} = build.(msg, state)
    answer_local(from, reply_opts, msg.serial, reply_destination(msg))
    state
  end

  # A caller that asked for no reply gets none, including an error.
  defp respond(%Message{} = msg, :remote, %Connection{} = state, build) do
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
    case build_reply(reply_opts, serial, destination) do
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

  # The answer a local caller gets is what `Rebus.call/3` would return for
  # the same reply from a peer.
  defp answer_local(from, reply_opts, serial, destination) do
    case build_reply(reply_opts, serial, destination) do
      {:ok, reply} ->
        GenServer.reply(from, local_result(reply))

      {:error, reason} ->
        Logger.warning("D-Bus internal reply dropped: #{inspect(reason)}", reason: reason)
        GenServer.reply(from, {:error, reason})
    end
  end

  defp local_result(%Message{type: :method_return} = reply), do: {:ok, reply}
  defp local_result(%Message{type: :error} = reply), do: {:error, {:dbus_error, reply}}

  defp build_reply(reply_opts, serial, destination) do
    {type, reply_opts} = Keyword.pop!(reply_opts, :type)
    Message.new(type, reply_opts ++ [reply_serial: serial] ++ destination_opts(destination))
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
