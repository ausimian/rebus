defmodule Rebus.Connection.Dispatch do
  @moduledoc false

  # The inbound half of an established D-Bus connection: reading the socket,
  # feeding `Rebus.Connection.Inbound`, and deciding what each complete frame
  # means. Replies are correlated against the pending table and handed to
  # their caller, signals are matched against this connection's handlers, and
  # method calls are handed to `Rebus.Connection.Service`, which routes them
  # to the handlers of exported objects or answers them itself.
  #
  # Every received descriptor is a `Rebus.UnixFD` handle that this process
  # owns from the moment it is read, so if the connection dies, even by
  # `:kill`, they all close with it. A reply's handles move to its caller as
  # it is answered; everything else is closed here once nothing claims it.
  #
  # It works on the connection struct directly — reply correlation writes the
  # pending table — but it is not a `GenServer`. Every entry point answers with
  # a `t:result/0`, and the connection maps that to a callback return in a
  # single place, so the framing recursion never has to know which callback it
  # was entered from.

  alias Rebus.Connection
  alias Rebus.Connection.HandedOver
  alias Rebus.Connection.Inbound
  alias Rebus.Connection.Pending
  alias Rebus.Connection.Rights
  alias Rebus.Connection.Service
  alias Rebus.Connection.Setup
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.UnixFD

  require Logger

  @max_read_chunk 65_536
  @max_unix_fd_control_size 256

  @typedoc """
  What the connection must do next.

  `:ok` means the callback returns without a continuation, `:continue` that it
  returns the continuation carried here, and the three terminal values that it
  stops: `:shutdown` verbatim, `:protocol_error` and `:transport_error` after
  the connection has sanitised, logged and failed everything outstanding.
  """
  @type result ::
          {:ok, Connection.t()}
          | {:continue, term(), Connection.t()}
          | {:protocol_error, term(), Connection.t()}
          | {:transport_error, term(), Connection.t()}
          | {:shutdown, term(), Connection.t()}

  @doc false
  @spec max_read_chunk() :: pos_integer()
  def max_read_chunk, do: @max_read_chunk

  @doc false
  @spec max_unix_fd_control_size() :: pos_integer()
  def max_unix_fd_control_size, do: @max_unix_fd_control_size

  @doc false
  @spec recv(Connection.t()) :: result()
  def recv(%Connection{} = state) do
    # Every read on a local socket goes through recvmsg, negotiated or not:
    # OTP documents CtrlSz=0 as its default control-buffer size, not as a
    # request to discard ancillary data. Reads coalesce exactly as plain recv
    # does; `Rebus.Connection.Rights` attributes any descriptors to the frames
    # that begin in the bytes they arrived with.
    if state.unix_fd_transport? do
      transport(state).recvmsg(state.sock, 0, @max_unix_fd_control_size, [], :nowait)
      |> receive_result(state)
    else
      receive_result(transport(state).recv(state.sock, 0, [], :nowait), state)
    end
  end

  @doc false
  @spec receive_result(term(), Connection.t()) :: result()
  def receive_result({:ok, data}, %Connection{} = state) when is_binary(data) do
    append_inbound(data, state, :recv)
  end

  def receive_result({:ok, message}, %Connection{} = state) when is_map(message) do
    append_recvmsg(message, state, :recv)
  end

  def receive_result({:select, {:select_info, op, handle}}, %Connection{} = state)
      when op in [:recv, :recvmsg] do
    {:ok, %{state | rref: handle}}
  end

  def receive_result({:select, {{:select_info, :recv, handle}, data}}, %Connection{} = state)
      when is_binary(data) do
    append_inbound(data, %{state | rref: handle}, :recv)
  end

  def receive_result(
        {:select, {{:select_info, :recvmsg, handle}, message}},
        %Connection{} = state
      )
      when is_map(message) do
    append_recvmsg(message, %{state | rref: handle}, :recv)
  end

  def receive_result({:error, reason}, %Connection{} = state) do
    {:transport_error, reason, state}
  end

  def receive_result(_result, %Connection{} = state) do
    {:transport_error, :receive_failed, state}
  end

  # Each zero-length receive returns data already available through the fixed
  # OTP buffer. Fixed-header validation still happens as soon as 16 bytes are
  # retained, without making allocation depend on a peer-declared frame length.
  @doc false
  @spec append_inbound(binary(), Connection.t(), term()) :: result()
  def append_inbound(data, %Connection{} = state, continuation) do
    case Inbound.append(state.inbound, data) do
      {:ok, inbound} ->
        process_inbound(%{state | inbound: inbound}, continuation)

      {:error, reason} ->
        {:protocol_error, reason, state}
    end
  end

  @doc false
  @spec append_recvmsg(map(), Connection.t(), term()) :: result()
  def append_recvmsg(message, %Connection{} = state, continuation) do
    # The single close-or-deliver ownership path for every received
    # descriptor: those held for frames stay with the rights, and anything
    # rejected on receipt is closed here, exactly once.
    case Rights.decode(message, state.inbound_fds, rights_context(state)) do
      {:ok, data, inbound_fds, rejected} ->
        _ = UnixFD.close_owned(rejected)
        append_inbound(data, %{state | inbound_fds: inbound_fds}, continuation)

      {:stop, reason, fds} ->
        _ = UnixFD.close_owned(fds)
        {:protocol_error, reason, state}
    end
  end

  @doc false
  @spec process_inbound(Connection.t(), term()) :: result()
  def process_inbound(%Connection{} = state, continuation) do
    case Inbound.next(state.inbound) do
      {:frame, data, inbound} ->
        parse_complete_message(data, %{state | inbound: inbound}, continuation)

      {:incomplete, inbound} ->
        buffer_incomplete_message(%{state | inbound: inbound}, continuation)

      {:error, reason} ->
        {:protocol_error, reason, state}
    end
  end

  @doc false
  @spec request_timeout(non_neg_integer(), reference(), Connection.t()) :: result()
  def request_timeout(serial, request_ref, %Connection{} = state) do
    case Pending.fetch_by_serial(state.pending, serial) do
      {:ok, %Pending.Entry{request_ref: ^request_ref}} ->
        {entry, pending} = Pending.pop_by_serial(state.pending, serial)
        Pending.fail(entry, {:error, :timeout})
        {:ok, %{state | pending: pending}}

      _stale ->
        {:ok, state}
    end
  end

  @doc false
  @spec discard_inbound_unix_fds(Connection.t()) :: Connection.t()
  def discard_inbound_unix_fds(%Connection{inbound_fds: inbound_fds} = state) do
    _ = UnixFD.close_owned(Rights.fds(inbound_fds))
    %{state | inbound_fds: Rights.new()}
  end

  @doc false
  @spec cancel_partial_frame_timer({reference(), reference()} | nil) :: nil
  def cancel_partial_frame_timer(nil), do: nil

  def cancel_partial_frame_timer({timer_ref, _token}) do
    _ = Process.cancel_timer(timer_ref)
    nil
  end

  # Everything the rights decoder borrows from the connection for one
  # `recvmsg` result.
  defp rights_context(%Connection{} = state) do
    %{
      negotiated?: state.unix_fd_negotiated?,
      inbound: state.inbound,
      max_bytes: @max_read_chunk,
      wrap: &wrap_received/1
    }
  end

  # Each received descriptor becomes a handle this process owns, and the
  # decoder treats the batch of one that cannot be wrapped as malformed. A
  # refused number needs no closing here. `wrap/2` closes it on every
  # refusal but three: after `:ebadf` and `:invalid_descriptor` nothing is
  # open under it, so closing it could only close a descriptor another
  # thread has just been given, and `:unsupported` comes only from a
  # platform that never receives descriptors.
  defp wrap_received(fd) do
    case UnixFD.wrap(fd, self()) do
      {:ok, handle} -> {:ok, handle}
      {:error, _reason} -> :error
    end
  end

  # Descriptors that arrived with bytes in which no further frame can begin,
  # and that no frame claimed, have no owner. They are closed here rather than
  # being carried into a later frame.
  defp expire_inbound_fds(%Connection{} = state) do
    case Rights.expire(state.inbound_fds) do
      {[], inbound_fds} ->
        %{state | inbound_fds: inbound_fds}

      {unclaimed, inbound_fds} ->
        _ = UnixFD.close_owned(unclaimed)
        Logger.warning("D-Bus descriptors dropped: :unclaimed", reason: :unclaimed)
        %{state | inbound_fds: inbound_fds}
    end
  end

  # A timer exists only while a nonempty frame is incomplete. Each retained
  # fragment replaces it, so a peer that is making progress remains connected
  # while a peer that stops or dribbles too slowly cannot pin retained data.
  defp buffer_incomplete_message(%Connection{} = state, continuation) do
    state = expire_inbound_fds(state)

    state =
      if Inbound.pending?(state.inbound) do
        %{state | partial_frame_timer: restart_partial_frame_timer(state)}
      else
        clear_partial_frame(state)
      end

    if is_nil(state.rref) do
      {:continue, continuation, state}
    else
      {:ok, state}
    end
  end

  defp clear_partial_frame(%Connection{} = state) do
    %{
      state
      | inbound: Inbound.clear(state.inbound),
        partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)
    }
  end

  defp finish_frame(%Connection{} = state) do
    %{state | partial_frame_timer: cancel_partial_frame_timer(state.partial_frame_timer)}
  end

  defp restart_partial_frame_timer(%Connection{} = state) do
    cancel_partial_frame_timer(state.partial_frame_timer)
    token = make_ref()
    timer_ref = Process.send_after(self(), {:partial_frame_timeout, token}, state.read_timeout)
    {timer_ref, token}
  end

  # One pass over a receive buffer carries two values that never change: the
  # continuation the entry point must return, and the flat binary every
  # remainder is a sub-binary of.
  defp parse_complete_message(data, %Connection{} = state, continuation) do
    parse_flat_messages(data, state, %{continuation: continuation, source: data})
  end

  # `data` is already flat when a complete frame is available. Parse every
  # coalesced frame directly from its sub-binary remainder, retaining only the
  # final incomplete tail. This avoids re-flattening a receive buffer per frame.
  # A coalesced frame is held to the same size cap as one at the head of the
  # buffer, so an oversized successor stops the connection as soon as its
  # header is parsed rather than being retained as the incomplete tail.
  defp parse_flat_messages(<<>>, %Connection{} = state, cursor) do
    process_inbound(state, cursor.continuation)
  end

  defp parse_flat_messages(data, %Connection{} = state, cursor) do
    case Message.parse_inbound(data, state.inbound.max_message_size) do
      nil ->
        append_inbound(Inbound.retain_remainder(data, cursor.source), state, cursor.continuation)

      # Anything but nil either consumed a frame or ends the connection, so
      # the partial-frame timer is cancelled once here rather than on every
      # way out.
      parsed ->
        parse_complete_frame(parsed, byte_size(data), finish_frame(state), cursor)
    end
  end

  # A consumed frame is everything before the remainder it was parsed with;
  # descriptor attribution follows the stream by that size.
  defp parse_complete_frame({:ok, %Message{} = msg, rest}, available, state, cursor) do
    parse_attached_message(msg, available - byte_size(rest), rest, state, cursor)
  end

  defp parse_complete_frame(
         {:error, :resource_limit, _envelope, _rest},
         _available,
         %Connection{hello_serial: hello_serial} = state,
         _cursor
       )
       when not is_nil(hello_serial) do
    {:protocol_error, {:hello_failed, :resource_limit}, state}
  end

  # `parse_inbound/1` knows the frame boundary, so the remainder is always
  # supplied and this frame can be dropped without rescanning the buffer.
  defp parse_complete_frame(
         {:error, :resource_limit, envelope, rest},
         available,
         %Connection{} = state,
         cursor
       ) do
    Logger.warning("D-Bus frame dropped: :resource_limit", reason: :resource_limit)
    state = skip_inbound_frame(available - byte_size(rest), state)
    {:ok, state} = drop_resource_limited_reply(envelope, state)
    parse_flat_messages(rest, state, cursor)
  end

  defp parse_complete_frame({:error, reason}, _available, %Connection{} = state, _cursor) do
    {:protocol_error, reason, state}
  end

  defp parse_attached_message(%Message{} = msg, size, rest, %Connection{} = state, cursor) do
    case attach_inbound_fds(msg, size, state) do
      {:ok, msg, state} ->
        dispatch_inbound_message(msg, rest, state, cursor)

      {:error, reason, state} ->
        drop_recoverable_fd_frame(reason, rest, state, cursor)
    end
  end

  defp attach_inbound_fds(%Message{} = msg, size, %Connection{} = state) do
    state = expire_inbound_fds(state)

    case Rights.attach(state.inbound_fds, msg, size, state.unix_fd_negotiated?) do
      {:ok, msg, inbound_fds} ->
        {:ok, msg, %{state | inbound_fds: inbound_fds}}

      {:error, reason, fds, inbound_fds} ->
        _ = UnixFD.close_owned(fds)
        {:error, reason, %{state | inbound_fds: inbound_fds}}
    end
  end

  # A frame dropped before its header could be trusted still occupies its
  # place in the stream. Whatever its batch held is closed with it.
  defp skip_inbound_frame(size, %Connection{} = state) do
    state = expire_inbound_fds(state)
    {fds, inbound_fds} = Rights.skip(state.inbound_fds, size)
    _ = UnixFD.close_owned(fds)
    %{state | inbound_fds: inbound_fds}
  end

  # Count/index/negotiation checks run after a complete D-Bus frame and its
  # ancillary data have been collected. The stream boundary is therefore known:
  # close the descriptors, drop only this frame, and continue with a coalesced
  # successor rather than letting a peer kill unrelated calls or handlers.
  defp drop_recoverable_fd_frame(reason, rest, state, cursor) do
    reason = Rights.drop_reason(reason)
    Logger.warning("D-Bus FD frame dropped: #{inspect(reason)}", reason: reason)
    parse_flat_messages(rest, state, cursor)
  end

  defp dispatch_inbound_message(
         %Message{} = msg,
         rest,
         %Connection{hello_serial: hello_serial} = state,
         cursor
       )
       when not is_nil(hello_serial) do
    # dbus-daemon's bus/dispatch.c replies to Hello before emitting the
    # directed NameAcquired signal. Until that reply supplies our unique name,
    # any other frame is a protocol error rather than application traffic.
    if msg.unix_fds != [] do
      close_message_fds(msg)
      {:protocol_error, :invalid_unix_fds, state}
    else
      dispatch_hello_reply(Setup.hello_reply_result(msg, hello_serial), rest, state, cursor)
    end
  end

  defp dispatch_inbound_message(%Message{type: type} = msg, rest, %Connection{} = state, cursor)
       when type in [:method_return, :error] do
    case reply(msg, state) do
      {:ok, state} -> parse_flat_messages(rest, state, cursor)
      {:error, reason} -> {:protocol_error, reason, state}
    end
  end

  defp dispatch_inbound_message(
         %Message{type: :signal} = msg,
         rest,
         %Connection{} = state,
         cursor
       ) do
    # Signals may have multiple subscribers, and a handle has one owner, so
    # FD-bearing signals are rejected and their handles closed.
    if msg.unix_fds == [] do
      parse_flat_messages(rest, notify(msg, apply_name_owner_change(msg, state)), cursor)
    else
      close_message_fds(msg)
      Logger.warning("D-Bus FD frame dropped: :signal_ownership", reason: :signal_ownership)
      parse_flat_messages(rest, state, cursor)
    end
  end

  defp dispatch_inbound_message(
         %Message{type: :method_call} = msg,
         rest,
         %Connection{} = state,
         cursor
       ) do
    # The call's descriptors go with it: a handler borrows them until the
    # call completes, and every call that reaches no handler closes them.
    parse_flat_messages(rest, Service.answer(msg, state), cursor)
  end

  defp dispatch_inbound_message(%Message{} = msg, rest, state, cursor) do
    close_message_fds(msg)
    parse_flat_messages(rest, state, cursor)
  end

  # The reader takes over from the setup path here, so this is the one point
  # in a pass where the continuation changes.
  defp dispatch_hello_reply({:ok, name}, rest, %Connection{} = state, cursor) do
    case Setup.establish_connection(%{
           state
           | name: name,
             hello_serial: nil,
             established?: true
         }) do
      {:ok, established} ->
        parse_flat_messages(rest, established, %{cursor | continuation: :recv})

      {:error, reason} ->
        {:shutdown, reason, state}
    end
  end

  defp dispatch_hello_reply({:error, reason}, _rest, %Connection{} = state, _cursor) do
    {:protocol_error, reason, state}
  end

  defp close_message_fds(%Message{unix_fds: fds}), do: UnixFD.close_owned(fds)

  # The owner table follows the bus driver's own NameOwnerChanged signals, and
  # only for the names a subscription asked to track. On a message bus the
  # sender header is set by the bus, so a peer cannot forge
  # `org.freedesktop.DBus`: that exact match is the trust boundary, and it is
  # deliberately not relaxed to the interface and member alone, which any peer
  # can send. The signal is still delivered to handlers afterwards, since a
  # subscription may be watching NameOwnerChanged itself.
  defp apply_name_owner_change(
         %Message{
           header_fields: %{
             sender: "org.freedesktop.DBus",
             interface: "org.freedesktop.DBus",
             member: "NameOwnerChanged"
           },
           body: [name, old_owner, new_owner]
         },
         %Connection{name_owners: name_owners} = state
       )
       when is_binary(name) and is_binary(old_owner) and is_binary(new_owner) do
    if Map.has_key?(name_owners, name) do
      %{state | name_owners: Map.put(name_owners, :binary.copy(name), new_owner(new_owner))}
    else
      state
    end
  end

  defp apply_name_owner_change(%Message{}, %Connection{} = state), do: state

  # An empty new owner means the name was released. Both the key and the owner
  # retained above are copied out of the received frame: a map insert for an
  # existing key retains the new key term, so an uncopied sub-binary would hold
  # a whole frame alive until the next owner change.
  defp new_owner(""), do: nil
  defp new_owner(owner), do: :binary.copy(owner)

  # A non-bus connection has no unique name, so nothing can be its own
  # NameAcquired signal. Match the absent name explicitly rather than letting
  # `nil` participate in the header comparison below.
  defp notify(%Message{} = msg, %Connection{name: nil} = state) do
    dispatch_signal(msg, state)
    state
  end

  # Ignore our own NameAcquired signals.
  defp notify(
         %Message{header_fields: %{member: "NameAcquired", destination: name}, body: [name]},
         %Connection{name: name} = state
       ),
       do: state

  defp notify(%Message{} = msg, %Connection{} = state) do
    dispatch_signal(msg, state)
    state
  end

  # Handlers live in this connection's state, so the match rule of a
  # subscription is evaluated here, in the connection process, and only for
  # the signals this connection received. The parsed body signature is shared
  # across the rules for this one signal, so it is parsed at most once.
  defp dispatch_signal(%Message{} = msg, %Connection{handlers: handlers} = state) do
    name_owners = state.name_owners

    Enum.reduce(handlers, :unparsed, fn
      {_handler_ref, %{name: _name} = watch}, body_types ->
        notify_name_watch(watch, msg, name_owners, body_types)

      {handler_ref, %{pid: pid, rule: nil}}, body_types ->
        send(pid, {handler_ref, msg})
        body_types

      {handler_ref, %{pid: pid, rule: rule}}, body_types ->
        {matched?, body_types} = MatchRule.match(rule, msg, name_owners, body_types)
        if matched?, do: send(pid, {handler_ref, msg})
        body_types
    end)
  end

  # A name watch turns the driver's NameAcquired and NameLost for its name
  # into the notice `Rebus.Bus` documents, rather than passing the signal on.
  defp notify_name_watch(%{pid: pid, rule: rule, name: name}, msg, name_owners, body_types) do
    case name_event(msg) do
      nil ->
        body_types

      event ->
        {matched?, body_types} = MatchRule.match(rule, msg, name_owners, body_types)
        if matched?, do: send(pid, {:rebus_name, self(), event, name})
        body_types
    end
  end

  defp name_event(%Message{header_fields: %{member: "NameAcquired"}}), do: :acquired
  defp name_event(%Message{header_fields: %{member: "NameLost"}}), do: :lost
  defp name_event(%Message{}), do: nil

  defp reply(%Message{} = msg, %Connection{} = state) do
    case Map.fetch(msg.header_fields, :reply_serial) do
      {:ok, reply_serial} ->
        {:ok, correlate_reply(msg, reply_serial, state)}

      :error ->
        {:error, {:malformed_reply, :missing_reply_serial}}
    end
  end

  defp correlate_reply(%Message{} = msg, reply_serial, %Connection{} = state) do
    case Pending.pop_by_serial(state.pending, reply_serial) do
      {nil, _pending} ->
        close_message_fds(msg)
        log_orphaned_reply(reply_serial)
        state

      {entry, pending} ->
        deliver_reply(msg, entry, %{state | pending: pending})
    end
  end

  # The reply's handles move to the caller before it is answered, so they
  # are the caller's the moment it can see them. `from` is the process that
  # called `Rebus.call/3` itself: the call reaches this connection straight
  # from it, with no process in between. A caller that has died meanwhile
  # gets no reply, and its handles closed as the transfer finds it gone. One
  # that is alive but stopped waiting never receives the reply, and its
  # timeout cancel closes the handles; see `Rebus.Connection.HandedOver`.
  defp deliver_reply(%Message{} = msg, %Pending.Entry{from: from} = entry, state) do
    Pending.release_monitor(entry)

    if hand_over(msg.unix_fds, from) do
      GenServer.reply(from, reply_result(msg))
      remember_handed_over(state, entry, msg.unix_fds)
    else
      state
    end
  end

  defp remember_handed_over(%Connection{} = state, _entry, []), do: state

  defp remember_handed_over(%Connection{} = state, %Pending.Entry{} = entry, handles) do
    handed_over = HandedOver.put(state.handed_over, entry.request_ref, handles, entry.deadline)
    %{state | handed_over: handed_over}
  end

  # A pending entry is only ever registered for a call that arrived through
  # `handle_call/3`, so its `from` is always a `t:GenServer.from/0`. Every
  # handle is offered to the caller even after one finds it dead, since that
  # transfer is what closes each of them.
  defp hand_over([], {pid, _tag}) when is_pid(pid), do: Process.alive?(pid)

  defp hand_over(handles, {pid, _tag}) when is_pid(pid) do
    handed? =
      handles
      |> Enum.map(&UnixFD.controlling_process(&1, pid))
      |> Enum.all?(&(&1 == :ok))

    # Whatever did not move is still this process's, and nobody will see it.
    unless handed?, do: UnixFD.close_owned(handles)
    handed?
  end

  # A D-Bus error reply is a definitive peer answer, not a transport failure,
  # but callers should not have to test the type to branch on it. The
  # `:dbus_error` tag keeps it apart from local failure reasons under the same
  # `:error`. The complete message is retained in either shape so its error
  # name, body and any handles it carries stay available to the caller.
  defp reply_result(%Message{type: :error} = msg), do: {:error, {:dbus_error, msg}}
  defp reply_result(%Message{} = msg), do: {:ok, msg}

  defp drop_resource_limited_reply(
         %{type: :method_return, reply_serial: reply_serial},
         %Connection{} = state
       )
       when is_integer(reply_serial) and reply_serial > 0 do
    drop_resource_limited_pending(reply_serial, :method_return, state)
  end

  defp drop_resource_limited_reply(
         %{type: :error, reply_serial: reply_serial, error_name: error_name},
         %Connection{} = state
       )
       when is_integer(reply_serial) and reply_serial > 0 and is_binary(error_name) do
    drop_resource_limited_pending(reply_serial, {:error, error_name}, state)
  end

  defp drop_resource_limited_reply(_envelope, %Connection{} = state), do: {:ok, state}

  defp drop_resource_limited_pending(reply_serial, reply_kind, %Connection{} = state) do
    case Pending.pop_by_serial(state.pending, reply_serial) do
      {nil, _pending} ->
        log_orphaned_reply(reply_serial)
        {:ok, state}

      {entry, pending} ->
        Pending.fail(entry, {:error, {:reply_dropped, reply_kind}})
        {:ok, %{state | pending: pending}}
    end
  end

  # The call timed out, its caller went down, or the peer answered twice.
  defp log_orphaned_reply(reply_serial) do
    Logger.info("Ignoring late or orphaned D-Bus reply for serial #{reply_serial}")
  end

  defp transport(%Connection{impl: %{transport: transport}}), do: transport
end
