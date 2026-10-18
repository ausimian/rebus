defmodule Rebus.Service do
  @moduledoc """
  Exports objects on a connection and answers the method calls made on them.

  An object is a set of `Rebus.Interface` declarations exported at an object
  path. Each export names a handler process, which receives every call made
  on the export's interfaces as a message:

      {export_ref, %Rebus.Service.Call{}}

  The connection process never runs application code. It routes each
  inbound call, checks it against the declarations, and refuses anything that
  does not match with a standard D-Bus error, so a handler only ever sees
  calls to declared methods with arguments of the declared types. Calls to
  one handler arrive in the order they were received.

  ## Example

      calculator =
        Rebus.Interface.new!("org.example.Calculator",
          methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}]
        )

      {:ok, conn} = Rebus.connect(:session)
      {:ok, ref} = Rebus.Service.export(conn, "/org/example/Calculator", [calculator])

      receive do
        {^ref, %Rebus.Service.Call{member: "Add", body: [a, b]} = call} ->
          :ok = Rebus.Service.reply(call, [a + b])
      end

  Clients usually find a service by a well-known name. Export its objects
  first, then request the name with `Rebus.Bus.request_name/4`, so that no
  client reaches the name before the objects are there to answer.

  ## Answering calls

  Answer every call exactly once, with `reply/3`, `reply_error/4` or
  `reply_message/3`. Any process holding the `Rebus.Service.Call` can answer
  it, so a handler can hand a slow call to another process, but the call
  stays bound to its export: when the export goes away, the call is failed.

  A call whose caller asked for no reply has `no_reply?: true`. Answering it
  sends nothing and returns `:ok`, so a handler can treat every call the same
  way.

  A second answer to the same call returns `{:error, :unknown_call}`, as does
  an answer to a call that has expired or whose export is gone. The caller
  never receives two replies.

  ## What callers see

  The connection answers these itself, each with a fixed message text that
  never echoes the call:

  | Condition | Error name |
  | --- | --- |
  | Nothing exported at the path or below it | `org.freedesktop.DBus.Error.UnknownObject` |
  | A path with exports only below it, for anything but `Introspect` | `org.freedesktop.DBus.Error.UnknownInterface` |
  | Interface not exported on the object | `org.freedesktop.DBus.Error.UnknownInterface` |
  | Method not declared, or, without an interface, declared by no exported interface or by more than one | `org.freedesktop.DBus.Error.UnknownMethod` |
  | Argument signature differs from the declaration | `org.freedesktop.DBus.Error.InvalidArgs` |
  | Property not declared by the interface a `Properties` call names | `org.freedesktop.DBus.Error.UnknownProperty` |
  | `Set` of a read-only property | `org.freedesktop.DBus.Error.PropertyReadOnly` |
  | `Get` of a write-only property | `org.freedesktop.DBus.Error.InvalidArgs` |
  | `Set` value of a type other than the declared one | `org.freedesktop.DBus.Error.InvalidArgs` |
  | `:max_pending_calls` calls already waiting | `org.freedesktop.DBus.Error.LimitsExceeded` |
  | Handler exited with the call pending | `org.freedesktop.DBus.Error.Failed` |
  | Object unexported with the call pending | `org.freedesktop.DBus.Error.Failed` |
  | No answer within the export's `:call_timeout` | `org.freedesktop.DBus.Error.NoReply` |

  A caller that asked for no reply gets none of these.

  `org.freedesktop.DBus.Peer` is answered by the connection on every path, so
  it cannot be exported, and neither can
  `org.freedesktop.DBus.Introspectable` or `org.freedesktop.DBus.Properties`,
  which the connection reserves for itself.

  ## Properties

  The connection checks every `org.freedesktop.DBus.Properties` call against
  the declaration of the interface it names, and sends it to the handler that
  exported that interface. The call arrives with
  `interface: "org.freedesktop.DBus.Properties"`, `member` `"Get"`, `"Set"`
  or `"GetAll"`, and the body as sent, whose first element names the
  interface. By then the interface is exported on the object, a `Get` or
  `Set` names a declared property that can be read or written, and a `Set`
  value has the declared type, so the handler only reads or writes values:

      alias Rebus.Service.Call

      receive do
        {^ref, %Call{member: "Get", body: [_iface, "Precision"]} = call} ->
          Rebus.Service.reply(call, [{"u", precision}])

        {^ref, %Call{member: "Set", body: [_iface, "Precision", {"u", value}]} = call} ->
          Rebus.Service.reply(call, [])

        {^ref, %Call{member: "GetAll"} = call} ->
          values = Rebus.Interface.wrap_properties!(settings, %{"Precision" => precision})
          Rebus.Service.reply(call, [values])
      end

  `call.out_signature` is `"v"` for `Get`, so the value is a
  `{signature, value}` variant; `""` for `Set`; and `"a{sv}"` for `GetAll`,
  which `Rebus.Interface.wrap_properties/2` builds from the declaration. A
  `GetAll` for an interface that declares no readable property is answered
  with an empty dictionary without reaching the handler.

  ## Signals

  `emit/6` sends a signal from an exported object, typed by the interface
  declaration, and `properties_changed/6` sends
  `org.freedesktop.DBus.Properties.PropertiesChanged` for properties that
  changed, as each property's `emits_changed` allows:

      :ok = Rebus.Service.emit(conn, "/org/example/Calculator", calculator, "Computed", [5])

      :ok =
        Rebus.Service.properties_changed(conn, "/org/example/Calculator", settings,
          %{"Precision" => 3}, [])

  ## Introspection

  The connection answers `org.freedesktop.DBus.Introspectable.Introspect`
  itself, with XML generated from the exported declarations, so tools such as
  `busctl` and `d-feet` can find and describe the objects. An exported object
  lists its own interfaces, then `Peer`, `Introspectable` and `Properties`,
  then the next path element of every export below it. A path with exports
  only below it, such as `/` or `/org` for `/org/example/Calculator`, lists
  `Introspectable` and those child elements, so a tool can walk the tree from
  `/`. A path served by a subtree export lists the subtree's interfaces and
  no children, since nothing tells the connection which paths the handler
  serves.

  Descriptors sent with an inbound call are closed before the call is
  routed, and `call.unix_fds` is always `[]`.
  """

  alias Rebus.Connection
  alias Rebus.Interface
  alias Rebus.Message
  alias Rebus.Service.Call
  alias Rebus.WireValue

  @default_timeout 5_000
  @default_call_timeout 60_000
  @signal_options [:signature, :destination, :flags, :fds]

  @reserved_interfaces [
    "org.freedesktop.DBus.Peer",
    "org.freedesktop.DBus.Introspectable",
    "org.freedesktop.DBus.Properties"
  ]

  @typedoc """
  A reason why `export/4` refused an export.

  `{:invalid_interface, reason}` carries the offending value, `{:duplicate,
  name}` for an interface listed twice, or `{:reserved, name}` for an
  interface the connection implements itself.
  """
  @type export_error ::
          :not_connected
          | :disconnected
          | :timeout
          | :remote_connection_unsupported
          | :invalid_path
          | :invalid_handler
          | :invalid_subtree
          | :invalid_call_timeout
          | {:invalid_interface, term()}
          | {:already_exported, path :: binary(), interface :: binary()}

  @typedoc "A reason why a reply was not sent."
  @type reply_error ::
          :unknown_call
          | :signature_mismatch
          | Message.construction_error()
          | Rebus.error_reason()

  @doc """
  Exports `interfaces` at `path` on `conn`.

  Calls on those interfaces at `path` are sent to the handler as
  `{ref, %Rebus.Service.Call{}}`, where `ref` is the reference returned
  here. Several exports can share a path, each with its own interfaces and
  handler, but an interface can be exported only once at a path.

  The handler is monitored. When it exits, everything it exported is
  unexported, and each call still waiting for its answer is failed with
  `org.freedesktop.DBus.Error.Failed`. Exports also end with the connection.

  ## Options

  - `:handler` - the local process that receives the calls. Defaults to the
    caller.
  - `:subtree` - when `true`, the export also serves every path below `path`,
    and `call.path` is the path actually called. An exact export of a path
    takes precedence over a subtree that covers it, and the longest
    covering subtree takes precedence over shorter ones. Defaults to `false`.
  - `:call_timeout` - positive milliseconds a call may wait for its answer
    before the connection answers the caller with
    `org.freedesktop.DBus.Error.NoReply`, or `:infinity`. Defaults to `60000`.
  - `:timeout` - milliseconds to wait for the connection to accept the
    export. Defaults to `5000`.

  ## Return values

  - `{:ok, ref}` - the export's reference, which tags its calls and is what
    `unexport/3` takes.
  - `{:error, :invalid_path}` - `path` is not a valid object path.
  - `{:error, {:invalid_interface, reason}}` - `interfaces` is not a
    non-empty list of distinct `Rebus.Interface` structs, or names an
    interface the connection reserves; see `t:export_error/0`.
  - `{:error, :invalid_handler}`, `{:error, :invalid_subtree}` and
    `{:error, :invalid_call_timeout}` - an invalid option. The handler must
    be a local PID.
  - `{:error, {:already_exported, path, interface}}` - `interface` is
    already exported at `path`.
  - `{:error, :not_connected}`, `{:error, :timeout}`,
    `{:error, :disconnected}` and `{:error, :remote_connection_unsupported}`
    - as for `Rebus.send/3`. An export that times out is withdrawn.
  """
  @spec export(pid(), binary(), [Interface.t()], keyword()) ::
          {:ok, reference()} | {:error, export_error()}
  def export(conn, path, interfaces, opts \\ []) when is_pid(conn) and is_list(opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)

    with :ok <- validate_path(path),
         :ok <- validate_interfaces(interfaces),
         {:ok, handler} <- validate_handler(Keyword.get(opts, :handler, self())),
         {:ok, subtree?} <- validate_subtree(Keyword.get(opts, :subtree, false)),
         {:ok, call_timeout} <-
           validate_call_timeout(Keyword.get(opts, :call_timeout, @default_call_timeout)) do
      Connection.export(
        conn,
        %{
          path: path,
          subtree?: subtree?,
          interfaces: interfaces,
          handler: handler,
          call_timeout: call_timeout
        },
        timeout
      )
    end
  end

  @doc """
  Removes an export.

  Each call on the export still waiting for its answer is failed with
  `org.freedesktop.DBus.Error.Failed`, and answering it afterwards returns
  `{:error, :unknown_call}`. Removing an export that is already gone returns
  `:ok`.
  """
  @spec unexport(pid(), reference(), non_neg_integer()) ::
          :ok
          | {:error, :not_connected | :disconnected | :timeout | :remote_connection_unsupported}
  def unexport(conn, ref, timeout \\ @default_timeout)
      when is_pid(conn) and is_reference(ref) and is_integer(timeout) and timeout >= 0 do
    Connection.unexport(conn, ref, timeout)
  end

  @doc """
  Answers a call with a method return carrying `body`.

  The reply is encoded with the method's declared out-signature,
  `call.out_signature`, in the calling process, so a body that does not fit
  it is reported here as `{:error, :signature_mismatch}` and the call stays
  open for another answer.

  ## Options

  - `:timeout` - milliseconds to wait for the connection to accept the
    reply. Defaults to `5000`.

  ## Return values

  - `:ok` - the reply was handed to the socket, or the caller asked for no
    reply and nothing was sent.
  - `{:error, :signature_mismatch}` - `body` does not fit `call.out_signature`.
  - `{:error, :unknown_call}` - the call was already answered, expired, or
    its export is gone.
  - `{:error, :timeout}` - delivery is ambiguous, as for `Rebus.send/3`. The
    call is complete either way.
  - `{:error, :disconnected}` - the connection stopped.
  - Any other reason is a failure to build or write the frame, as for
    `Rebus.send/3`. The call is complete unless the frame could not be built.
  """
  @spec reply(Call.t(), [term()], keyword()) :: :ok | {:error, reply_error()}
  def reply(%Call{} = call, body, opts \\ []) when is_list(body) and is_list(opts) do
    case Message.new(:method_return,
           reply_serial: call.serial,
           signature: call.out_signature,
           body: body
         ) do
      {:ok, msg} -> send_reply(call, msg, opts)
      {:error, :invalid_body} -> {:error, :signature_mismatch}
      {:error, _} = error -> error
    end
  end

  @doc """
  Answers a call with a D-Bus error named `name` and carrying `message`.

  `name` must be a valid D-Bus error name, such as
  `"org.example.Error.Refused"`; an invalid one returns
  `{:error, {:invalid_header_field, :error_name}}` and leaves the call open.
  Takes the same options and returns the same values as `reply/3`.
  """
  @spec reply_error(Call.t(), binary(), binary(), keyword()) :: :ok | {:error, reply_error()}
  def reply_error(%Call{} = call, name, message, opts \\ [])
      when is_binary(name) and is_binary(message) and is_list(opts) do
    case Message.new(:error,
           reply_serial: call.serial,
           error_name: name,
           signature: "s",
           body: [message]
         ) do
      {:ok, msg} -> send_reply(call, msg, opts)
      {:error, _} = error -> error
    end
  end

  @doc """
  Answers a call with a reply built by hand.

  `message` must be a `:method_return` or an `:error`. A method return must
  have the declared out-signature, `call.out_signature`, or this returns
  `{:error, :signature_mismatch}`; an error can carry any body. Whatever
  `reply_serial` and `destination` the message holds are replaced with the
  call's own. The message is encoded in the calling process first, so one
  that cannot be encoded returns the encoding error, as `Rebus.Message.encode/2`
  reports it, and leaves the call open. Takes the same options and returns
  the same values as `reply/3`, plus `{:error, {:invalid_message_type, type}}`
  for any other message type.
  """
  @spec reply_message(Call.t(), Message.t(), keyword()) ::
          :ok | {:error, reply_error() | Message.encoding_error()}
  def reply_message(%Call{} = call, %Message{} = message, opts \\ []) when is_list(opts) do
    with :ok <- check_reply_message(call, message),
         {:ok, _iodata} <- Message.encode(message) do
      send_reply(call, message, opts)
    end
  end

  defp check_reply_message(%Call{} = call, %Message{type: :method_return} = message) do
    if Message.signature(message) == call.out_signature,
      do: :ok,
      else: {:error, :signature_mismatch}
  end

  defp check_reply_message(%Call{}, %Message{type: :error}), do: :ok

  defp check_reply_message(%Call{}, %Message{type: type}),
    do: {:error, {:invalid_message_type, type}}

  # A call that expects no reply is not tracked by the connection, so there
  # is nothing to complete there and nothing to send.
  defp send_reply(%Call{no_reply?: true}, %Message{}, _opts), do: :ok

  defp send_reply(%Call{} = call, %Message{} = msg, opts) do
    timeout = Keyword.get(opts, :timeout, @default_timeout)
    Connection.reply_call(call.conn, call.ref, msg, timeout)
  end

  @doc """
  Emits the signal `member` of `interface` from the object at `path`.

  Given a `Rebus.Interface`, the signal must be one it declares, and `body`
  is encoded with the declared signature. Given an interface name, the
  signature must be passed as the `:signature` option. The signal is sent
  as `Rebus.send/3` sends any message: the connection does not check that
  anything is exported at `path`.

  ## Options

  - `:signature` - the body's signature. Required with an interface name,
    and refused with a `Rebus.Interface`, which supplies its own.
  - `:destination` - the unique name to send the signal to, rather than
    broadcasting it.
  - `:flags` and `:fds` - as for `Rebus.Message.new/2`.
  - `:timeout` - milliseconds to wait for the connection to accept the
    signal. Defaults to `5000`.

  ## Return values

  - `:ok` - the signal was handed to the socket.
  - `{:error, :unknown_signal}` - the interface does not declare `member`.
  - `{:error, :signature_required}` - an interface name without `:signature`.
  - `{:error, {:invalid_option, opts}}` - an option not listed above.
  - Any error of `Rebus.Message.new/2`, such as `{:error, :invalid_body}` for
    a body that does not fit the signature, or of `Rebus.send/3`.
  """
  @spec emit(pid(), binary(), Interface.t() | binary(), binary(), [term()], keyword()) ::
          :ok
          | {:error,
             :unknown_signal
             | :signature_required
             | {:invalid_option, term()}
             | Message.construction_error()
             | Rebus.error_reason()}
  def emit(conn, path, interface, member, body, opts \\ [])
      when is_pid(conn) and is_list(body) and is_list(opts) do
    {timeout, opts} = Keyword.pop(opts, :timeout, @default_timeout)

    with {:ok, signal} <- build_signal(interface, path, member, body, opts) do
      Rebus.send(conn, signal, timeout)
    end
  end

  defp build_signal(%Interface{} = interface, path, member, body, opts),
    do: Interface.signal(interface, path, member, body, opts)

  defp build_signal(interface, path, member, body, opts) when is_binary(interface) do
    with :ok <- validate_signal_opts(opts),
         {:ok, signature} <- fetch_signature(opts) do
      Message.new(
        :signal,
        [path: path, interface: interface, member: member, signature: signature, body: body] ++
          Keyword.delete(opts, :signature)
      )
    end
  end

  defp validate_signal_opts(opts) do
    if Keyword.keyword?(opts) and
         Enum.all?(opts, fn {key, _value} -> key in @signal_options end),
       do: :ok,
       else: {:error, {:invalid_option, opts}}
  end

  defp fetch_signature(opts) do
    case Keyword.fetch(opts, :signature) do
      {:ok, signature} -> {:ok, signature}
      :error -> {:error, :signature_required}
    end
  end

  @doc """
  Emits `org.freedesktop.DBus.Properties.PropertiesChanged` for properties
  of `interface` on the object at `path`.

  `changed` maps property names to their new values, typed by the
  declaration, and `invalidated` names properties that changed without
  their values. Each property's `emits_changed` decides where it may appear,
  as `Rebus.Interface.properties_changed/5` describes: one that emits `true`
  only in `changed`, one that emits `:invalidates` only in `invalidated`,
  and one that emits `false` or `:const` in neither.

  Takes the options `emit/6` takes, except `:signature`, and returns what it
  returns, plus `Rebus.Interface.properties_changed/5`'s errors for a
  property the interface does not declare or does not allow where it
  appears.
  """
  @spec properties_changed(
          pid(),
          binary(),
          Interface.t(),
          %{binary() => term()},
          [binary()],
          keyword()
        ) ::
          :ok
          | {:error,
             Interface.property_error()
             | {:invalid_property_change, binary()}
             | {:invalid_option, term()}
             | Message.construction_error()
             | Rebus.error_reason()}
  def properties_changed(conn, path, %Interface{} = interface, changed, invalidated, opts \\ [])
      when is_pid(conn) and is_map(changed) and is_list(invalidated) and is_list(opts) do
    {timeout, opts} = Keyword.pop(opts, :timeout, @default_timeout)

    with {:ok, signal} <-
           Interface.properties_changed(interface, path, changed, invalidated, opts) do
      Rebus.send(conn, signal, timeout)
    end
  end

  # Makes a method call to an object exported on `conn` without leaving the
  # node, so local code such as an object manager can ask another object's
  # handler something through the same routing, checks and bounds a peer's
  # call meets. The call is encoded and decoded first, so its handler sees
  # exactly what it would for the same call from a peer, and so is the
  # reply, so this returns what `Rebus.call/3` would. A handler's reply that
  # carries descriptors is refused with `{:error, :unix_fd_unsupported}`.
  # The handler must not be the calling process, which would wait for itself
  # until `timeout`.
  @doc false
  @spec invoke(pid(), binary(), binary() | nil, binary(), [term()], binary(), non_neg_integer()) ::
          {:ok, Message.t()}
          | {:error, {:dbus_error, Message.t()} | Message.encoding_error() | Rebus.error_reason()}
  def invoke(conn, path, interface, member, body, signature, timeout)
      when is_pid(conn) and is_list(body) and is_integer(timeout) and timeout >= 0 do
    fields =
      [path: path, interface: interface, member: member, signature: signature, body: body]
      |> Enum.reject(fn {_key, value} -> is_nil(value) end)

    with {:ok, msg} <- Message.new(:method_call, fields),
         {:ok, msg} <- as_received(msg) do
      conn |> Connection.invoke(msg, timeout) |> received_result()
    end
  end

  defp received_result({:ok, %Message{} = reply}), do: as_received(reply)

  defp received_result({:error, {:dbus_error, %Message{} = reply}}) do
    with {:ok, reply} <- as_received(reply), do: {:error, {:dbus_error, reply}}
  end

  defp received_result({:error, _reason} = error), do: error

  # The encoder accepts some values in more than one shape, such as an
  # integer for a double, while the decoder produces one.
  defp as_received(%Message{} = msg) do
    with {:ok, iodata} <- Message.encode(msg) do
      iodata |> IO.iodata_to_binary() |> Message.decode()
    end
  end

  defp validate_path(path) do
    if WireValue.valid_object_path?(path), do: :ok, else: {:error, :invalid_path}
  end

  defp validate_interfaces([_ | _] = interfaces) do
    Enum.reduce_while(interfaces, MapSet.new(), fn
      %Interface{name: name}, _seen when name in @reserved_interfaces ->
        {:halt, {:error, {:invalid_interface, {:reserved, name}}}}

      %Interface{name: name}, seen ->
        if MapSet.member?(seen, name),
          do: {:halt, {:error, {:invalid_interface, {:duplicate, name}}}},
          else: {:cont, MapSet.put(seen, name)}

      other, _seen ->
        {:halt, {:error, {:invalid_interface, other}}}
    end)
    |> case do
      {:error, _} = error -> error
      _seen -> :ok
    end
  end

  defp validate_interfaces(interfaces), do: {:error, {:invalid_interface, interfaces}}

  # A handler is monitored, and a monitor on a remote process also reports
  # node failures, which would unexport on a network partition.
  defp validate_handler(handler) when is_pid(handler) and node(handler) == node(),
    do: {:ok, handler}

  defp validate_handler(_handler), do: {:error, :invalid_handler}

  defp validate_subtree(subtree?) when is_boolean(subtree?), do: {:ok, subtree?}
  defp validate_subtree(_subtree), do: {:error, :invalid_subtree}

  defp validate_call_timeout(:infinity), do: {:ok, :infinity}

  defp validate_call_timeout(timeout) when is_integer(timeout) and timeout > 0,
    do: {:ok, timeout}

  defp validate_call_timeout(_timeout), do: {:error, :invalid_call_timeout}
end
