### Added

- `use Rebus.Object` runs an exported object as a GenServer.
  `Rebus.Object.start_link/3` exports the object before calling its `init/1`,
  so a supervisor restart exports it again, and maps each call to a
  callback: `handle_method/3` for methods, and `handle_get_property/3` and
  `handle_set_property/4` for `Get`, `GetAll` and `Set`. The callback's
  return value answers the call, or `{:noreply, state}` leaves it for
  `Rebus.Service.reply/3` to answer later from any process. A successful
  `Set` emits `PropertiesChanged` as the property's `emits_changed` declares,
  unless the callback returns `emit: false`. Exceptions are not rescued: the
  object crashes and the caller receives `org.freedesktop.DBus.Error.Failed`.
  The object stops with its connection. `Rebus.Object.dispatch/4` gives a
  GenServer that keeps its own loop the same mapping.
- `Rebus.Bus.request_name/4` and `Rebus.Bus.release_name/3` request and
  release well-known bus names, taking the `:allow_replacement`,
  `:replace_existing` and `:do_not_queue` flags as atoms and returning the
  bus's answer as `:primary_owner`, `:in_queue`, `:exists`, `:already_owner`,
  `:released`, `:non_existent` or `:not_owner`. The process that requested a
  name, or the `:owner` given instead, receives
  `{:rebus_name, conn, :acquired | :lost, name}` each time the connection
  gains or loses it, until the name is released or that process exits.
- `Rebus.Service` exports objects and answers the method calls made on them.
  `Rebus.Service.export/4` binds `Rebus.Interface` declarations at an object
  path, or a whole subtree, to a handler process, which receives each call as
  `{export_ref, %Rebus.Service.Call{}}` and answers it with
  `Rebus.Service.reply/3`, `Rebus.Service.reply_error/4` or
  `Rebus.Service.reply_message/3`. The connection checks every call against
  the declarations first, so a handler only sees declared methods with
  arguments of the declared types, and callers get the standard D-Bus error
  for anything else. A call without an interface resolves to the one exported
  interface that declares its method. Pending calls are failed with
  `org.freedesktop.DBus.Error.Failed` when their handler exits or their
  object is unexported (`Rebus.Service.unexport/3`), and expire with
  `org.freedesktop.DBus.Error.NoReply` after the export's `:call_timeout`,
  60 seconds by default.
- A handler receives the Unix file descriptors sent with a method call in
  `call.unix_fds`, and borrows them until the call completes. The connection
  closes them exactly once, whether the call is answered, expires, or fails
  because its handler exited or its object was unexported, including for a
  call that asked for no reply. `Rebus.Service.take_fds/2` transfers their
  ownership to the handler instead, through the same handover as an
  FD-bearing reply to `Rebus.call/3`. `Rebus.Service.reply/3` takes an
  `:fds` option to send descriptors with a reply, as does
  `Rebus.Service.reply_error/4`, and Rebus borrows them as it does for every
  outbound message. A reply that sends a descriptor the connection still
  owns, such as one the call itself borrowed, returns
  `{:error, :unix_fd_borrowed}`.
- A connection answers `org.freedesktop.DBus.Introspectable.Introspect` for
  the objects it exports, with XML generated from their `Rebus.Interface`
  declarations, so `busctl tree`, `busctl introspect`, `gdbus introspect` and
  `d-feet` can find and describe them. Every path above an exported object,
  from `/` down, answers with its child nodes. A path served by a subtree
  export lists the subtree's interfaces and no child nodes.
- Exported objects answer `org.freedesktop.DBus.Properties`. The connection
  checks each `Get`, `Set` and `GetAll` against the declaration of the
  interface it names and answers `UnknownInterface`, `UnknownProperty`,
  `PropertyReadOnly` (for a `Set` of a read-only property) or `InvalidArgs`
  for a call that does not fit, including a `Get` of a write-only property
  and a `Set` value of the wrong type. Anything else goes to the handler that
  exported the interface, as a `Rebus.Service.Call` whose `out_signature` is
  `"v"`, `""` or `"a{sv}"`. `GetAll` for an interface with no readable
  property answers an empty dictionary.
- `Rebus.Service.emit/6` sends a signal from an exported object. Given a
  `Rebus.Interface`, it encodes the body with the signal's declared
  signature; given an interface name, it takes the signature as the
  `:signature` option. `Rebus.Service.properties_changed/6` sends
  `org.freedesktop.DBus.Properties.PropertiesChanged`, typing each value by
  its declaration and refusing a property whose `emits_changed` does not
  allow it where it appears.
- `Rebus.connect/2` takes a `:max_pending_calls` option, 1024 by default,
  that bounds how many inbound calls may wait for a handler's answer at once.
  Calls beyond it are answered with `org.freedesktop.DBus.Error.LimitsExceeded`.
- `Rebus.Interface` declares a D-Bus interface's methods, signals, properties
  and annotations, validating every name and signature once. It provides the
  standard `Peer`, `Introspectable`, `Properties` and `ObjectManager`
  interfaces, and builds signals, `PropertiesChanged` signals and `a{sv}`
  property bodies typed by the declaration. It is the groundwork for exporting
  objects from Rebus.
- `Rebus.Introspection.to_xml/1` renders a `Rebus.Introspection.Node` as
  D-Bus introspection XML, with no XML library dependency.
- `Rebus.unique_name/1` returns `{:ok, name}` with the unique name the message
  bus assigned to a connection in reply to `Hello`, such as `":1.42"`, or
  `{:ok, nil}` for a connection created with `bus: false`.
- `{:rebus_match_uncertain, conn, ref, rule}` notices tell each holder of a
  reference on a match rule, and the connection's `:owner` with `nil` for the
  reference, that Rebus can no longer prove what the bus holds for that rule.
- `Rebus.add_match/3` returns `{:error, :match_rule_uncertain}` for an uncertain
  rule, and `{:error, :match_recovery_capacity_exhausted}` for a new rule while
  `:match_recovery_max_rules` rules on the connection are unresolved.
- `Rebus.connect/2` takes a `:max_message_size` option that caps the size of
  each inbound message, from 4096 bytes up to the protocol limit of 128 MiB,
  which stays the default. A peer that declares a larger message stops the
  connection with `:message_too_large` before Rebus buffers the message body,
  so an untrusted peer can no longer make a connection hold 128 MiB. An
  invalid value returns `{:error, :invalid_max_message_size}`.

### Changed

- A method call to a path with nothing exported is now answered with
  `org.freedesktop.DBus.Error.UnknownObject` rather than
  `org.freedesktop.DBus.Error.UnknownMethod`, including on a connection that
  exports nothing. `org.freedesktop.DBus.Peer` is still answered on every
  path.
- **Breaking:** `Rebus.call/3` now returns a D-Bus error reply from the peer as
  `{:error, {:dbus_error, %Rebus.Message{type: :error}}}` rather than
  `{:error, %Rebus.Message{type: :error}}`, so it no longer shares a shape
  with local and transport failures such as `{:error, :timeout}`. The message
  is unchanged and still carries any received descriptors in `:unix_fds`.

  ```elixir
  # Before
  {:error, %Rebus.Message{type: :error} = error} -> handle_error(error)

  # After
  {:error, {:dbus_error, %Rebus.Message{} = error}} -> handle_error(error)
  ```

- Connections that negotiate Unix file descriptor passing now read inbound
  messages in bulk, as other connections do, rather than with at least two
  reads per message. Received descriptors go to the messages that begin in the
  same read, in order, according to each message's declared descriptor count.
  Descriptors that no such message claims are closed and logged.
- Rebus no longer closes a connection when match-rule cleanup fails. A rule
  whose cleanup exhausts `:match_recovery_max_attempts`, that finds
  `:match_recovery_max_rules` cleanups already running, or whose operation task
  is lost is left uncertain and reported instead, and the application decides
  whether to close the connection. Its references keep working and can still
  be removed. An operation whose task is lost now returns
  `{:error, :match_rule_uncertain}` rather than `{:error, :disconnected}`.
- The process that manages a connection's match rules now lives and dies with
  the connection instead of being restarted from saved state. If it fails,
  which would be a bug in Rebus, the connection stops with
  `{:shutdown, {:match_subscription_exited, reason}}` and the bus discards its
  rules, as it does when that process is stopped by a restart of Rebus's own
  supervision tree. Rebus no longer returns
  `{:error, :match_subscription_state_lost}`.
- An established connection that the peer closes or resets now logs
  `D-Bus connection transport stopped` at `info` rather than `warning`, so a
  bus restart no longer floods logs with warnings. The same stop during
  connection setup, and every other transport error, is still a warning.
- On Linux, Rebus reads the local uid for `EXTERNAL` authentication from
  `/proc/self/status` rather than running `id -u`. Where it still runs `id`
  (on other platforms, and for the `DBUS_COOKIE_SHA1` username), it uses
  `/usr/bin/id` or `/bin/id` and searches `PATH` only when neither exists,
  ignoring a relative match.

### Fixed

- `EXTERNAL` authentication now completes when the peer answers it with an
  empty `DATA` challenge, which the D-Bus specification permits. Rebus repeats
  its identity once and then expects `OK` or `REJECTED`. A peer `ERROR` now
  cancels `EXTERNAL` and moves on to the next advertised mechanism, as it would
  after `REJECTED`, rather than failing the connection with `:auth_failed`.
- `Rebus.connect/2` now treats `:timeout` (or `:read_timeout`, when given) as
  one setup budget for a direct address, as it already did for `:system` and
  `:session`. The identity lookup, socket connect and authentication share it,
  so a slow peer can no longer hold setup for up to three times the timeout.
  An expired budget still returns `{:error, :read_timeout}`.
- `Rebus.close/1` now stops a connection that is still connecting its socket or
  authenticating at once. It used to wait until setup finished or timed out,
  and when setup's budget outlasted the connection's 5-second shutdown timeout
  the connection was killed. The `connect/2` call waiting for that connection
  now returns `{:error, :shutdown}`, and a `:system` or `:session` connect no
  longer moves on to the next bus address.
