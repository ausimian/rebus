### Added

- Services. `Rebus.Service` exports objects on a connection and answers the
  method calls made on them. `Rebus.Service.export/4` binds `Rebus.Interface`
  declarations at an object path, or a whole subtree, to a handler process,
  which receives each call as `{export_ref, %Rebus.Service.Call{}}` and
  answers it with `Rebus.Service.reply/3`, `Rebus.Service.reply_error/4` or
  `Rebus.Service.reply_message/3`. The connection checks every call against
  the declarations first, so a handler only sees declared methods with
  arguments of the declared types. Unanswered calls expire with
  `org.freedesktop.DBus.Error.NoReply` after the export's `:call_timeout`,
  60 seconds by default, and are failed with
  `org.freedesktop.DBus.Error.Failed` when their handler exits or their
  object is unexported. See the [Exporting objects](services.html) guide.
- `use Rebus.Object` runs an exported object as a GenServer, mapping each
  call to `handle_method/3`, `handle_get_property/3` or
  `handle_set_property/4`. The callback's return value answers the call, or
  `{:noreply, state}` leaves it for `Rebus.Service.reply/3` to answer later
  from any process. `Rebus.Object.start_link/3` exports the object before its
  `init/1` runs, so a supervisor restart exports it again, and the object
  stops with its connection. `Rebus.Object.dispatch/4` gives a GenServer with
  its own loop the same mapping.
- `Rebus.Bus.request_name/4` and `Rebus.Bus.release_name/3` request and
  release well-known bus names. Flags are atoms (`:allow_replacement`,
  `:replace_existing`, `:do_not_queue`) and the bus's answer is returned as
  an atom such as `:primary_owner` or `:exists`. The requesting process, or
  the `:owner` given instead, receives
  `{:rebus_name, conn, :acquired | :lost, name}` whenever the connection
  gains or loses the name.
- `Rebus.Interface` declares an interface's methods, signals, properties and
  annotations, validating every name and signature once. It provides the
  standard `Peer`, `Introspectable`, `Properties` and `ObjectManager`
  interfaces and builds signals and `a{sv}` property bodies typed by the
  declaration.
- Exported objects answer `org.freedesktop.DBus.Properties`. The connection
  checks each `Get`, `Set` and `GetAll` against the declaration and answers
  `UnknownInterface`, `UnknownProperty`, `PropertyReadOnly` or `InvalidArgs`
  for calls that do not fit. Valid calls reach the handler as ordinary
  `Rebus.Service.Call`s. A `Rebus.Object` emits `PropertiesChanged` after a
  successful `Set` as the property's `emits_changed` declares.
- `Rebus.Service.emit/6` sends a signal from an exported object, and
  `Rebus.Service.properties_changed/6` sends
  `org.freedesktop.DBus.Properties.PropertiesChanged`, both typed by the
  interface declaration.
- Connections answer `org.freedesktop.DBus.Introspectable.Introspect` for
  exported objects, with XML generated from their declarations, and for every
  path above them, so `busctl tree`, `gdbus introspect` and `d-feet` can
  find and describe a Rebus service. `Rebus.Introspection.to_xml/1` renders a
  `Rebus.Introspection.Node` without an XML library.
- Descriptors sent with a method call arrive in `call.unix_fds`. The handler
  borrows them until the call completes and the connection closes them
  exactly once, however the call completes. `Rebus.Service.take_fds/2`
  transfers ownership to the handler instead. `Rebus.Service.reply/3` and
  `Rebus.Service.reply_error/4` take an `:fds` option to send descriptors
  with a reply; sending one the connection still owns returns
  `{:error, :unix_fd_borrowed}`.
- `Rebus.connect/2` takes `:max_pending_calls`, 1024 by default, which bounds
  how many inbound calls may wait for a handler's answer at once. Calls
  beyond it are answered with `org.freedesktop.DBus.Error.LimitsExceeded`.
- `Rebus.connect/2` takes `:max_message_size`, which caps each inbound
  message from 4096 bytes up to the protocol limit of 128 MiB, the default.
  A peer that declares a larger message stops the connection with
  `:message_too_large` before the body is buffered. An invalid value returns
  `{:error, :invalid_max_message_size}`.
- `Rebus.unique_name/1` returns the unique name the bus assigned to a
  connection, or `{:ok, nil}` for a connection made with `bus: false`.
- `{:rebus_match_uncertain, conn, ref, rule}` notices tell each holder of a
  match-rule reference, and the connection's `:owner` with `nil` for the
  reference, that Rebus can no longer prove what the bus holds for that
  rule. `Rebus.add_match/3` returns `{:error, :match_rule_uncertain}` for
  such a rule, and `{:error, :match_recovery_capacity_exhausted}` for a new
  rule while `:match_recovery_max_rules` rules are unresolved. See
  [Signal subscriptions and match rules](match_rules.html).

### Changed

- **Breaking:** `Rebus.call/3` returns a D-Bus error reply from the peer as
  `{:error, {:dbus_error, %Rebus.Message{type: :error}}}` rather than
  `{:error, %Rebus.Message{type: :error}}`, so it no longer shares a shape
  with local and transport failures such as `{:error, :timeout}`. The message
  itself is unchanged and still carries received descriptors in `:unix_fds`.

  ```elixir
  # Before
  {:error, %Rebus.Message{type: :error} = error} -> handle_error(error)

  # After
  {:error, {:dbus_error, %Rebus.Message{} = error}} -> handle_error(error)
  ```

- A method call to a path with nothing exported is answered with
  `org.freedesktop.DBus.Error.UnknownObject` rather than `UnknownMethod`,
  including on a connection that exports nothing. `org.freedesktop.DBus.Peer`
  is still answered on every path.
- Rebus no longer closes a connection when match-rule cleanup fails. The
  rule is left uncertain and reported with a `:rebus_match_uncertain` notice,
  and the application decides whether to close the connection. An operation
  whose task is lost now returns `{:error, :match_rule_uncertain}` rather
  than `{:error, :disconnected}`.
- The process that manages a connection's match rules now lives and dies
  with the connection instead of being restarted from saved state. If it
  fails, the connection stops with
  `{:shutdown, {:match_subscription_exited, reason}}` and the bus discards
  its rules. `{:error, :match_subscription_state_lost}` is no longer
  returned.
- Connections that negotiate descriptor passing now read inbound messages in
  bulk, as other connections do. Received descriptors go to the messages
  that begin in the same read, in order, by each message's declared
  descriptor count; descriptors no message claims are closed and logged.
- A peer closing or resetting an established connection is now logged at
  `info` rather than `warning`, so a bus restart no longer floods the log.
  The same stop during setup, and every other transport error, is still a
  warning.
- On Linux, the local uid for `EXTERNAL` authentication is read from
  `/proc/self/status` rather than by running `id -u`. Where `id` is still
  run, Rebus uses `/usr/bin/id` or `/bin/id` and searches `PATH` only when
  neither exists, ignoring a relative match.

### Fixed

- `EXTERNAL` authentication now completes when the peer answers with an
  empty `DATA` challenge, which the specification permits. A peer `ERROR`
  now cancels `EXTERNAL` and moves on to the next mechanism rather than
  failing the connection with `:auth_failed`.
- `Rebus.connect/2` now treats `:timeout` (or `:read_timeout`) as one setup
  budget for a direct address, as it already did for `:system` and
  `:session`, so a slow peer can no longer hold setup for up to three times
  the timeout.
- `Rebus.close/1` now stops a connection that is still connecting or
  authenticating at once, instead of waiting for setup to finish or time
  out. The `connect/2` call waiting on it returns `{:error, :shutdown}`, and
  a `:system` or `:session` connect no longer moves on to the next bus
  address.
