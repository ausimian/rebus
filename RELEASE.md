Rebus is now a usable D-Bus client: method calls, signal subscriptions,
descriptor passing, authentication negotiation and full bus-address support.
It is a breaking release: result shapes changed and validation is stricter.

### Added

- `Rebus.call/3` sends a method call and waits for its reply.
- `Rebus.send/2` and `Rebus.send/3` send a message without waiting for a reply.
- `Rebus.close/1` stops a connection you no longer need.
- `Rebus.MatchRule` with `Rebus.add_match/3` and `Rebus.remove_match/3`
  subscribes the caller to the signals the bus routes on request.
- Unix file descriptor passing on Linux and macOS local sockets: `fds:` to
  send, `unix_fds` to receive, `Rebus.UnixFD.close/1` to release each one.
- Authentication negotiation: `DBUS_COOKIE_SHA1`, and opt-in `ANONYMOUS`.
- Address-list parsing for `:system` and `:session`: `unix:path`,
  `unix:abstract` and `tcp` entries in order, with `guid=` verification.
- The `:timeout`, `:name`, `:read_timeout`, `:write_timeout`,
  `:allow_anonymous`, `:bus` and `:owner` connection options.
- `bus: false` connects to a peer-to-peer endpoint rather than a message bus.
- `owner: pid` ties a connection's lifetime to a process: Rebus stops the
  connection when that process exits, so the bus reclaims the names, services
  and other state the connection held. Without it a connection still lives
  until `Rebus.close/1`.
- Inbound method calls are answered: `org.freedesktop.DBus.Peer` is
  implemented, and everything else gets an `UnknownMethod` error reply.
- `Rebus.Message.max_message_size/0`, `max_array_size/0` and
  `max_scalar_elements/0` report the size limits Rebus enforces.
- `Rebus.ProtocolLimitError`, raised by `Rebus.Encoder` when an array exceeds
  the D-Bus 64 MiB array limit; `Rebus.Message` reports it as
  `:message_too_large`.
- Guides for descriptor passing, authentication and match rules.

### Changed

- **Breaking:** `Rebus.call/3` returns `{:ok, msg}`, and a D-Bus error reply as
  `{:error, %Rebus.Message{type: :error}}`.
- **Breaking:** `Rebus.add_signal_handler/1` returns `{:ok, ref}`.
- **Breaking:** `Rebus.Message.new/2` and `validate/1` return atom and tuple
  reasons instead of strings.
- **Breaking:** `Rebus.connect!/2` raises `ArgumentError`.
- **Breaking:** `Rebus.connect/2` waits for the `Hello` reply before returning,
  and a rejected handshake is `{:error, {:auth_rejected, mechanisms}}`.
- **Breaking:** connections and operations also return `{:name_taken, pid}`,
  `{:name_registered, pid}`, `:not_connected` and `{:reply_dropped, outcome}`.
- **Breaking:** validation now rejects invalid bodies, incomplete signatures,
  missing or invalid required headers, and malformed or over-long D-Bus names.
- **Breaking:** decoding rejects booleans other than 0 and 1, and non-zero
  alignment padding, including between the header and the body, as libdbus
  does.
- **Breaking:** doubles use `:infinity`, `:negative_infinity` and `:nan`.
- **Breaking:** Rebus requires Elixir 1.18 or later.
- Signals are delivered by the connection that received them, so an internal
  failure no longer stops established connections.
- Connection log entries carry their reason as `reason:` Logger metadata.
- `Rebus.connect/2` is the only supported way to create a connection, and the
  supervisor owns it until `Rebus.close/1` or, with `owner: pid`, until that
  process exits.

### Removed

- The Windows completion-socket path. Rebus supports Linux and macOS.

### Fixed

- Arrays such as `av` and `aad` declared a wrong length and were malformed.
- Fragmented header fields and 12 to 15 byte fragments killed the connection.
- Connections crashed instead of returning an error on some failure paths.
- A body that failed to encode was silently sent as an empty body.
- Pending replies leaked when a call timed out or no reply arrived.
- Well-known bus names containing hyphens were rejected.
- `:system` and `:session` honour `DBUS_SYSTEM_BUS_ADDRESS` and fall back to
  `$XDG_RUNTIME_DIR/bus`, as other D-Bus implementations do.
