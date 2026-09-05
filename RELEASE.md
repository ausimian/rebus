### Added

- Answer inbound method calls instead of dropping them: an unhandled call now
  receives an `org.freedesktop.DBus.Error.UnknownMethod` error reply, so a
  calling peer fails immediately instead of blocking until its own timeout.
  Replies join the caller write queue, honour `:write_timeout`, and stop being
  queued once a stalled transport has left too many of them unwritten.
- Implement `org.freedesktop.DBus.Peer`. `Ping` returns an empty reply and
  `GetMachineId` returns the host machine id, as `busctl` and `d-feet` expect.
- Add the `bus: false` connection option for peer-to-peer D-Bus endpoints that
  are not a message bus. Rebus skips Hello, the connection has no unique name,
  and `add_match/3` returns `{:error, :not_a_bus}`. This makes
  `allow_anonymous: true` usable for its intended peer-to-peer case.
- Add `Rebus.MatchRule` and `Rebus.add_match/3`/`remove_match/3` for bounded,
  validated D-Bus signal subscriptions. Canonical rules share bus registrations
  across independent handler references, filter only supported criteria
  locally, and clean up on handler or connection teardown. Ambiguous cleanup
  retries with bounded backoff, queues same-rule callers by their deadlines,
  and safely resets a connection at a bounded recovery capacity; sender routing
  remains bus-owned when a well-known name is forwarded as a unique name.
  Worker restarts preserve stable subscriptions; a restart during an in-flight
  operation returns an explicit state-loss error until teardown rather than
  silently acknowledging an unknown reference.
- Keep ordinary owner-exit removals separate from ambiguous recovery capacity,
  queueing excess initial cleanup safely. Reject overlapping subscriptions with
  incompatible sender predicates rather than cross-delivering a signal whose
  well-known sender cannot be safely recovered from its forwarded unique name.

- Negotiate D-Bus authentication mechanisms after `EXTERNAL` is rejected.
  Rebus supports `DBUS_COOKIE_SHA1` using a bounded, private per-user
  `~/.dbus-keyrings` cookie read and cryptographically strong client
  challenges, including TCP endpoints where Unix credentials cannot be passed.
  `ANONYMOUS` is available only with `allow_anonymous: true`; it provides no
  authentication, confidentiality, or integrity and is intended only for
  deliberately unauthenticated peer-to-peer use. Authentication failures return
  bounded payload-free errors and never log cookie material, challenges,
  authorization identities, GUIDs, or peer authentication text.
  `:auth_cookie_unavailable` stops bus-address fallback, avoiding disclosure to
  later candidates or IPs. When a username is unavailable before cookie `AUTH`
  starts, an explicitly opted-in, peer-advertised `ANONYMOUS` attempt is sent
  directly; after cookie `AUTH` begins, cookie failures are terminal and never
  fall back to `ANONYMOUS`. A final `$HOME` symlink is supported after target
  ownership/permission validation; keyring directories and cookie files remain
  non-symlink-only.
- Add negotiated Unix file-descriptor passing for Linux and macOS local Unix
  sockets.
  `Rebus.Message.new/2` accepts borrowed `:fds`, validates `h` indexes and the
  D-Bus `unix_fds` count, and delivers validated inbound descriptors with live
  method replies in `message.unix_fds`. `Rebus.UnixFD.close/1` closes an owned
  inbound raw descriptor exactly once. TCP and peers that reject
  `NEGOTIATE_UNIX_FD` reject FD-bearing frames before any frame bytes are sent.
  A stale borrowed descriptor fails as `:unix_fd_send_failed` before framing
  and leaves the connection usable. Inbound FD-bearing signals and complete
  frames with invalid FD count/index/negotiation metadata are closed and
  dropped without stopping unrelated calls or signal handlers. Rebus retains
  reply descriptors until public call delivery is internally acknowledged via a
  caller-local one-shot alias. The initial handoff has a 100 ms grace and a
  250 ms claim deadline, but a queued acknowledgement is resolved FIFO without
  a second wall-clock limit so Rebus never reports failure while it could still
  transfer ownership. An FD-bearing call can therefore complete after its
  reply timeout while a live connection dispatches that resolver;
  `:fd_claim_expired` means descriptors were definitively closed and a stopped
  connection reports `:disconnected`. Late timeouts, cancellation, caller
  death, and ordinary `Rebus.close/1` teardown close descriptors safely;
  untrappable `:kill` remains outside that cleanup guarantee.
  Declined negotiation receives a bounded ancillary buffer and quarantines an
  illicit complete frame while preserving coalesced successors; `MSG_CTRUNC`
  remains fail-closed because omitted descriptors cannot be closed.
  Other Unix and BSD variants remain out of scope for FD passing.
- Parse D-Bus system and session address lists, including percent-escaped
  `unix:path`, Linux `unix:abstract`, and `tcp` host/port entries. Address
  selection follows the listed fallback order, tries all IPv6 then IPv4 TCP
  results when no family is specified (capped at four per family), verifies a
  valid address `guid` against `AUTH OK`, accepts bounded libdbus-compatible
  literal values and parameterless unsupported transports, ignores other
  forward-compatible parameters, and divides one address-selection setup budget
  fairly between one caller-owned auth-ID lookup, resolver, IP, and
  later-address attempts.
- Add the documented `:timeout` and `:name` connection options. `:timeout`
  bounds socket setup and authentication reads, while `:name` registers the
  connection process locally.
- Add `Rebus.close/1` to explicitly stop a supervisor-owned local connection.
- Add `Rebus.call/3` for public method calls with configurable timeouts and
  `Rebus.send/2` and `Rebus.send/3` for fire-and-forget messages.
- Add the `:write_timeout` connection option to bound outbound frame readiness.
- Add `Rebus.Message.max_message_size/0`, exposing the 128 MiB D-Bus message
  limit.
- Add `Rebus.Message.max_array_size/0` and `max_scalar_elements/0`, exposing
  the 64 MiB D-Bus wire-array limit and the local 1,000,000 scalar-element cap.
- Add the `:read_timeout` connection option to bound connection setup and the
  complete initial Hello reply, plus gaps between inbound fragments without
  timing out idle connections.

### Changed

- **Breaking:** `Rebus.call/3` no longer returns a bare `%Rebus.Message{}`. A
  successful reply is `{:ok, %Rebus.Message{type: :method_return}}` and a D-Bus
  error reply is `{:error, %Rebus.Message{type: :error}}`; both carry the
  complete message, including any received descriptors the caller owns.
- **Breaking:** `Rebus.add_signal_handler/1` returns `{:ok, reference()}`
  instead of a bare `reference()`.
- **Breaking:** `Rebus.Message.new/2` and `Rebus.Message.validate/1` return
  pattern-matchable reasons instead of strings: `:invalid_type`,
  `:invalid_flags`, `:invalid_version`, `:invalid_body`, `:invalid_signature`,
  `:invalid_header_fields`, `{:invalid_header_field, field}`,
  `{:missing_header_field, field}` and `{:unknown_header_field, field}`. No
  caller-supplied value appears in a reason.
- **Breaking:** `Rebus.connect!/2` raises `ArgumentError` instead of
  `RuntimeError`, matching `Rebus.Message.new!/2` and `Rebus.MatchRule.new!/1`.

- Rebus now requires Elixir 1.18 or later.
- Connection log entries now carry their drop or stop reason as `reason:`
  Logger metadata, so handlers can filter on it. The message text is unchanged.
- Clarify the public Quick Start with `send/2`, document exact TCP
  socket-address maps, and remove the stale test-count claim.
- CI now runs the ordinary Unix-FD lifecycle suite on every valid combination
  in the current Elixir 1.18--1.20 and OTP 27--29 matrix for both Linux and
  macOS. Other Unix and BSD variants, and Windows, remain outside the
  supported FD-passing scope.
- Treat `MSG_CTRUNC` as a fail-closed `:unix_fd_truncated` connection error
  even when SCM_RIGHTS decoding also finds malformed or over-limit control
  data. Rebus closes every descriptor decoded from the received control list
  before stopping.
- Document `Rebus.connect/2` as the sole supported connection construction API.
  Connection internals are excluded from generated documentation, and
  match-subscription recovery now resets only supervisor-owned connections.
- `Rebus.Message.new/2` now returns `{:error, :invalid_body}` and `new!/2`
  raises when a body cannot be encoded for its signature, including out-of-range
  D-Bus integers. `encode/2` now also returns `:invalid_header_fields` for
  malformed manually constructed messages, `:invalid_message` for malformed
  envelopes, and `:message_too_large` for frames over D-Bus size limits.
- Message signatures are now validated as complete D-Bus type expressions;
  malformed, unbalanced, oversized, or over-nested signatures are rejected
  before encoding, including `g` values in bodies and variants.
- Decoding now rejects messages with missing or invalid required header fields.
  The D-Bus wire limits remain 64 MiB for an array payload and 128 MiB for a
  complete frame. Separately, each header or body decode locally allows up to
  100,000 structural terms and 1,000,000 materialized fixed-width scalar
  elements. Encoding applies the 1,000,000 scalar-element cap cumulatively
  across all fixed-width scalar arrays in one operation. Wire-valid frames over
  these local resource caps are dropped without closing an established
  connection.
- D-Bus special doubles use `:infinity`, `:negative_infinity`, and `:nan`.
  NaN payload and sign are canonicalized on decode.
- `call/3` now returns `{:error, {:reply_dropped, :method_return}}` when a
  successful peer reply exceeds local decode caps, or
  `{:error, {:reply_dropped, {:error, error_name}}}` for a dropped D-Bus error
  reply. The peer definitely received the request and produced a reply; decide
  whether to retry from the operation and error semantics, never blindly.
- `connect/2` now returns `{:error, {:auth_rejected, mechanisms}}` for a
  `REJECTED` authentication response instead of `{:error, :auth_failed}`.
- `connect/2` now waits for a validated initial `Hello` reply before returning
  its connection PID, so that PID can be used immediately.
- Named connection collisions now return `{:error, {:name_taken, pid}}`, so a
  caller can adopt or close the existing local connection.
- Collisions with unrelated local registrations now return
  `{:error, {:name_registered, pid}}` instead of advertising that PID as a
  Rebus connection.
- `call/3`, `send/2`, `send/3`, and signal-handler registration/removal now
  return `{:error, :not_connected}` during setup and safe timeout/disconnection
  errors when their connection is unavailable.
- Document that an operation issued to a named PID before its corresponding
  `connect/2` returns can time out before any frame is written, and is safe to
  retry after setup succeeds.
- Treat any frame other than a valid Hello reply as a protocol error before a
  connection is established.

### Removed

- Remove the Windows completion-socket I/O path. Rebus supports Linux and
  macOS; Windows was never tested or documented as supported.

### Fixed

- Validate D-Bus interface, member, error and bus names consistently against
  the specification; single-element interface or unique names such as `Foo` or
  `:1` are now rejected everywhere.
- Filter directed signals for a well-known match `sender` by exact sender
  header, preventing a peer's unique name from satisfying that predicate while
  preserving legitimate bus-driver signals.
- Return `:invalid_body` when a message body cannot be encoded for its
  signature, rather than silently declaring an empty body.
- Reject duplicate or trailing body data in decoded D-Bus messages, and accept
  hyphens in valid well-known bus names.
- Reject control characters in outbound D-Bus names and paths, and reject body
  arrays over the D-Bus 64 MiB array limit before emitting a frame.
- Bound scalar-array materialization independently of wire-size limits to avoid
  large BEAM list allocations from otherwise valid inbound frames.
- Treat local container-nesting exhaustion as a nonfatal resource limit for an
  established connection, while malformed D-Bus signature grammar remains an
  invalid message.
- Allow independent connections to establish concurrently so a stalled
  authentication handshake does not block the shared connection supervisor.
- Reject inbound D-Bus frames over the protocol's 128 MiB limit or with
  header-fields arrays over the 64 MiB array limit, and validate malformed
  fixed headers as soon as they arrive.
- Keep partial inbound frame storage bounded when peers fragment messages into
  very small socket writes.
- Fall back to OTP's default receive buffer if a socket backend rejects either
  supported receive-buffer option form.
- Bound socket setup, complete authentication, and initial Hello reads with
  `:read_timeout`, including peers that dribble progress indefinitely.
- Return stable `:auth_id_unavailable` and named-connection collision errors
  when the local authentication identity cannot be read or a requested local
  registration name is already in use.
- Reject invalid or oversized unique names in initial `Hello` replies before
  retaining them for the connection lifetime.
- Copy retained authentication GUIDs and unique names so small validated values
  cannot pin large peer-controlled receive buffers.
- Keep signal-handler removal scoped to the connection that registered it.
- Copy peer-provided Hello error names and authentication mechanisms before
  returning them, so callers cannot retain a larger receive buffer indirectly.
- Accept fragmented D-Bus authentication responses and initial Hello replies.
- Reject D-Bus messages whose array/struct or total container nesting exceeds
  the protocol limits.
- Reject array elements that do not consume input instead of spinning while
  decoding malformed messages.
- Return clean connection and protocol errors instead of invalid GenServer
  callback values.
- Return `nil` while a streamed message header's array length is incomplete,
  preventing valid fragmented messages from being treated as fatal errors.
- Correct declared byte lengths for arrays requiring position-dependent padding,
  including variants and nested arrays of 8-byte-aligned values (`av`, `aav`,
  `aax`, and `aad`), which could otherwise produce malformed messages and make
  self-decoding fail.
