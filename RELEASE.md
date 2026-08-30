# Release Notes

## Unreleased

### Added

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

### Fixed

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
