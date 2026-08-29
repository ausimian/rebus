# Release Notes

## Unreleased

### Added

- Add `Rebus.call/3` for public method calls with configurable timeouts and
  `Rebus.send/2` and `Rebus.send/3` for fire-and-forget messages.
- Add the `:write_timeout` connection option to bound outbound frame readiness.
- Add `Rebus.Message.max_message_size/0`, exposing the 128 MiB D-Bus message
  limit.
- Add the `:read_timeout` connection option to bound connection setup and the
  complete initial Hello reply, plus gaps between inbound fragments without
  timing out idle connections.

### Changed

- Treat any frame other than a valid Hello reply as a protocol error before a
  connection is established.

### Fixed

- Reject inbound D-Bus frames over the protocol's 128 MiB limit or with
  header-fields arrays over the 64 MiB array limit, and validate malformed
  fixed headers as soon as they arrive.
- Keep partial inbound frame storage bounded when peers fragment messages into
  very small socket writes.
- Bound socket setup, complete authentication, and initial Hello reads with
  `:read_timeout`, including peers that dribble progress indefinitely.
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
