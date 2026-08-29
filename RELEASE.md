# Release Notes

## Unreleased

### Added

- Add `Rebus.call/3` for public method calls with configurable timeouts and
  `Rebus.send/2` and `Rebus.send/3` for fire-and-forget messages.
- Add the `:write_timeout` connection option to bound outbound frame readiness.

### Fixed

- Return clean connection and protocol errors instead of invalid GenServer
  callback values.
- Return `nil` while a streamed message header's array length is incomplete,
  preventing valid fragmented messages from being treated as fatal errors.
- Correct declared byte lengths for arrays requiring position-dependent padding,
  including variants and nested arrays of 8-byte-aligned values (`av`, `aav`,
  `aax`, and `aad`), which could otherwise produce malformed messages and make
  self-decoding fail.
