# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- %% CHANGELOG_ENTRIES %% -->

## 0.3.0 - 2026-09-06

Rebus 0.3 expands the client API for method calls, signal
subscriptions, descriptor passing, and authenticated bus or peer connections.
It includes several breaking corrections to public results and wire handling.

### Added

- Added `Rebus.call/3`, `Rebus.send/2`, `Rebus.send/3`, and `Rebus.close/1` for
  client operations and explicit connection lifetime management. Connections
  are supervised, outlive their creator by default, and can instead follow an
  `owner: pid`.
- Added structured signal subscriptions with `Rebus.MatchRule`,
  `Rebus.add_match/3`, and `Rebus.remove_match/3`, including recovery when a
  timed-out bus operation leaves the rule's state uncertain.
- Added Unix file descriptor passing on Linux and macOS local sockets. Senders
  retain ownership of outbound descriptors; callers own received reply
  descriptors and close them with `Rebus.UnixFD.close/1`.
- Added `DBUS_COOKIE_SHA1` authentication, opt-in `ANONYMOUS` authentication,
  ordered D-Bus address lists, Unix and TCP addresses, and server-GUID checks.

### Changed

- Bare version tags now publish the corresponding package and documentation to
  Hex after the test suite passes.
- **Breaking:** calls now return the complete reply as `{:ok, message}` or a
  complete D-Bus error reply as `{:error, message}`; operation errors and
  signal-handler registration results also use the new documented shapes.
- **Breaking:** message construction and validation now use structured atom or
  tuple reasons and reject invalid bodies, signatures, headers, and D-Bus
  names that earlier versions accepted.
- **Breaking:** decoding rejects invalid Boolean values and non-zero alignment
  padding, and special doubles are represented as `:infinity`,
  `:negative_infinity`, and `:nan`.
- **Breaking:** `Rebus.connect/2` is now the supported connection constructor,
  waits for bus registration before returning, and reports authentication and
  registered-name failures with the new documented results; `connect!/2` now
  raises `ArgumentError` on failure.
- **Breaking:** Rebus now requires Elixir 1.18 or later.
- Connections now answer `org.freedesktop.DBus.Peer` calls and
  reject other inbound method calls with `UnknownMethod`.

### Removed

- **Breaking:** removed Windows support. Rebus supports Linux and macOS.

### Fixed

- Corrected array lengths, fragmented-message handling, connection failure
  paths, body-encoding failures, timed-out reply cleanup, well-known names
  containing hyphens, and standard system/session bus address fallbacks.

## 0.2.0 - 2025-11-21

### Added

- `Rebus.connect/2` accepts the `:system` and `:session` bus aliases, resolving
  the system bus from the `:rebus, :system_bus_address` application setting and
  the session bus from `DBUS_SESSION_BUS_ADDRESS`.
- `Rebus.connect!/2`, which raises instead of returning `{:error, reason}`.
- `Rebus.Message.signature/1` for reading a message's body signature.

### Changed

- `Rebus.Message` no longer carries a separate `:signature` struct field; the
  signature is stored in `header_fields` and set automatically when a body is
  present.

## 0.1.1 - 2025-11-04

### Fixed

- Corrected errors in the public API documentation.

## 0.1.0 - 2025-11-04

### Added

- Initial release: D-Bus message encoder, decoder and wire-format types,
  message construction and validation, and a supervised bus connection with
  signal handlers.
