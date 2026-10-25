# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- %% CHANGELOG_ENTRIES %% -->

## 0.4.0-rc.1 - 2026-10-25

Rebus can now serve D-Bus as well as call it: applications can export
objects, answer method calls and own bus names. Unix file descriptors arrive
as handles that close themselves, and passing them is opt-in.

### Added

- Services. `Rebus.Service` exports objects described by `Rebus.Interface`
  declarations and sends each validated method call to a handler process.
  `use Rebus.Object` runs an exported object as a GenServer. Exported objects
  answer `Properties` and `Introspectable`, so `busctl`, `gdbus` and `d-feet`
  can find and describe them, and `Rebus.Service.emit/6` and
  `Rebus.Service.properties_changed/6` send typed signals. See the
  [Exporting objects](services.html) guide.
- `Rebus.Bus.request_name/4` and `Rebus.Bus.release_name/3` own well-known
  bus names and notify the owner when a name is acquired or lost.
- `Rebus.unique_name/1` returns a connection's unique bus name.
- `Rebus.connect/2` takes `:max_pending_calls` and `:max_message_size` to
  bound inbound work.
- Precompiled descriptor-passing libraries for x86_64 and aarch64 Linux
  (glibc and musl) and macOS. Other targets build from source with `make`
  and a C++17 compiler.

### Changed

- **Breaking:** Unix file descriptor passing is opt-in with
  `config :rebus, unix_fds: true`. Without it, Rebus is pure Elixir. See
  [Enabling descriptor passing](unix_fds.html#enabling-descriptor-passing).
- **Breaking:** descriptors are `Rebus.UnixFD` handles rather than raw
  numbers. A handle closes its descriptor exactly once, when it is closed,
  when its owner exits or when it is garbage collected. Use
  `Rebus.UnixFD.with_fd/2` to work with the number.
- **Breaking:** `Rebus.call/3` returns a D-Bus error reply as
  `{:error, {:dbus_error, message}}`.
- Calls to a path with nothing exported are answered with `UnknownObject`.
- A failed match-rule cleanup no longer closes the connection. Rebus sends a
  `:rebus_match_uncertain` notice instead.

### Fixed

- Fixes to connection setup timeouts, `EXTERNAL` authentication and
  descriptor handling on macOS.

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
