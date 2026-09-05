# Changelog

All notable changes to this project are documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

<!-- %% CHANGELOG_ENTRIES %% -->

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
