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
