### Added

- `Rebus.unique_name/1` returns `{:ok, name}` with the unique name the message
  bus assigned to a connection in reply to `Hello`, such as `":1.42"`, or
  `{:ok, nil}` for a connection created with `bus: false`.
- `{:rebus_match_uncertain, conn, ref, rule}` notices tell each holder of a
  reference on a match rule, and the connection's `:owner` with `nil` for the
  reference, that Rebus can no longer prove what the bus holds for that rule.
- `Rebus.add_match/3` returns `{:error, :match_rule_uncertain}` for an uncertain
  rule, and `{:error, :match_recovery_capacity_exhausted}` for a new rule while
  `:match_recovery_max_rules` rules on the connection are unresolved.
- `Rebus.connect/2` takes a `:max_message_size` option that caps the size of
  each inbound message, from 4096 bytes up to the protocol limit of 128 MiB,
  which stays the default. A peer that declares a larger message stops the
  connection with `:message_too_large` before Rebus buffers the message body,
  so an untrusted peer can no longer make a connection hold 128 MiB. An
  invalid value returns `{:error, :invalid_max_message_size}`.

### Changed

- **Breaking:** `Rebus.call/3` now returns a D-Bus error reply from the peer as
  `{:error, {:dbus_error, %Rebus.Message{type: :error}}}` rather than
  `{:error, %Rebus.Message{type: :error}}`, so it no longer shares a shape
  with local and transport failures such as `{:error, :timeout}`. The message
  is unchanged and still carries any received descriptors in `:unix_fds`.

  ```elixir
  # Before
  {:error, %Rebus.Message{type: :error} = error} -> handle_error(error)

  # After
  {:error, {:dbus_error, %Rebus.Message{} = error}} -> handle_error(error)
  ```

- Connections that negotiate Unix file descriptor passing now read inbound
  messages in bulk, as other connections do, rather than with at least two
  reads per message. Received descriptors go to the messages that begin in the
  same read, in order, according to each message's declared descriptor count.
  Descriptors that no such message claims are closed and logged.
- Rebus no longer closes a connection when match-rule cleanup fails. A rule
  whose cleanup exhausts `:match_recovery_max_attempts`, that finds
  `:match_recovery_max_rules` cleanups already running, or whose operation task
  is lost is left uncertain and reported instead, and the application decides
  whether to close the connection. Its references keep working and can still
  be removed. An operation whose task is lost now returns
  `{:error, :match_rule_uncertain}` rather than `{:error, :disconnected}`.
- The process that manages a connection's match rules now lives and dies with
  the connection instead of being restarted from saved state. If it fails,
  which would be a bug in Rebus, the connection stops with
  `{:shutdown, {:match_subscription_exited, reason}}` and the bus discards its
  rules, as it does when that process is stopped by a restart of Rebus's own
  supervision tree. Rebus no longer returns
  `{:error, :match_subscription_state_lost}`.
- An established connection that the peer closes or resets now logs
  `D-Bus connection transport stopped` at `info` rather than `warning`, so a
  bus restart no longer floods logs with warnings. The same stop during
  connection setup, and every other transport error, is still a warning.
- On Linux, Rebus reads the local uid for `EXTERNAL` authentication from
  `/proc/self/status` rather than running `id -u`. Where it still runs `id`
  (on other platforms, and for the `DBUS_COOKIE_SHA1` username), it uses
  `/usr/bin/id` or `/bin/id` and searches `PATH` only when neither exists,
  ignoring a relative match.

### Fixed

- `EXTERNAL` authentication now completes when the peer answers it with an
  empty `DATA` challenge, which the D-Bus specification permits. Rebus repeats
  its identity once and then expects `OK` or `REJECTED`. A peer `ERROR` now
  cancels `EXTERNAL` and moves on to the next advertised mechanism, as it would
  after `REJECTED`, rather than failing the connection with `:auth_failed`.
- `Rebus.connect/2` now treats `:timeout` (or `:read_timeout`, when given) as
  one setup budget for a direct address, as it already did for `:system` and
  `:session`. The identity lookup, socket connect and authentication share it,
  so a slow peer can no longer hold setup for up to three times the timeout.
  An expired budget still returns `{:error, :read_timeout}`.
- `Rebus.close/1` now stops a connection that is still connecting its socket or
  authenticating at once. It used to wait until setup finished or timed out,
  and when setup's budget outlasted the connection's 5-second shutdown timeout
  the connection was killed. The `connect/2` call waiting for that connection
  now returns `{:error, :shutdown}`, and a `:system` or `:session` connect no
  longer moves on to the next bus address.
