### Added

- `{:rebus_match_uncertain, conn, ref, rule}` notices tell each holder of a
  reference on a match rule, and the connection's `:owner` with `nil` for the
  reference, that Rebus can no longer prove what the bus holds for that rule.
- `Rebus.add_match/3` returns `{:error, :match_rule_uncertain}` for an uncertain
  rule, and `{:error, :match_recovery_capacity_exhausted}` for a new rule while
  `:match_recovery_max_rules` rules on the connection are unresolved.

### Changed

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
  `{:error, :match_rule_uncertain}` rather than `{:error, :disconnected}`. A
  connection is still closed when a restarted match-subscription worker finds
  it lost track of an in-flight operation.
