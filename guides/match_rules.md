# Signal subscriptions and match rules

A message bus forwards a broadcast signal only to connections that have
asked for it. Rebus gives you two ways to receive signals. Both deliver
`{ref, %Rebus.Message{}}` to the process that registered, and both belong to
one connection: a handler never sees signals from another connection.

## Every signal on the connection

```elixir
{:ok, ref} = Rebus.add_signal_handler(conn)
# {^ref, %Rebus.Message{type: :signal}} arrives here
:ok = Rebus.delete_signal_handler(conn, ref)
```

`Rebus.add_signal_handler/1` will result in all signals to that connection
being passed to your process.

## Subscribing with a match rule

`Rebus.add_match/3` registers a rule with the bus, which then routes the
matching broadcast signals to the connection:

```elixir
rule =
  Rebus.MatchRule.new!(
    sender: "org.freedesktop.DBus",
    interface: "org.freedesktop.DBus",
    member: "NameOwnerChanged",
    args: %{0 => "org.example.Service"}
  )

{:ok, ref} = Rebus.add_match(conn, rule, 1_000)

receive do
  {^ref, %Rebus.Message{body: [name, old_owner, new_owner]}} ->
    handle(name, old_owner, new_owner)
end

:ok = Rebus.remove_match(conn, ref)
```

Rules are built with `Rebus.MatchRule.new!/1`. Raw rule strings are not
accepted. It takes `:sender`, `:interface`, `:member`, `:path`,
`:path_namespace`, `:destination`, `:args`, `:arg_paths` and
`:arg0namespace`. `:path` and `:path_namespace` cannot be combined, rules
always match signals, and the encoded rule is limited to the D-Bus maximum of
1024 bytes. `eavesdrop` is not accepted; use a dedicated monitoring
connection for that.

Two equivalent rules share one registration with the bus. Each `add_match/3`
call still returns its own reference, and the bus rule is removed once the
last reference is removed or its owning process exits.

A connection opened with `bus: false` has no bus driver to ask, so
`add_match/3` returns `{:error, :not_a_bus}`.

## Sender matching

The bus routes broadcast signals, but Rebus checks the `:sender` of each
delivered signal itself.

A unique-name sender is a direct comparison. A well-known name is harder,
because a directed signal, one sent to a specific destination, bypasses the
bus's routing. Rebus tracks the name's current owner with `GetNameOwner` and
`NameOwnerChanged`, and accepts a directed signal only from that owner or
from the bus driver. Any other directed signal is dropped.

Ownership tracking is best effort. If a lookup fails, Rebus logs a warning
and retries with backoff. Directed delivery for that name stays off until
ownership is known; broadcast delivery is unaffected. If the retries run
out, Rebus logs an error and keeps rejecting directed signals from that
service for the life of the connection. A directed signal can also be missed
briefly while a new subscription obtains its first ownership answer.

## When a subscription fails

`{:error, :timeout}` from `add_match/3` is ambiguous: you have no reference,
but the bus may already hold the rule. Rebus cleans up in the background
while the connection lives. A later `add_match/3` for the same rule waits
behind that cleanup, under its own timeout, and returns
`{:error, :match_rule_cleanup_pending}` if the cleanup outlasts it.

A bus reply too large for the local decoding limit also surfaces as
`{:error, :timeout}`, because the connection cannot recover the serial it
would need to correlate the reply. `Rebus.call/3` describes that fallback.

### Uncertain rules

Sometimes Rebus cannot establish what the bus holds for a rule: the cleanup
exhausts its retries, too many other cleanups are already running, or the
task running an operation on the rule is lost. Rebus then marks the rule
uncertain, keeps the connection up, and notifies each process holding a
reference on it:

```elixir
receive do
  {:rebus_match_uncertain, ^conn, ^ref, %Rebus.MatchRule{}} ->
    # This application would rather start again than live with the rule.
    Rebus.close(conn)
end
```

The connection's `:owner`, if it has one, receives the same notice with
`nil` in place of the reference, once per rule. A process that is both gets
both. The notice is a four-element tuple, so a receive pattern written for
`{ref, %Rebus.Message{}}` never matches it by accident.

An uncertain rule stays uncertain for the life of the connection:

- Rebus does no more bus work for it. The bus may keep routing its signals
  to the connection, and removing the last reference does not send
  `RemoveMatch`.
- Existing references keep receiving signals until they are removed or their
  owners exit. `Rebus.remove_match/3` still stops delivery to a reference.
- `Rebus.add_match/3` for the same rule returns
  `{:error, :match_rule_uncertain}`.

Closing the connection is the only way to make the bus forget the rule.
Whether that is worth the disruption is your application's call.

### Tuning recovery

Two application environment settings bound the cleanup work:

- `:match_recovery_max_rules`, 64 by default, is how many rules may be in
  cleanup at once. A rule that needs cleanup beyond that is left uncertain.
  While the rules in cleanup and the uncertain rules together reach this
  limit, `add_match/3` returns `{:error, :match_recovery_capacity_exhausted}`
  for any rule the connection does not already hold. An equivalent rule
  already held is still served, since it adds nothing on the bus.
- `:match_recovery_max_attempts`, 30 by default, is how many cleanup
  attempts a rule gets before it is left uncertain. A transient local
  failure is not proof that the bus resolved the rule, so recovery continues
  through those.

Each connection's match rules are managed by a process linked to it, and
that process is the only record of the rules and handlers the connection
holds. If it fails, which would be a bug in Rebus, the connection stops with
`{:shutdown, {:match_subscription_exited, reason}}`. Stopping removes the
connection's handlers and makes the bus discard every rule it held.

## Errors

`Rebus.add_match/3` and `Rebus.remove_match/3` return these reasons.

| Reason | Meaning |
| --- | --- |
| `:not_a_bus` | The connection was opened with `bus: false`. Nothing was sent. |
| `:sender_routing_ambiguous` | The rule overlaps an existing one with a different sender. |
| `:timeout` | The operation did not finish in time, or the reply header exceeded a local decoding limit. The bus may still hold the rule; see `Rebus.call/3` for the header-limit fallback. |
| `:match_rule_cleanup_pending` | An earlier ambiguous operation on the same rule is still being cleaned up. |
| `:match_rule_uncertain` | Rebus has left the rule uncertain, or the operation's task was lost and left it so. See [Uncertain rules](#uncertain-rules). |
| `:match_recovery_capacity_exhausted` | Too many rules on the connection are unresolved to accept a new one. Nothing was sent. |
| `{:bus_error, error_name}` | The bus returned a D-Bus error reply. |
| `:invalid_bus_reply` | The bus reply did not have the expected shape. |
| `:not_connected` | Connection setup has not completed. |
| `:disconnected` | The connection stopped. |
| `:not_started` | Rebus could not start the local task for the operation. Nothing was sent to the bus and the rule and its references are unchanged, so retrying is safe. |
| `:encode_failed` | The request could not be encoded. |
| `:serial_exhausted` | Every D-Bus serial is in use. |
| `{:reply_dropped, outcome}` | The bus replied, but the reply exceeded a local decoding cap. |
| `:remote_connection_unsupported` | The connection PID belongs to another node. |
