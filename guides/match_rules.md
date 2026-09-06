# Signal subscriptions and match rules

A message bus forwards a broadcast signal only to connections that asked for
it. Rebus gives you two ways to receive signals.

## At a glance

- `Rebus.add_signal_handler/1` delivers every signal that arrives on the
  connection.
- `Rebus.add_match/3` asks the bus to route matching broadcast signals to you.
- Both deliver `{ref, %Rebus.Message{}}` to the process that registered.
- Build rules with `Rebus.MatchRule.new!/1`. Raw rule strings are not accepted.
- Handlers are per connection, and never see signals from another connection.

## Every signal on the connection

```elixir
{:ok, ref} = Rebus.add_signal_handler(conn)
# {^ref, %Rebus.Message{type: :signal}} arrives here
:ok = Rebus.delete_signal_handler(conn, ref)
```

This asks the bus for nothing. It gives you the signals the connection already
receives, such as signals directed at your unique name.

## Routed subscriptions

`Rebus.add_match/3` registers a rule with the bus and returns a reference:

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

A connection opened with `bus: false` has no bus driver to ask, so
`add_match/3` returns `{:error, :not_a_bus}`.

## Building a rule

`Rebus.MatchRule.new!/1` accepts `:sender`, `:interface`, `:member`, `:path`,
`:path_namespace`, `:destination`, `:args`, `:arg_paths` and `:arg0namespace`.
`:path` and `:path_namespace` cannot be combined. Rules always match signals.

The encoded rule is limited to 1024 bytes, which is the D-Bus limit.
`eavesdrop` is deliberately not accepted; use a dedicated monitoring
connection for that.

Two equivalent rules share one registration with the bus. Each `add_match/3`
call still returns its own reference, and the bus rule is removed once the
last reference is removed or its owning process exits.

## Sender matching

Rebus checks a unique `:sender` name itself, on top of whatever the bus
routed.

A well-known `:sender` is left to the bus for broadcast signals, because the
bus forwards them under the sender's current unique name. A directed signal
bypasses bus routing, so Rebus accepts one for a well-known `:sender` only
when the signal's sender header is that exact name.

D-Bus does not say which rule admitted a signal. Rebus therefore rejects a
rule that overlaps an existing one with a different sender, returning
`{:error, :sender_routing_ambiguous}`, rather than delivering a signal to the
wrong subscription.

## When a subscription fails

`{:error, :timeout}` from `add_match/3` is ambiguous. You get no reference,
but the bus may already hold the rule. Rebus cleans up in the background while
the connection lives. A later `add_match/3` for the same rule waits behind
that cleanup under its own timeout, and returns
`{:error, :match_rule_cleanup_pending}` if the cleanup outlasts it.

Two outcomes end differently. Rebus closes the connection when too many rules
are left uncertain, which makes the bus discard all of them. And
`{:error, :match_subscription_state_lost}` means an operation in flight lost
its state, and that reference stays unresolved until the connection closes.
Both closes are `Rebus.close/1`, which only works on a connection created by
`Rebus.connect/2`; a connection started any other way never gets closed, so the
lost state lasts as long as that connection process does.

Closing a connection stops its local handlers, and the bus discards every
match rule it held.

## Errors

`Rebus.add_match/3` and `Rebus.remove_match/3` return these reasons.

| Reason | Meaning |
| --- | --- |
| `:not_a_bus` | The connection was opened with `bus: false`. Nothing was sent. |
| `:sender_routing_ambiguous` | The rule overlaps an existing one with a different sender. |
| `:timeout` | The operation did not finish in time. The bus may still hold the rule. |
| `:match_rule_cleanup_pending` | An earlier ambiguous operation on the same rule is still being cleaned up. |
| `:match_subscription_state_lost` | An in-flight operation lost its state and cannot be resolved on this connection. Rebus ends it by closing a connection created by `Rebus.connect/2`; the state dies with the connection. |
| `{:bus_error, error_name}` | The bus returned a D-Bus error reply. |
| `:invalid_bus_reply` | The bus reply did not have the expected shape. |
| `:not_connected` | Connection setup has not completed. |
| `:disconnected` | The connection stopped. |
| `:encode_failed` | The request could not be encoded. |
| `:serial_exhausted` | Every D-Bus serial is in use. |
| `:fd_claim_expired` | Descriptors arrived with the bus reply and were closed. |
| `{:reply_dropped, outcome}` | The bus replied, but the reply exceeded a local decoding cap. |
| `:remote_connection_unsupported` | The connection PID belongs to another node. |
