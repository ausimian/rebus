# Rebus

Rebus is a D-Bus client for Elixir. It speaks the D-Bus wire format, connects
to a message bus or a peer, makes method calls, receives signals through match
rules, and passes Unix file descriptors on Linux and macOS.

## Features

- Wire format encoding and decoding, with bounded size and nesting inbound
- System bus, session bus, Unix socket and TCP connections via
  `Rebus.connect/2`, supervised and outliving the caller
- `Rebus.call/3` for a correlated reply, `Rebus.send/2` for signals and
  no-reply calls
- Signals delivered to the registering process, all of them or those a
  structured `Rebus.MatchRule` selects
- Unix file descriptor passing over local sockets on Linux and macOS
- Authentication with `EXTERNAL`, `DBUS_COOKIE_SHA1` and opt-in `ANONYMOUS`
- `org.freedesktop.DBus.Peer` answered for you, other inbound calls refused as
  `UnknownMethod`

## Installation

Add `rebus` to your dependencies in `mix.exs`. Rebus requires Elixir 1.18 or
later.

```elixir
def deps do
  [
    {:rebus, "~> 0.2"}
  ]
end
```

## Quick start

```elixir
{:ok, conn} = Rebus.connect(:session)

# Call a method and wait for the reply.
call = Rebus.Message.new!(:method_call,
  path: "/org/freedesktop/DBus",
  interface: "org.freedesktop.DBus",
  destination: "org.freedesktop.DBus",
  member: "ListNames"
)
{:ok, %Rebus.Message{type: :method_return, body: [names]}} = Rebus.call(conn, call)

# Emit a signal without waiting for anything.
signal = Rebus.Message.new!(:signal,
  path: "/com/example/Object",
  interface: "com.example.Interface",
  member: "Changed",
  signature: "s",
  body: ["updated"]
)
:ok = Rebus.send(conn, signal)

# Receive every signal that arrives on the connection.
{:ok, ref} = Rebus.add_signal_handler(conn)
# {^ref, %Rebus.Message{type: :signal}} now arrives in this process
```

A successful reply is `{:ok, %Rebus.Message{type: :method_return}}` and a
D-Bus error reply is `{:error, %Rebus.Message{type: :error}}`, both carrying
the whole message. Other failures are atoms, such as `{:error, :disconnected}`.
`{:error, :timeout}` is delivery-ambiguous, because the peer may already have
the message, so do not blindly retry it. `Rebus.call/3` lists every reason.

## Connections

`Rebus.connect/2` is the only supported constructor. It returns a supervised
PID that you pass to every other Rebus function, and `Rebus.close/1` releases
it. Rebus supports Linux and macOS, the platforms exercised in CI.

```elixir
{:ok, conn} = Rebus.connect(:system)
{:ok, conn} = Rebus.connect(:session)
{:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/my-dbus"})
{:ok, conn} = Rebus.connect(%{family: :inet, addr: {127, 0, 0, 1}, port: 12345})
```

`:system` and `:session` read a D-Bus address list: `:system` from
`DBUS_SYSTEM_BUS_ADDRESS`, else the `:system_bus_address` config key, else
`unix:path=/run/dbus/system_bus_socket`; `:session` from
`DBUS_SESSION_BUS_ADDRESS`, else `unix:path=$XDG_RUNTIME_DIR/bus`.
Rebus tries the supported entries in order and skips transports it cannot use.
See [Authentication](https://hexdocs.pm/rebus/authentication.html) for what an
address list may hold.

Pass `bus: false` to talk to an endpoint that is not a message bus. Rebus then
sends no `Hello`, the connection has no unique name, and `Rebus.add_match/3`
returns `{:error, :not_a_bus}`.

A connection outlives the process that created it and lives until
`Rebus.close/1`. Pass `owner: pid` when the state it holds on the bus — a
published service, a requested name, an inhibitor — belongs to one process
instead: Rebus monitors that process and stops the connection when it exits,
so the bus reclaims that state. See `Rebus.connect/2` for the full address,
option, error and timeout contract.

## Signals

Use `Rebus.MatchRule` and `Rebus.add_match/3` to have a bus route broadcast
signals to you:

```elixir
rule = Rebus.MatchRule.new!(
  interface: "org.freedesktop.DBus",
  member: "NameOwnerChanged",
  args: %{0 => "org.example.Service"}
)

{:ok, ref} = Rebus.add_match(conn, rule, 1_000)
# {^ref, %Rebus.Message{type: :signal}} now arrives here
:ok = Rebus.remove_match(conn, ref)
```

Raw rule strings and `eavesdrop` are not accepted. Equivalent subscriptions
share one bus rule and get their own references. Use
`Rebus.add_signal_handler/1` instead for every signal on the connection.

See [Signal subscriptions and match rules](https://hexdocs.pm/rebus/match_rules.html).

## Unix file descriptors

A local Unix-socket connection can carry raw file descriptors. Attach them
with `:fds`, and index them from the body with `h` values:

```elixir
message = Rebus.Message.new!(:signal,
  path: "/com/example/Object",
  interface: "com.example.Interface",
  member: "DescriptorReady",
  signature: "h",
  body: [0],
  fds: [fd]
)
```

Rebus borrows outbound descriptors, so the sender keeps ownership. Descriptors
received with a `Rebus.call/3` reply arrive in `message.unix_fds`, and the
calling process must close each one exactly once with `Rebus.UnixFD.close/1`.

See [Unix file descriptor passing](https://hexdocs.pm/rebus/unix_fds.html).

## Authentication

`Rebus.connect/2` authenticates before it returns. It tries `EXTERNAL` first,
then `DBUS_COOKIE_SHA1` if the peer offers it, and `ANONYMOUS` only when you
pass `allow_anonymous: true`. Cookie authentication reads `~/.dbus-keyrings`,
and a cookie failure never falls back to `ANONYMOUS`.

See [Authentication](https://hexdocs.pm/rebus/authentication.html).

## Inbound method calls

Rebus answers method calls made *to* a connection: `org.freedesktop.DBus.Peer`
(`Ping` and `GetMachineId`) is implemented, and every other method call is
refused with `org.freedesktop.DBus.Error.UnknownMethod` rather than being
dropped. A call flagged `:no_reply_expected` receives no reply. There is no API
for serving your own methods yet.

## Scope

Rebus does not implement all of D-Bus. What is missing today:

- No service-side API. You cannot export an object or serve your own methods.
- No proxies and no introspection. You build and match messages yourself.
- Windows is not supported, and descriptor passing on other Unix variants is
  untested.

The module list is in the [published documentation](https://hexdocs.pm/rebus),
grouped by core, wire format and errors.

## Testing

Run the unit suite with `mix test`. It uses an in-process test server.

### Integration tests

`test/integration` runs against a real `dbus-daemon` instead of the in-process
test server. It is tagged `:integration` and excluded from `mix test`. On a
Linux host with D-Bus installed, run `dbus-run-session -- mix test --only
integration`; the private session bus keeps the suite off any bus already
running. Elsewhere (macOS included) `mix test.integration` runs that same
command inside the container defined by `docker/Dockerfile`, which needs
Docker. Without `DBUS_SESSION_BUS_ADDRESS` the suite skips with a message
rather than failing.

## Documentation

Run `mix docs` to build the documentation locally. The published version is at
[hexdocs.pm/rebus](https://hexdocs.pm/rebus).

## License

This project is licensed under the MIT License.

## Contributing

Work on a feature branch and open a pull request against `main`. Run
`mix precommit` first; it compiles with warnings as errors, formats, runs
Credo and runs the tests.
