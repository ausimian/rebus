# Rebus

An Elixir implementation of the D-Bus message protocol.

Rebus provides a clean, Elixir-native interface for communicating over D-Bus, the inter-process communication (IPC) and remote procedure call (RPC) mechanism that is standard on Linux desktop systems.

## Features

- **D-Bus Wire Format** - Bounded message encoding/decoding for the supported
  wire types, including 8-byte struct alignment
- **Multiple Connection Types** - Support for TCP/IP and Unix domain socket connections
- **Signal Handling** - Register handlers to receive D-Bus signals  
- **Message Encoding/Decoding** - Robust serialization of D-Bus messages with proper type handling
- **Supervised Connections** - Fault-tolerant connection management with automatic supervision

## Quick Start

```elixir
# Connect to a D-Bus endpoint
{:ok, conn} = Rebus.connect(:session)

# Build and call a D-Bus method
message = Rebus.Message.new!(:method_call,
  path: "/com/example/Object",
  interface: "com.example.Interface",
  member: "TestMethod",
  body: [42, "hello"],
  signature: "is"
)

# A reply is always the full D-Bus message, including error replies.
{:ok, %Rebus.Message{type: :method_return, body: [result]}} = Rebus.call(conn, message)

# Set a per-call timeout in milliseconds. Timed-out calls are cleaned up.
{:error, :timeout} = Rebus.call(conn, message, 1_000)

# Emit a signal (or a method call with :no_reply_expected) without waiting.
signal = Rebus.Message.new!(:signal,
  path: "/com/example/Object",
  interface: "com.example.Interface",
  member: "Changed",
  signature: "s",
  body: ["updated"]
)

:ok = Rebus.send(conn, signal)
```

`Rebus.call/3` returns `{:ok, %Rebus.Message{type: :method_return}}` for a
successful reply and `{:error, %Rebus.Message{type: :error}}` for a D-Bus error
reply; both carry the full message, including any received descriptors that the
caller owns. It returns `{:error, :timeout}` when no reply arrives before
the configured timeout, and `{:error, :encode_failed}` if the outgoing message
cannot be encoded. `Rebus.send/2` is fire-and-forget and is intended for
signals and method calls with the `:no_reply_expected` flag. `Rebus.call/3`
rejects signals and no-reply method calls; `Rebus.send/2` rejects method calls
that expect replies. Both functions return `{:error, :disconnected}` if the
connection closes while sending or waiting. `{:error, :timeout}` is
delivery-ambiguous: the message may already have reached the peer, so do not
blindly retry it. `{:error, :serial_exhausted}` means all D-Bus serials are in
use. Connections must be local to the caller's node; remote PIDs return
`{:error, :remote_connection_unsupported}`.

## Connection lifecycle

`Rebus.connect/2` is the sole supported way to create a connection. It returns
a supervisor-owned PID; pass that PID to Rebus APIs and release it with
`Rebus.close/1` when it is no longer needed. Starting or managing connection
processes directly is unsupported.

## Inbound method calls

Rebus answers method calls made *to* a connection: `org.freedesktop.DBus.Peer`
(`Ping` and `GetMachineId`) is implemented, and every other method call is
refused with `org.freedesktop.DBus.Error.UnknownMethod` rather than being
dropped. A call flagged `:no_reply_expected` receives no reply. There is no API
for serving your own methods yet.

## Peer-to-peer connections

```elixir
{:ok, conn} = Rebus.connect(%{family: :local, path: "/tmp/peer"}, bus: false)
```

`bus: false` connects to an endpoint that is not a message bus: Rebus sends no
`Hello`, the connection has no unique name, and `Rebus.add_match/3` returns
`{:error, :not_a_bus}`. It is rejected for `:system` and `:session`.

## Routed signal subscriptions

Use `Rebus.MatchRule` and `Rebus.add_match/3` to receive broadcast signals
through a D-Bus bus without constructing `AddMatch` and `RemoveMatch` method
calls yourself:

```elixir
rule = Rebus.MatchRule.new!(
  sender: "org.freedesktop.DBus",
  interface: "org.freedesktop.DBus",
  member: "NameOwnerChanged",
  args: %{0 => "org.example.Service"}
)

{:ok, ref} = Rebus.add_match(conn, rule, 1_000)

receive do
  {^ref, %Rebus.Message{body: [name, old_owner, new_owner]}} ->
    # Handle the matching signal.
end

:ok = Rebus.remove_match(conn, ref)
```

Rules are structured and canonical; raw rule strings and `eavesdrop` are not
accepted. Equivalent subscriptions share one bus rule but get their own
references, and the bus rule goes away when the last reference is removed or
its owner exits. Use `Rebus.add_signal_handler/1` instead to receive every
signal that arrives on the connection.

See [Signal subscriptions and match rules](https://hexdocs.pm/rebus/match_rules.html).

## Architecture

Rebus is built with a modular architecture:

- **`Rebus`** - Main API module for establishing connections and managing signal handlers
- **`Rebus.Message`** - Message creation, encoding, decoding, and validation
- **`Rebus.Encoder`** - D-Bus wire format encoding with proper alignment
- **`Rebus.Decoder`** - D-Bus wire format decoding with struct boundary tracking

## Supported platforms

Rebus supports Linux and macOS, the platforms exercised in CI. Other Unix
variants are untested (Unix file descriptor passing is Linux and macOS only),
and Windows is not supported.

## Connection Types

Rebus supports connecting to different types of D-Bus endpoints:

- **IPv4 TCP/IP connections** - `%{family: :inet, addr: {127, 0, 0, 1}, port: 12345}`
- **IPv6 TCP/IP connections** - `%{family: :inet6, addr: {0, 0, 0, 0, 0, 0, 0, 1}, port: 12345}`
- **Unix domain sockets** - `%{family: :local, path: "/path/to/socket"}`

## Bus addresses

`Rebus.connect(:system)` reads the configured `:system_bus_address` (falling
back to `/run/dbus/system_bus_socket`); `Rebus.connect(:session)` reads
`DBUS_SESSION_BUS_ADDRESS`. Both use D-Bus address lists such as:

```text
unix:path=/run/user/1000/bus,guid=30313233343536373839414243444546;tcp:host=localhost,port=12345
```

Rebus supports ordered `unix:path`, Linux `unix:abstract`, and `tcp` host/port
entries. TCP accepts `family=ipv4` or `family=ipv6`; without a family it tries
the bounded IPv6 results first, then IPv4 results, before moving to the next
address. Values use D-Bus percent escapes, so separators or control bytes in a
path must be escaped. A valid `guid` is verified against the server's `AUTH OK`
identity, while unsupported transports are skipped so a later supported entry
can be used. Malformed supported entries are rejected rather than skipped.

The list setup budget is one aggregate `:timeout` (or `:read_timeout`) budget
for DNS and pre-Hello setup; direct socket maps keep their normal independent
connection budgets. See `Rebus.connect/2` for the complete address, error, and
timeout contract.

## Authentication

`Rebus.connect/2` authenticates before it returns. It tries `EXTERNAL` first,
then `DBUS_COOKIE_SHA1` if the peer advertises it, and `ANONYMOUS` only when
you pass `allow_anonymous: true`. Cookie authentication needs a private
keyring under `~/.dbus-keyrings`, and a cookie failure never downgrades to
`ANONYMOUS`.

See [Authentication](https://hexdocs.pm/rebus/authentication.html).

## Message Types

Rebus supports all D-Bus message types:

- **`:method_call`** - Method invocations  
- **`:method_return`** - Method replies with returned data
- **`:error`** - Error responses
- **`:signal`** - Signal emissions

## Unix file descriptors

On Linux and macOS, a local Unix-socket connection can carry raw file
descriptors. Attach them with `:fds`; `h` values in the body are zero-based
indexes into that list:

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

Rebus borrows outbound descriptors and never closes them, so the sender keeps
ownership. A `Rebus.call/3` reply can carry received descriptors in
`message.unix_fds`; the calling process then owns them and must close each one
exactly once with `Rebus.UnixFD.close/1`. Signals and inbound method calls
never hand a descriptor to your code.

See [Unix file descriptor passing](https://hexdocs.pm/rebus/unix_fds.html).

## D-Bus wire-format scope

Rebus does not claim to implement the entire D-Bus specification. Its supported
wire-format and connection scope includes:

- Proper 8-byte struct alignment in arrays
- Header field encoding at correct positions
- Message size calculations with alignment padding
- The 128 MiB (2^27 byte) inbound message limit and 64 MiB (2^26 byte)
  header-fields array limit, with early fixed-header validation and a
  progress-aware deadline and bounded retained storage for incomplete frames
- D-Bus container nesting limits: 32 array levels, 32 struct levels, and 64
  total levels
- Array boundary tracking for consecutive arrays
- Position-aware encoding and decoding
- Optional Unix FD passing over negotiated local Unix sockets only

## Testing

Run the unit suite with `mix test`. It uses an in-process test server, so it
needs no D-Bus daemon.

### Integration tests

`test/integration` runs against a real `dbus-daemon` instead of the in-process
test server. It is tagged `:integration` and excluded from `mix test`. On a
Linux host with D-Bus installed, run `dbus-run-session -- mix test --only
integration`; the private session bus keeps the suite off any bus already
running. Elsewhere (macOS included) `mix test.integration` runs that same
command inside the container defined by `docker/Dockerfile`, which needs
Docker. Without `DBUS_SESSION_BUS_ADDRESS` the suite skips with a message
rather than failing.

## Installation

Add `rebus` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:rebus, "~> 0.2"}
  ]
end
```

## Documentation

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc):

```bash
mix docs
```

The published documentation is at [hexdocs.pm/rebus](https://hexdocs.pm/rebus).

## License

This project is licensed under the MIT License.

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin feature/my-new-feature`)
5. Create a new Pull Request

Run `mix precommit` before submitting a pull request.
