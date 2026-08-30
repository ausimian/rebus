# Rebus

An Elixir implementation of the D-Bus message protocol.

Rebus provides a clean, Elixir-native interface for communicating over D-Bus, the inter-process communication (IPC) and remote procedure call (RPC) mechanism that is standard on Linux desktop systems.

## Features

- **D-Bus Wire Protocol Compliance** - Full implementation of the D-Bus specification including 8-byte struct alignment
- **Multiple Connection Types** - Support for TCP/IP and Unix domain socket connections
- **Signal Handling** - Register handlers to receive D-Bus signals  
- **Message Encoding/Decoding** - Robust serialization of D-Bus messages with proper type handling
- **Supervised Connections** - Fault-tolerant connection management with automatic supervision
- **Comprehensive Testing** - Extensive test suite with 200+ tests ensuring reliability

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
%Rebus.Message{type: :method_return, body: [result]} = Rebus.call(conn, message)

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

`Rebus.call/3` returns `%Rebus.Message{}` for both `:method_return` and
`:error` replies. It returns `{:error, :timeout}` when no reply arrives before
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

## Architecture

Rebus is built with a modular architecture:

- **`Rebus`** - Main API module for establishing connections and managing signal handlers
- **`Rebus.Connection`** - Supervised connection processes that handle D-Bus protocol communication
- **`Rebus.Message`** - Message creation, encoding, decoding, and validation
- **`Rebus.Encoder`** - D-Bus wire format encoding with proper alignment
- **`Rebus.Decoder`** - D-Bus wire format decoding with struct boundary tracking
- **`Rebus.SignalHandler`** - Event-based signal distribution to registered handlers

## Connection Types

Rebus supports connecting to different types of D-Bus endpoints:

- **TCP/IP connections** - `%{family: :inet, addr: {127, 0, 0, 1}, port: 12345}`
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

## Message Types

Rebus supports all D-Bus message types:

- **`:method_call`** - Method invocations  
- **`:method_return`** - Method replies with returned data
- **`:error`** - Error responses
- **`:signal`** - Signal emissions

## D-Bus Compliance

Rebus implements the D-Bus specification including:

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

## Testing

Rebus includes comprehensive testing infrastructure:

- **200+ test cases** covering encoding, decoding, message handling, and edge cases
- **Test server infrastructure** for integration testing
- **Code coverage reporting** with test utilities excluded from metrics
- **Property-based testing** for robust validation

## Installation

Add `rebus` to your list of dependencies in `mix.exs`:

```elixir
def deps do
  [
    {:rebus, "~> 0.1.0"}
  ]
end
```

## Documentation

Documentation can be generated with [ExDoc](https://github.com/elixir-lang/ex_doc):

```bash
mix docs
```

The generated documentation includes comprehensive API references, examples, and implementation details.

## License

This project is licensed under the MIT License.

## Contributing

1. Fork the repository
2. Create your feature branch (`git checkout -b feature/my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin feature/my-new-feature`)
5. Create a new Pull Request

Make sure to run the test suite before submitting:

```bash
mix test
mix test --cover  # With coverage reporting
```
