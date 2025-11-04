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
address = %{family: :inet, addr: {{127, 0, 0, 1}, port: 12345}}
{:ok, conn} = Rebus.connect(address)

# Add a signal handler to receive D-Bus signals
ref = Rebus.add_signal_handler(conn)

# Create and send a D-Bus message
{:ok, message} = Rebus.Message.new(:method_call,
  path: "/com/example/Object",
  interface: "com.example.Interface", 
  member: "TestMethod",
  body: [42, "hello"],
  signature: "is"
)

# Clean up when done
Rebus.delete_signal_handler(conn, ref)
```

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

- **TCP/IP connections** - `%{family: :inet, addr: {{ip_tuple}, port}}`
- **Unix domain sockets** - `%{family: :local, path: "/path/to/socket"}`

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

This project is licensed under the BSD License.

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

