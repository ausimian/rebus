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

## Authentication

Rebus always tries D-Bus `EXTERNAL` first, preserving the normal local Unix
credential flow. If the peer rejects it with a valid, bounded `REJECTED`
mechanism list, Rebus deterministically prefers `DBUS_COOKIE_SHA1`. It reads
the effective Unix username with `id -un`, then reads only that user's
`$HOME/.dbus-keyrings/<context>` cookie file. A final `$HOME` symlink is
followed only after its resolved directory is validated; the keyring directory
and cookie file themselves must be owned non-symlink entries. The resolved home
cannot be group/other writable, and the keyring directory and cookie file must
be private from group and other users and within local size and 256-line limits.
Contexts, IDs, challenges, and cookie records are validated before use. Cookie
contents, challenges, authorization identities, server GUIDs, and peer
authentication text are never returned in errors or logged.

For interoperability, Rebus accepts upper- or lower-case hexadecimal cookie
input, but always emits the lower-case form required by the D-Bus specification.
Cookie contexts follow the specification and therefore cannot contain `.` (or a
path separator or whitespace). Rebus deliberately limits an advertised
`REJECTED` list to 64 mechanism names and rejects larger lists as malformed;
this avoids retaining an unbounded peer-controlled list.

Cookie authentication is useful for local TCP and peer-to-peer endpoints where
Unix credentials cannot be transported. It authenticates possession of the
private cookie file; it does not add encryption or message integrity, so TCP
connections should normally be loopback or otherwise protected by an external
secure transport. Rebus fails with `:auth_cookie_unavailable` if it cannot read
a safe matching local credential, `:auth_failed` for malformed authentication
protocol data, and `{:auth_rejected, mechanisms}` when no usable advertised
mechanism remains. All of these use the same bounded setup deadline as socket
authentication and initial setup. `:auth_cookie_unavailable` is terminal for a
bus address list, so Rebus does not disclose the missing local credential to
later candidate addresses or IPs.

`ANONYMOUS` is disabled by default. It can be enabled only with
`allow_anonymous: true`, and only after the peer advertised it. Anonymous D-Bus
performs no authentication, confidentiality, or integrity check; use it only
for intentionally unauthenticated peer-to-peer services, never as trust for a
message bus or an unprotected network endpoint. Rebus never downgrades to it
after a DBUS_COOKIE_SHA1 protocol or authentication failure. It is selected
directly only when the peer did not advertise `DBUS_COOKIE_SHA1`, or when the
local username cannot be obtained before cookie `AUTH` starts. Once cookie
`AUTH` has started or a challenge has been received, every cookie failure is
terminal: Rebus sends neither `CANCEL` nor `AUTH ANONYMOUS`.

## Message Types

Rebus supports all D-Bus message types:

- **`:method_call`** - Method invocations  
- **`:method_return`** - Method replies with returned data
- **`:error`** - Error responses
- **`:signal`** - Signal emissions

## Unix file descriptors

On Linux and macOS, a local Unix-domain connection requests the
optional D-Bus `NEGOTIATE_UNIX_FD` authentication extension. When the peer
agrees, a message can borrow raw OS descriptors through `:fds`; `h` values in
the body are zero-based indexes into that list:

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

Rebus does not duplicate or close outbound descriptors: the sender retains
ownership. A successfully delivered method reply can contain received raw
descriptors in `message.unix_fds`; the receiving process owns them and must
close each descriptor exactly once (for example, in an `after` block) with
`Rebus.UnixFD.close/1`, or adopt it with an appropriate OTP/OS API. Do not call
`close/1` twice: descriptor numbers can be reused by the OS.

For a reply that carries descriptors, Rebus retains ownership through a
caller-local, one-shot delivery alias. The initial private delivery and
acknowledgement use a 100 ms grace after the original reply deadline; a claim
is refused and closed after its 250 ms claim deadline when the connection
process dispatches it. If that initial acknowledgement times out after it was
already queued, Rebus waits for a FIFO resolver rather than reporting failure
while the queued acknowledgement could still transfer ownership. Therefore an
FD-bearing `call/3` that has reached handoff can return later than its supplied
timeout and has no separate fixed wall-clock maximum while a live connection is
dispatching the resolver. It returns `{:error, :fd_claim_expired}` only after
the connection has definitively closed the retained descriptors, or
`{:error, :disconnected}` if the connection stops. A late internal
FD-delivery tuple is never sent to the caller's ordinary mailbox.

`Rebus.close/1` performs this retained-descriptor cleanup for ordinary
supervisor shutdown. An untrappable BEAM `:kill` bypasses `terminate/2`, so it
cannot provide the same raw-descriptor cleanup guarantee.

FD transfer is never attempted over TCP and is rejected before a frame is
written when the peer did not agree to the extension. A stale borrowed
descriptor that fails before any frame byte is accepted returns
`{:error, :unix_fd_send_failed}` and leaves the connection usable. Rebus
validates the header count and every `h` index before delivery, bounds a
message to 16 descriptors, and closes descriptors on rejected, orphaned, or
undelivered frames. FD-bearing inbound signals are closed and dropped—not
distributed—because one raw descriptor cannot safely be transferred to
multiple subscribers; a complete dropped frame does not terminate the
connection. On a peer that declined FD negotiation Rebus still uses a bounded
ancillary receive buffer, immediately closes illicit rights, and quarantines
only the associated complete frame; `MSG_CTRUNC` remains a fail-closed
connection error because the kernel may have omitted uncloseable descriptors.
Other Unix and BSD variants are not currently supported for FD passing.

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
