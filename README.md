# Rebus

Rebus is a D-Bus client for Elixir. It connects to a message bus or peer,
makes method calls, sends and receives signals, and passes Unix file
descriptors over local sockets.

Rebus supports Elixir 1.18 and later on Linux and macOS. Windows is not
supported. It currently has no service-side API, proxies, or introspection;
applications build and match D-Bus messages directly.

## Installation

Add Rebus to your dependencies:

```elixir
def deps do
  [
    {:rebus, "~> 0.3.0"}
  ]
end
```

## Example

```elixir
{:ok, conn} = Rebus.connect(:session)

message =
  Rebus.Message.new!(:method_call,
    path: "/org/freedesktop/DBus",
    interface: "org.freedesktop.DBus",
    destination: "org.freedesktop.DBus",
    member: "ListNames"
  )

{:ok, %Rebus.Message{type: :method_return, body: [names]}} =
  Rebus.call(conn, message)

:ok = Rebus.close(conn)
```

The [HexDocs API reference](https://hexdocs.pm/rebus) documents connection
options, results, errors, and wire-format modules. The practical guides cover
[authentication](https://hexdocs.pm/rebus/authentication.html),
[signal subscriptions](https://hexdocs.pm/rebus/match_rules.html), and
[Unix file descriptors](https://hexdocs.pm/rebus/unix_fds.html).

See [CONTRIBUTING.md](CONTRIBUTING.md) for development and testing instructions.

## License

Rebus is licensed under the MIT License.
