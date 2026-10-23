# Rebus

Rebus is a D-Bus library for Elixir. It connects to a message bus or peer,
makes method calls, sends and receives signals, exports objects that answer
method calls, and passes Unix file descriptors over local sockets.

Rebus supports Elixir 1.18 and later on Linux and macOS. Windows is not
supported. It has no client-side proxies and does not parse introspection
data, so clients build and match D-Bus messages directly.

## Installation

Add Rebus to your dependencies:

```elixir
def deps do
  [
    {:rebus, "~> 0.3.0"}
  ]
end
```

On Linux and macOS, Rebus compiles a small native library when it is built,
for Unix file descriptor passing, so the build needs `make` and a C++17
compiler such as GCC or Clang.

## Examples

Call a method:

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

Export an object and give it a well-known name:

```elixir
defmodule Calculator do
  use Rebus.Object

  @calculator Rebus.Interface.new!("org.example.Calculator",
                methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}]
              )

  @impl true
  def interfaces, do: [@calculator]

  @impl true
  def init(_arg), do: {:ok, %{}}

  @impl true
  def handle_method(%{member: "Add"}, [a, b], state), do: {:reply, [a + b], state}
end

{:ok, conn} = Rebus.connect(:session)
{:ok, _pid} = Rebus.Object.start_link(Calculator, nil, conn: conn, path: "/org/example/Calculator")
{:ok, :primary_owner} = Rebus.Bus.request_name(conn, "org.example.Calculator")
```

The [HexDocs API reference](https://hexdocs.pm/rebus) documents connection
options, results, errors, and wire-format modules. The practical guides cover
[exporting objects](https://hexdocs.pm/rebus/services.html),
[authentication](https://hexdocs.pm/rebus/authentication.html),
[signal subscriptions](https://hexdocs.pm/rebus/match_rules.html), and
[Unix file descriptors](https://hexdocs.pm/rebus/unix_fds.html).

See [CONTRIBUTING.md](CONTRIBUTING.md) for development and testing instructions.

## License

Rebus is licensed under the MIT License.
