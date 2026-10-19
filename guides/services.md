# Exporting objects

A D-Bus service exports objects at object paths, and other connections call
the methods, read and write the properties, and receive the signals of the
interfaces those objects implement. This guide covers the service side of
Rebus: declaring interfaces, exporting objects, and answering the calls made
on them.

## At a glance

- Declare each interface once with `Rebus.Interface.new!/2`. The declaration
  types every call, reply, property and signal, and generates introspection
  data.
- `use Rebus.Object` runs an exported object as a GenServer with a callback
  for each call. `Rebus.Service` is the protocol underneath it, for handlers
  that want to receive calls as messages.
- The connection checks every call against the declarations before any of
  your code sees it, so a handler only receives declared methods with
  arguments of the declared types.
- Answer every call exactly once. Calls that are never answered expire after
  the export's `:call_timeout`, 60 seconds by default.
- Export a service's objects before requesting its bus name.
- The connection answers `org.freedesktop.DBus.Introspectable` and
  `org.freedesktop.DBus.Peer` itself.
- Treat callers on a peer-to-peer connection as untrusted.

## A minimal object

An interface declaration names the interface and lists its methods, signals
and properties, with their D-Bus signatures:

```elixir
defmodule Calculator do
  use Rebus.Object

  @calculator Rebus.Interface.new!("org.example.Calculator",
                methods: [
                  {"Add", in: [a: "i", b: "i"], out: [sum: "i"]},
                  {"Divide", in: [a: "i", b: "i"], out: [quotient: "i"]}
                ]
              )

  @impl true
  def interfaces, do: [@calculator]

  @impl true
  def init(_arg), do: {:ok, %{}}

  @impl true
  def handle_method(%{member: "Add"}, [a, b], state), do: {:reply, [a + b], state}

  def handle_method(%{member: "Divide"}, [_a, 0], state),
    do: {:error, "org.example.Calculator.Error.DivideByZero", "Division by zero", state}

  def handle_method(%{member: "Divide"}, [a, b], state), do: {:reply, [div(a, b)], state}
end
```

`Rebus.Object.start_link/3` starts the object, exports it on a connection,
and then runs `init/1`:

```elixir
{:ok, conn} = Rebus.connect(:session)
{:ok, _pid} = Rebus.Object.start_link(Calculator, nil, conn: conn, path: "/org/example/Calculator")
{:ok, :primary_owner} = Rebus.Bus.request_name(conn, "org.example.Calculator")
```

Each call reaches `handle_method/3` as a `Rebus.Service.Call` struct and the
call's arguments, already decoded with the declared types. The callback's
return value answers the call: `{:reply, body, state}` with a method return
whose body is encoded with the declared out-signature, or
`{:error, name, message, state}` with a D-Bus error.

From another terminal, `busctl` can now call it:

```console
$ busctl --user call org.example.Calculator /org/example/Calculator org.example.Calculator Add ii 2 3
i 5
```

`use Rebus.Object` defines `child_spec/1`, which takes the `{arg, opts}` that
`start_link/3` takes, so objects belong in a supervision tree. Start them
before requesting the name, so the name is only granted once they are
exported:

```elixir
{:ok, conn} = Rebus.connect(:session)

children = [{Calculator, {nil, conn: conn, path: "/org/example/Calculator"}}]
{:ok, sup} = Supervisor.start_link(children, strategy: :one_for_one)

{:ok, :primary_owner} = Rebus.Bus.request_name(conn, "org.example.Calculator")
```

An object that crashes is restarted and exports itself again. One that
stops because its connection stopped is not restarted, since its export
went with the connection. See `Rebus.Object` for the rest of its lifecycle.

## The raw protocol

`Rebus.Object` is built on `Rebus.Service`, which exports interfaces at a path
and sends each call to a handler process as a message:

```elixir
alias Rebus.Service.Call

{:ok, ref} = Rebus.Service.export(conn, "/org/example/Calculator", [calculator])

receive do
  {^ref, %Call{member: "Add", body: [a, b]} = call} ->
    :ok = Rebus.Service.reply(call, [a + b])
end
```

The handler is the calling process unless `export/4` is given `:handler`. It
receives `{ref, %Rebus.Service.Call{}}` for every call, where `ref` is the
export's reference, and answers each one with `Rebus.Service.reply/3`,
`Rebus.Service.reply_error/4` or, for a reply built by hand,
`Rebus.Service.reply_message/3`. Calls to one handler arrive in the order the
connection received them.

The handler is monitored. When it exits, everything it exported is
unexported. `Rebus.Service.unexport/3` removes an export explicitly.

An export with `subtree: true` also serves every path below its own, and
`call.path` is the path actually called. That suits objects that come and go
too quickly to export one by one, such as one per database row. An exact
export takes precedence over a subtree that covers the same path.

A GenServer that exports itself this way can still use the `Rebus.Object`
callbacks for its calls by passing them to `Rebus.Object.dispatch/4` from its
own `handle_info/2`.

## Answering every call exactly once

Each call waits in the connection until it is answered, and the caller waits
for that answer. Rebus enforces the "exactly once" from its side: the caller
never receives two replies, and a second answer returns
`{:error, :unknown_call}`. The exception is a call that asked for no reply and
carried no descriptors, which the connection does not track, so a repeated
answer to it is not detected: it sends nothing either way. Your side is to make sure there is a
first one.

- **Wrong reply shape.** A body that does not fit the declared out-signature
  returns `{:error, :signature_mismatch}` and leaves the call open for another
  answer. `Rebus.Object` treats that as a bug and stops the object.
- **No reply expected.** A caller can set the `NO_REPLY_EXPECTED` flag on any
  call, and the handler sees `call.no_reply?` set to `true`. Answer it anyway:
  the answer sends nothing and returns `:ok`, so a handler need not tell such
  calls apart, and it is what releases any descriptors the call carried.
  Declaring a method with `no_reply: true` only advertises in introspection
  that callers need not wait; it does not change what the handler receives.
- **Expiry.** A call not answered within the export's `:call_timeout` is
  answered by the connection with `org.freedesktop.DBus.Error.NoReply`, and a
  later answer returns `{:error, :unknown_call}`. `call.deadline` is the
  monotonic time at which that happens. Pass `call_timeout: :infinity` to
  `Rebus.Service.export/4` or `Rebus.Object.start_link/3` to wait forever.
- **Handler death.** When a handler exits, or its object is unexported, every
  call still waiting for it is answered with
  `org.freedesktop.DBus.Error.Failed`. Exceptions inside a `Rebus.Object`
  callback are not rescued: the object crashes, the caller receives `Failed`,
  and the exception never reaches the wire.
- **Limits.** At most `:max_pending_calls` calls, 1024 by default, may wait
  for their answers on one connection at a time. Rebus answers calls beyond
  that with `org.freedesktop.DBus.Error.LimitsExceeded`. Set the limit with
  `Rebus.connect/2`. A call that asked for no reply and carried no
  descriptors is not tracked, so it counts towards neither this limit nor the
  `:call_timeout`.

The connection also refuses calls that do not match the declarations, such as
an undeclared method or arguments of the wrong type, without involving the
handler. `Rebus.Service` lists every error a caller can see.

## Replying asynchronously

A handler does not have to answer a call before it takes the next one. The
`Rebus.Service.Call` struct is all that is needed to answer it, and any
process holding it can do so. A `Rebus.Object` callback returns
`{:noreply, state}` to leave the call open:

```elixir
@impl true
def handle_method(%{member: "Factorise"} = call, [n], state) do
  Task.Supervisor.start_child(MyApp.TaskSupervisor, fn ->
    Rebus.Service.reply(call, [factorise(n)])
  end)

  {:noreply, state}
end
```

The call stays bound to its export. If the object exits before the task
answers, the caller has already received `Failed`, and the task's
`Rebus.Service.reply/3` returns `{:error, :unknown_call}`. If the task crashes
without answering, the call stays open until it expires and the caller gets
`NoReply`, so answer with an error from the task when it can still do so.

## Properties

Properties are declared with a name, a signature and an access of `:read`,
`:write` or `:readwrite`:

```elixir
@settings Rebus.Interface.new!("org.example.Settings",
            properties: [
              {"Precision", "u", :readwrite},
              {"Version", "s", :read, emits_changed: :const}
            ]
          )
```

The connection answers `org.freedesktop.DBus.Properties` calls for exported
objects. It checks each `Get`, `Set` and `GetAll` against the declaration of
the interface it names, refusing an unknown interface or property, a `Set` of
a read-only property, a `Get` of a write-only one, and a `Set` value of the
wrong type. A `Rebus.Object` handles the rest with two callbacks:

```elixir
@impl true
def handle_get_property("org.example.Settings", "Precision", state),
  do: {:ok, state.precision, state}

def handle_get_property("org.example.Settings", "Version", state), do: {:ok, "1.0", state}

@impl true
def handle_set_property("org.example.Settings", "Precision", precision, state)
    when precision <= 16,
    do: {:ok, %{state | precision: precision}}

def handle_set_property("org.example.Settings", "Precision", _precision, state),
  do: {:error, "org.freedesktop.DBus.Error.InvalidArgs", "Precision above 16", state}
```

`GetAll` calls `handle_get_property/3` for every readable property of the
interface. Values are plain terms, typed by the declaration, so the callback
returns `3` rather than a variant.

### PropertiesChanged

After a successful `Set`, a `Rebus.Object` emits
`org.freedesktop.DBus.Properties.PropertiesChanged` as the property's
`emits_changed` declares: with the new value when it is `true`, the default;
with only the property's name when it is `:invalidates`; and not at all when
it is `false` or `:const`. Return `{:ok, state, emit: false}` from
`handle_set_property/4` to suppress it, for example when the object stores a
value other than the one it was given.

A property that changes for any other reason is announced with
`Rebus.Service.properties_changed/6`, which types each value by its
declaration and refuses a property whose `emits_changed` does not allow it:

```elixir
:ok =
  Rebus.Service.properties_changed(conn, "/org/example/Calculator", @settings,
    %{"Precision" => 3}, [])
```

### Properties without the behaviour

A raw `Rebus.Service` handler receives property calls as ordinary calls, with
`interface: "org.freedesktop.DBus.Properties"`, the member `"Get"`, `"Set"` or
`"GetAll"`, and the body as the caller sent it. A `Get` is answered with a
`{signature, value}` variant, and a `GetAll` with a dictionary that
`Rebus.Interface.wrap_properties!/2` builds from the declaration:

```elixir
receive do
  {^ref, %Call{member: "Get", body: [_interface, "Precision"]} = call} ->
    Rebus.Service.reply(call, [{"u", precision}])

  {^ref, %Call{member: "Set", body: [_interface, "Precision", {"u", _value}]} = call} ->
    Rebus.Service.reply(call, [])

  {^ref, %Call{member: "GetAll"} = call} ->
    Rebus.Service.reply(call, [Rebus.Interface.wrap_properties!(settings, %{"Precision" => precision})])
end
```

A raw handler emits `PropertiesChanged` itself.

## Emitting signals

Declare a signal with its arguments, and emit it from an exported object with
`Rebus.Service.emit/6`, which encodes the body with the declared signature:

```elixir
@events Rebus.Interface.new!("org.example.Events",
          signals: [{"Computed", args: [sum: "i"]}]
        )

:ok = Rebus.Service.emit(conn, "/org/example/Calculator", @events, "Computed", [5])
```

Given an interface name rather than a declaration, `emit/6` takes the
signature as the `:signature` option instead:

```elixir
:ok =
  Rebus.Service.emit(conn, "/org/example/Calculator", "org.example.Events", "Computed", [5],
    signature: "i"
  )
```

A signal is broadcast unless it has a `:destination`. The bus delivers a
broadcast only to connections whose match rules select it, so a client
subscribes as the [Signal subscriptions and match rules](match_rules.html)
guide describes.

## Unix file descriptors

Descriptors sent with a call arrive in `call.unix_fds`, indexed by the call's
`h` arguments. The handler borrows them until the call completes, and the
connection closes them then, however the call completes. A handler that
ignores them leaks nothing, and must never close one itself.

To keep a descriptor beyond the call, take ownership of it with
`Rebus.Service.take_fds/2` before answering:

```elixir
receive do
  {^ref, %Call{member: "TakeLog", unix_fds: [_]} = call} ->
    case Rebus.Service.take_fds(call) do
      {:ok, [fd]} ->
        :ok = Rebus.Service.reply(call, [])
        keep_and_later_close(fd)

      {:error, _reason} ->
        Rebus.Service.reply_error(call, "org.example.Error.Failed", "Not received")
    end
end
```

A failed `take_fds/2` transfers nothing. `{:error, :unknown_call}` means the
descriptors were already taken, and stay with whoever took them, or the call
has completed and the connection closed them. Any other error means the
connection has closed them. Either way there is nothing new to close, and a
pending call still needs its answer.

`Rebus.Service.reply/3` sends descriptors with a reply through its `:fds`
option. Rebus borrows those, as it does every outbound descriptor, so the
handler still owns them. A reply cannot send one of the call's own borrowed
descriptors back; take it first. `Rebus.Object` callbacks pass `:fds` by
returning `{:reply, body, state, fds: fds}`.

The [Unix file descriptor passing](unix_fds.html) guide covers the ownership
rules in full.

## Introspection

The connection answers `org.freedesktop.DBus.Introspectable.Introspect` for
the objects it exports, with XML generated from their declarations, and
answers it for every path above an exported object with that path's child
nodes. Tools can therefore walk and describe a Rebus service without any
code in it:

```console
$ busctl --user tree org.example.Calculator
└─ /org
  └─ /org/example
    └─ /org/example/Calculator

$ busctl --user introspect org.example.Calculator /org/example/Calculator
```

Annotations on the declaration appear in the XML. `no_reply: true` on a
method and `:emits_changed` on a property add the standard annotations for
them. A path served by a subtree export lists the subtree's interfaces and no
child nodes, since the connection cannot know which paths below it exist.

## Exporting before requesting a name

Clients find a service by its well-known name, which
`Rebus.Bus.request_name/4` asks the bus for. Export the service's objects
first. As soon as the bus grants the name it announces the new owner, and a
client waiting for that, or one that caused the bus to activate the service,
may call at once. A call to a path with nothing exported yet is answered with
`org.freedesktop.DBus.Error.UnknownObject`.

```elixir
{:ok, _pid} = Rebus.Object.start_link(Calculator, nil, conn: conn, path: "/org/example/Calculator")

case Rebus.Bus.request_name(conn, "org.example.Calculator", [:do_not_queue]) do
  {:ok, owned} when owned in [:primary_owner, :already_owner] -> :ok
  {:ok, :exists} -> {:error, :already_running}
  {:error, reason} -> {:error, reason}
end
```

The process that requested the name receives
`{:rebus_name, conn, :acquired | :lost, name}` whenever the connection gains
or loses it. A name can be lost later, for example to a connection that asks
with `:replace_existing` when this one allowed replacement. See `Rebus.Bus`
for the flags and every answer.

## Untrusted callers

On a message bus, the bus authenticates every connection and sets each
call's sender, so `call.sender` is the caller's unique name and cannot be
forged. Bus policy may also restrict who can call you. On a peer-to-peer
connection, made with `bus: false`, there is no bus in between: nothing has
filtered the calls, and the peer can send whatever it likes, including any
`sender` header it chooses. `call.sender` is then unauthenticated, usually
`nil` but possibly a forged name, so never use it to decide what a caller may
do. Treat such a peer as untrusted by default.

Rebus bounds what any caller can do:

- Every call is checked against the declarations before it reaches the
  handler, so arguments always have the declared types.
- `:max_message_size` on `Rebus.connect/2` caps the size of each inbound
  message. Its default is the protocol limit of 128 MiB, so lower it for
  peer-to-peer and TCP connections.
- `:max_pending_calls` caps the calls waiting for answers, and an export's
  `:call_timeout` caps how long each one waits. Together they bound the
  tracked calls and descriptors a slow handler can be made to hold. They do
  not cover calls that ask for no reply and carry no descriptors, which the
  connection hands straight to the handler without tracking them.
- Errors the connection sends use fixed messages that never echo the call,
  and an exception in a `Rebus.Object` callback never reaches the wire.

The rest is the application's:

- Declared types say nothing about values. Check ranges, lengths and paths
  before acting on them, especially before passing them to the file system
  or another program.
- `Rebus.Service.reply_error/4` sends the message you give it. Do not put
  `inspect/1` output, exception messages or internal state in it.
- A peer can call as fast as it can write, and calls that ask for no reply
  are not bounded by `:max_pending_calls`, so they can fill a slow handler's
  mailbox. Rate-limit expensive methods yourself.
- Take descriptors with `Rebus.Service.take_fds/2` only when you need to keep
  them, and close each one exactly once.
