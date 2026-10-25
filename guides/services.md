# Exporting objects

A D-Bus service exports objects at object paths. Other connections call the
methods, read and write the properties, and receive the signals of the
interfaces those objects implement. This guide covers the service side of
Rebus: declaring interfaces, exporting objects, and answering the calls made
on them.

Rebus has two layers for this. `Rebus.Service` is the protocol: it exports
interfaces at a path and delivers each call to a handler process as a
message. `Rebus.Object` is a behaviour on top of it that runs an object as a
GenServer with one callback per kind of call. Most services want
`Rebus.Object`. The raw protocol is for handlers that already have a process
loop of their own, or that need something the behaviour does not cover.

Whichever layer you use, the connection checks every call against your
interface declarations before your code sees it. A handler only ever
receives declared methods with arguments of the declared types, and the
connection answers `org.freedesktop.DBus.Introspectable` and
`org.freedesktop.DBus.Peer` itself.

## A minimal object

An interface declaration names the interface and lists its methods, signals
and properties with their D-Bus signatures. The connection uses the
declaration to check inbound calls, to encode replies, and to generate
introspection data, so each interface is declared once:

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

Each call reaches `handle_method/3` as a `Rebus.Service.Call` struct plus the
call's arguments, already decoded with the declared types. The return value
answers the call. `{:reply, body, state}` sends a method return whose body is
encoded with the declared out-signature, and `{:error, name, message, state}`
sends a D-Bus error.

From another terminal, `busctl` can now call it:

```console
$ busctl --user call org.example.Calculator /org/example/Calculator org.example.Calculator Add ii 2 3
i 5
```

### Supervision

`use Rebus.Object` defines `child_spec/1`, which takes the same `{arg, opts}`
as `start_link/3`, so objects go in a supervision tree like any GenServer.
Start them before requesting the bus name, so the name is only granted once
they are exported:

```elixir
{:ok, conn} = Rebus.connect(:session)

children = [{Calculator, {nil, conn: conn, path: "/org/example/Calculator"}}]
{:ok, sup} = Supervisor.start_link(children, strategy: :one_for_one)

{:ok, :primary_owner} = Rebus.Bus.request_name(conn, "org.example.Calculator")
```

The child spec is `:transient`, which suits an exported object:

- An object that crashes is restarted, and the new process exports itself
  again.
- An object whose connection stops exits with `{:shutdown, :disconnected}`.
  Supervisors treat that as a normal exit, so it is not restarted. A restart
  could not export on a stopped connection.

`:temporary` is the only other sensible value, for objects that should never
be restarted, such as ones started on demand under a `DynamicSupervisor`.
Avoid `:permanent`: after a disconnect it restarts the object onto the
stopped connection, every restart fails, and the supervisor exits once it
reaches its restart limit, taking its other children with it. Pass
`restart:` or any other child spec key to `use Rebus.Object` to choose, as
with `use GenServer`. `Rebus.Object` documents the rest of the lifecycle.

## The raw protocol

`Rebus.Service.export/4` exports interfaces at a path and sends each call to
a handler process as a `{ref, %Rebus.Service.Call{}}` message, where `ref` is
the export's reference:

```elixir
alias Rebus.Service.Call

{:ok, ref} = Rebus.Service.export(conn, "/org/example/Calculator", [calculator])

receive do
  {^ref, %Call{member: "Add", body: [a, b]} = call} ->
    :ok = Rebus.Service.reply(call, [a + b])
end
```

The handler is the calling process unless `export/4` is given a `:handler`.
It answers each call with `Rebus.Service.reply/3`,
`Rebus.Service.reply_error/4` or, for a reply built by hand,
`Rebus.Service.reply_message/3`. Calls to one handler arrive in the order the
connection received them.

The handler is monitored, and everything it exported is unexported when it
exits. `Rebus.Service.unexport/3` removes an export explicitly.

An export with `subtree: true` also serves every path below its own, and
`call.path` is the path actually called. This suits objects that come and go
too quickly to export one by one, such as one per database row. An exact
export takes precedence over a subtree that covers the same path.

A GenServer that exports itself this way can still use the `Rebus.Object`
callbacks by passing each call to `Rebus.Object.dispatch/4` from its own
`handle_info/2`.

## Answering calls

Every call must be answered exactly once. Rebus guarantees the "at most
once" half: the caller never receives two replies, and a second answer to the
same call returns `{:error, :unknown_call}`. Making sure there is a first
answer is the handler's job, and `Rebus.Object` does it whenever a callback
returns a reply.

What happens when a call is not answered cleanly:

- **It times out.** A call not answered within the export's `:call_timeout`,
  60 seconds by default, is answered by the connection with
  `org.freedesktop.DBus.Error.NoReply`. `call.deadline` is the monotonic time
  at which that happens. Pass `call_timeout: :infinity` to
  `Rebus.Service.export/4` or `Rebus.Object.start_link/3` to wait forever.
- **The handler dies.** When a handler exits, or its object is unexported,
  every call still waiting on it is answered with
  `org.freedesktop.DBus.Error.Failed`. Exceptions in a `Rebus.Object`
  callback are not rescued: the object crashes, the caller receives `Failed`,
  and the exception never reaches the wire.
- **The reply does not fit.** A body that does not match the declared
  out-signature returns `{:error, :signature_mismatch}` and leaves the call
  open. `Rebus.Object` treats this as a bug and stops the object.

A caller can set the `NO_REPLY_EXPECTED` flag on any call, and the handler
sees `call.no_reply?` set to `true`. Answer it anyway. The answer sends
nothing and returns `:ok`, so a handler need not tell such calls apart, and
answering is what releases any descriptors the call carried. Declaring a
method with `no_reply: true` only adds the introspection annotation telling
callers they need not wait. It does not change what the handler receives.

At most `:max_pending_calls` calls, 1024 by default, may wait for answers on
one connection at a time. The connection answers the rest with
`org.freedesktop.DBus.Error.LimitsExceeded`. The limit is an option of
`Rebus.connect/2`. A call that asked for no reply and carries no descriptors
is not tracked at all: it counts towards neither this limit nor the
`:call_timeout`, and a repeated answer to it goes unnoticed, since answering
it sends nothing in any case.

`Rebus.Service` lists every error a caller can see, including the ones the
connection sends for calls that never reach the handler.

### Replying later

A handler does not have to answer a call before taking the next one. The
`Rebus.Service.Call` struct is all that is needed to answer, and any process
holding it can do so. A `Rebus.Object` callback returns `{:noreply, state}`
to leave the call open:

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
`Rebus.Service.reply/3` returns `{:error, :unknown_call}`. If the task
crashes without answering, the call stays open until it expires and the
caller gets `NoReply`, so have the task answer with an error while it still
can.

## Properties

A property is declared with a name, a signature and an access of `:read`,
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
interface. Values are plain terms typed by the declaration, so the callback
returns `3` rather than a variant.

### PropertiesChanged

After a successful `Set`, a `Rebus.Object` emits
`org.freedesktop.DBus.Properties.PropertiesChanged` according to the
property's `emits_changed`: with the new value when it is `true`, the
default; with only the property's name when it is `:invalidates`; and not at
all when it is `false` or `:const`. Return `{:ok, state, emit: false}` from
`handle_set_property/4` to suppress the signal, for example when the object
stores a value other than the one it was given.

A property that changes for any other reason is announced with
`Rebus.Service.properties_changed/6`. It types each value by its declaration
and refuses a property whose `emits_changed` does not allow it:

```elixir
:ok =
  Rebus.Service.properties_changed(conn, "/org/example/Calculator", @settings,
    %{"Precision" => 3}, [])
```

### Properties without the behaviour

A raw `Rebus.Service` handler receives property calls as ordinary calls, with
`interface: "org.freedesktop.DBus.Properties"`, the member `"Get"`, `"Set"`
or `"GetAll"`, and the body as the caller sent it. Answer a `Get` with a
`{signature, value}` variant, and a `GetAll` with the dictionary that
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

## Signals

Declare a signal with its arguments and emit it from an exported object with
`Rebus.Service.emit/6`, which encodes the body with the declared signature:

```elixir
@events Rebus.Interface.new!("org.example.Events",
          signals: [{"Computed", args: [sum: "i"]}]
        )

:ok = Rebus.Service.emit(conn, "/org/example/Calculator", @events, "Computed", [5])
```

Given an interface name rather than a declaration, `emit/6` takes the
signature as its `:signature` option:

```elixir
:ok =
  Rebus.Service.emit(conn, "/org/example/Calculator", "org.example.Events", "Computed", [5],
    signature: "i"
  )
```

A signal is broadcast unless it has a `:destination`. The bus delivers a
broadcast only to connections whose match rules select it, so clients
subscribe as described in
[Signal subscriptions and match rules](match_rules.html).

## Unix file descriptors

Descriptors sent with a call arrive in the call's `:unix_fds` as
`Rebus.UnixFD` handles, one for each `h` argument. The connection owns these
handles and lends them to the handler for the life of the call. While the
call is pending, the handler may reach a descriptor with
`Rebus.UnixFD.with_fd/2` or send the handle on, but only the connection, as
owner, can close it. When the call completes, however it completes, the
connection closes every handle it still owns, so a handler that does nothing
with them leaks nothing.

To keep a descriptor after the call, become its owner with
`Rebus.Service.take_fds/2` before answering. The handles it returns are the
same ones as in the call's `:unix_fds`, now owned by the handler, so they
outlive the call:

```elixir
receive do
  {^ref, %Call{member: "TakeLog", unix_fds: [_]} = call} ->
    case Rebus.Service.take_fds(call) do
      {:ok, [handle]} ->
        :ok = Rebus.Service.reply(call, [])
        keep_and_later_close(handle)

      {:error, _reason} ->
        Rebus.Service.reply_error(call, "org.example.Error.Failed", "Not received")
    end
end
```

Whatever `take_fds/2` returns, the call stays pending and still needs its
answer. `{:error, :unknown_call}` means the handles were already taken or
the call has completed. After `{:error, :timeout}` or
`{:error, :disconnected}` the hand-over may or may not have gone through;
`Rebus.UnixFD.owner/1` tells you which. In every case each handle closes
exactly once.

To send descriptors with a reply, pass them as the `:fds` option of
`Rebus.Service.reply/3`, or return `{:reply, body, state, fds: fds}` from a
`Rebus.Object` callback. A reply may carry the call's own borrowed handles
without taking them first. Answering the call closes them, but their
descriptors stay open until the reply has been written.

[Unix file descriptor passing](unix_fds.html) covers the ownership rules in
full.

## Introspection

The connection answers `org.freedesktop.DBus.Introspectable.Introspect` for
every exported object, with XML generated from its declarations. It also
answers for every path above an exported object with that path's child
nodes, so tools can walk a Rebus service without any code in it:

```console
$ busctl --user tree org.example.Calculator
└─ /org
  └─ /org/example
    └─ /org/example/Calculator

$ busctl --user introspect org.example.Calculator /org/example/Calculator
```

Annotations on the declaration appear in the XML. `no_reply: true` on a
method and `:emits_changed` on a property add the standard annotations. A
path served by a subtree export lists the subtree's interfaces and no child
nodes, since the connection cannot know which paths below it exist.

## Requesting a bus name

Clients find a service by its well-known name, which
`Rebus.Bus.request_name/4` asks the bus for. Export the service's objects
first. The bus announces the new owner as soon as it grants the name, and a
client waiting for that announcement, or one whose call activated the
service, may call at once. A call to a path with nothing exported yet is
answered with `org.freedesktop.DBus.Error.UnknownObject`.

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
or loses it. A name can be lost later, for example to a connection that
requests it with `:replace_existing` when this one allowed replacement.
`Rebus.Bus` lists the flags and every possible answer.

## Untrusted callers

On a message bus, the bus authenticates every connection and sets each
call's sender, so `call.sender` is the caller's unique name and cannot be
forged. Bus policy may also restrict who can call you.

On a peer-to-peer connection, made with `bus: false`, there is no bus in
between. Nothing has filtered the calls, and the peer can send whatever it
likes, including any `sender` header it chooses. `call.sender` is then
unauthenticated, usually `nil` but possibly a forged name, so never use it to
decide what a caller may do. Treat such a peer as untrusted by default.

Rebus bounds what a caller can do to you:

- Every call is checked against the declarations before it reaches the
  handler, so arguments always have the declared types.
- `:max_message_size` on `Rebus.connect/2` caps each inbound message. Its
  default is the protocol limit of 128 MiB, so lower it for peer-to-peer and
  TCP connections.
- `:max_pending_calls` caps the calls waiting for answers, and an export's
  `:call_timeout` caps how long each one waits. Together they bound the calls
  and descriptors a slow handler can be made to hold. Calls that ask for no
  reply and carry no descriptors are handed straight to the handler and are
  not covered.
- Errors the connection sends use fixed messages that never echo the call,
  and an exception in a `Rebus.Object` callback never reaches the wire.

The rest is up to the application:

- Declared types say nothing about values. Check ranges, lengths and paths
  before acting on them, especially before passing them to the file system
  or another program.
- `Rebus.Service.reply_error/4` sends the message you give it. Keep
  `inspect/1` output, exception messages and internal state out of it.
- A peer can call as fast as it can write, and calls that ask for no reply
  are not bounded by `:max_pending_calls`, so they can fill a slow handler's
  mailbox. Rate-limit expensive methods yourself.
- Take descriptors with `Rebus.Service.take_fds/2` only when you need to keep
  them, and close each one exactly once.
