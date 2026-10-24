# Unix file descriptor passing

> #### Experimental support {: .warning}
>
> Unix file descriptor support is experimental and likely to change in a
> subsequent release.

D-Bus can carry Unix file descriptors alongside a message body. Rebus
supports this on Linux and macOS, over local Unix sockets, when the
application has enabled it and the peer agrees to it during authentication.
If the transport cannot carry descriptors or the peer did not agree to
receive them, Rebus rejects the message without sending any bytes.

## Enabling descriptor passing

Descriptor passing is off by default, so a default build of Rebus is pure
Elixir. To enable it, set this in your application's `config/config.exs`:

```elixir
config :rebus, unix_fds: true
```

Rebus then builds a small native library from C++17 source shipped in the
package, which needs `make` and a C++17 compiler, such as GCC or Clang. Most
Linux distributions provide them, as do the Xcode command line tools on
macOS. Nothing is built on other platforms, whatever the setting.

- The setting must be in `config/config.exs` (or a file it imports), not in
  `config/runtime.exs`, because it decides how Rebus is compiled. A release
  whose runtime configuration disagrees with the compiled value refuses to
  boot.
- Changing it makes Mix recompile Rebus on the next `mix compile`, adding or
  dropping the native library; no `--force` or `mix deps.clean` is needed.
- Only the end application can enable it. Mix never loads a dependency's
  configuration, so a library built on Rebus cannot enable descriptor
  passing for its users. A library that needs it can check
  `Rebus.UnixFD.supported?/0` at startup and tell its users to add the
  setting.
- If the setting is on but the native library fails to load, Rebus fails to
  load with it, rather than running without the descriptor passing the
  application asked for.

Without it, a connection never offers descriptor passing to its peer, every
`Rebus.UnixFD` function returns `{:error, :unsupported}`, and a message with
descriptors is refused with `{:error, :unix_fd_unsupported}`. A peer should
then never send any. If one does anyway on Linux, the kernel discards the
descriptors and Rebus drops the message that declared them, as it does for
any peer that sends descriptors it was not offered. On macOS the kernel hands
them to Rebus even so, with nothing in such a build able to close them, so
the connection stops with `:unix_fd_not_negotiated` at the first one, and
`Rebus.connect/2` returns that error if they arrive while it is still
authenticating.

Every descriptor Rebus receives reaches your code as a `Rebus.UnixFD`
handle, never as a raw number. A handle owns one open descriptor and closes
it exactly once, whichever comes first of:

- any holder calling `Rebus.UnixFD.close/1`,
- its owning process exiting, for any reason including `:kill`,
- the last reference to it being garbage collected.

A closed handle forgets its descriptor number, so a stale handle can never
close or use an unrelated descriptor that the operating system has since
given the same number. Inspecting a handle shows its state and owner, never
the number:

```elixir
iex> handle
#Rebus.UnixFD<open, owner: #PID<0.215.0>>
```

## Ownership and aliases

Each handle has exactly one owning process, in the controlling-process model
of ports and sockets. A handle is an ordinary term, so any number of
processes can hold it; those copies are aliases of the same handle, not
duplicates of the descriptor. The rules are:

- Any holder may use a handle, send it, duplicate it with
  `Rebus.UnixFD.dup/1`, or close it. Closing is idempotent: a second
  `Rebus.UnixFD.close/1` returns `{:error, :closed}`.
- Only the owner can transfer ownership, with
  `Rebus.UnixFD.controlling_process/2`. Anyone else gets
  `{:error, :not_owner}`, and a closed handle answers `{:error, :closed}`
  before ownership is even checked.
- When the owner exits, the handle closes at once, and every alias reads
  closed from then on. Transferring a handle to a process that is not alive
  closes it and returns `{:error, :noproc}`.
- `Rebus.UnixFD.dup/1` makes a new handle, owned by the process that calls
  it, for the same open file. The two close independently, so `dup/1` is how
  a process that does not own a handle gets a descriptor of its own.
- `Rebus.UnixFD.owner/1` and `Rebus.UnixFD.open?/1` tell who owns a handle
  and whether it is open.

Where handles come from:

- The handles in a reply to `Rebus.call/3` are owned by the process that
  called it.
- The handles on an inbound method call are owned by the connection and lent
  to the handler for the life of the call. `Rebus.Service.take_fds/2` makes
  the handler their owner instead.
- Signals never hand a descriptor to your code.

## Receiving descriptors in a reply

A `Rebus.call/3` reply carries a handle for each received descriptor in
`message.unix_fds`, and the calling process owns them from the moment the
call returns. A D-Bus error reply carries them the same way, so check
`unix_fds` on both `{:ok, reply}` and `{:error, {:dbus_error, reply}}`:

```elixir
case Rebus.call(conn, message) do
  {:ok, reply} ->
    handle_and_close(reply, &handle_reply/1)

  {:error, {:dbus_error, error}} ->
    handle_and_close(error, &handle_error_reply/1)

  {:error, reason} ->
    handle_call_failure(reason)
end

defp handle_and_close(%Rebus.Message{unix_fds: handles} = message, handler) do
  try do
    handler.(message)
  after
    Enum.each(handles, &Rebus.UnixFD.close/1)
  end
end
```

Closing a handle you no longer need releases the descriptor at once. A
handle you leave open is closed when your process exits or when nothing
refers to it any more, so it never leaks for good, but it holds the
descriptor until then.

`Rebus.UnixFD.close/1` returns once the descriptor itself is closed.
Closing a descriptor can block, for instance on a socket with unsent data
and `SO_LINGER` set, so `Rebus.UnixFD.close/1` runs on a dirty I/O
scheduler, where only the caller waits. When a
handle closes any other way, because its owner exits, because it is garbage
collected, or because the connection closes it, it reads closed at once,
and a background thread closes the descriptor a moment later, so a slow
close never holds up the VM or the process that let the handle go.

## Using a descriptor with `with_fd/2`

`Rebus.UnixFD.with_fd/2` calls a function with the raw descriptor number and
returns `{:ok, result}`, or `{:error, :closed}` without calling it. The
descriptor stays open until the function returns, even if the handle is
closed or its owner exits meanwhile. The number is valid only inside the
function: hand it to something that takes its own copy, and never close it
yourself.

Adopt a received socket into an OTP `:socket`. `:socket.open/2` duplicates
the descriptor by default, so the socket outlives the handle:

```elixir
{:ok, {:ok, sock}} =
  Rebus.UnixFD.with_fd(h, fn fd ->
    :socket.open(fd, %{domain: :local, type: :stream})
  end)

:ok = Rebus.UnixFD.close(h)
```

Read a file that a portal handed you, through an independent OTP file opened
on `/dev/fd`:

```elixir
{:ok, {:ok, contents}} =
  Rebus.UnixFD.with_fd(h, fn fd -> File.read("/dev/fd/#{fd}") end)
```

What opening `/dev/fd/N` gives you depends on the platform. On Linux it
reopens the file the descriptor refers to, so the new file has its own
offset, starting at the beginning, and it does not work for sockets. On
macOS it duplicates the descriptor, so the new file shares the original's
offset.

Many descriptors need no `with_fd/2` at all. A systemd-logind inhibitor lock,
for instance, holds for as long as its descriptor is open, so the handle is
all you need:

```elixir
{:ok, %Rebus.Message{unix_fds: [lock]}} = Rebus.call(conn, inhibit_msg)
# ... work ...
Rebus.UnixFD.close(lock)   # or let the owning process exit
```

Do not take the number out of the function:

```elixir
# Wrong
{:ok, fd} = Rebus.UnixFD.with_fd(h, & &1)
```

Once `with_fd/2` returns, nothing keeps that descriptor open. Any holder can
close the handle, or its owner can exit, and the number may then be closed,
or given to an unrelated file, socket or pipe by the time you use it. To
keep a descriptor of your own, use `Rebus.UnixFD.dup/1`, or have something
like `:socket.open/2` take its own copy inside the function.

## Sending descriptors

Pass descriptors as `fds:` when you build the message: `Rebus.UnixFD`
handles, raw descriptor numbers, or both. Each `h` value in the body is a
zero-based index into that list:

```elixir
message = Rebus.Message.new!(:method_call,
  path: "/com/example/Object",
  interface: "com.example.Interface",
  destination: "com.example.Service",
  member: "TakeFiles",
  signature: "hh",
  body: [0, 1],
  fds: [handle, raw_fd]
)

{:ok, reply} = Rebus.call(conn, message)
```

`Rebus.send/2,3`, `Rebus.Service.emit/6`, the replies of `Rebus.Service`
and a `Rebus.Object`'s `{:reply, body, state, fds: fds}` all take
descriptors the same way.

A handle can be sent by any process that holds it, not only its owner.
Sending it neither closes it nor transfers it. When the connection accepts
the message for writing, it pins each handle, which keeps the descriptor
open until the kernel has taken it, or until the write is abandoned: it
fails, its caller gives up waiting, or the connection stops. So you may
close a handle, or its owner may exit, while it is being sent: the handle
reads closed for everyone at once, and the descriptor itself is closed once
it has been passed on. A handle that is already closed when the connection
accepts the message, or that belongs to another node, fails the message
with `{:error, :unix_fd_closed}`, and nothing is written. Building the
message does not check this, so a message built from an open handle can
still be refused if the handle is closed before you send it.

A raw number is borrowed. Rebus never duplicates or closes it, and keeping
it open until the send returns, and closing it afterwards, is up to you. A
number from inside `with_fd/2` qualifies, as long as the send happens inside
the function:

```elixir
{:ok, :ok} = Rebus.UnixFD.with_fd(handle, &Rebus.send(conn, signal_with_fd(&1)))
```

## Inbound method calls

A method call to an object exported with `Rebus.Service.export/4` arrives
with a handle for each of its descriptors in `call.unix_fds`. The handler
borrows them: it may use them while the call is pending, or close them
early, but the connection owns them and closes each one it still owns when
the call completes. A call completes when it is answered, including an
answer to a call that asked for no reply, when it expires, when its handler
exits, when its object is unexported, or when the connection closes. A
handler that ignores the handles leaks nothing.

To keep the descriptors beyond the call, take ownership before answering:

```elixir
receive do
  {^ref, %Rebus.Service.Call{member: "TakeFile", unix_fds: [_]} = call} ->
    case Rebus.Service.take_fds(call) do
      {:ok, [handle]} ->
        :ok = Rebus.Service.reply(call, [])
        keep_and_later_close(handle)

      {:error, _reason} ->
        Rebus.Service.reply_error(call, "com.example.Error.Failed", "Not received")
    end
end
```

`take_fds/2` makes the calling process the owner of the handles, the same
ones as in `call.unix_fds`, so they outlive the call. On
`{:error, :timeout}` the connection may still make you their owner a moment
later, or may not, and the same holds for `{:error, :disconnected}` from a
connection killed in the middle of the hand-over. Either way each handle
closes exactly once, and `Rebus.UnixFD.owner/1` tells you which. A second
`take_fds/2` for the same call returns `{:error, :unknown_call}`. In every
case the call stays pending and still needs its answer.

When a call never reaches a handler, because nothing is exported at its
path, it names an undeclared method, or too many calls are already pending,
the connection closes its descriptors as it answers it.

### Descriptors in replies

`Rebus.Service.reply/3` and `Rebus.Service.reply_error/4` take the same
`fds:` option as `Rebus.Message.new/2`, and `Rebus.Service.reply_message/3`
sends whatever descriptors its message carries. A reply may carry the call's
own borrowed handles, with no need to take them first. Answering the call
closes them, but the connection pins them before it does, so their
descriptors stay open until the reply has passed them on:

```elixir
receive do
  {^ref, %Rebus.Service.Call{member: "Echo", unix_fds: [handle]} = call} ->
    :ok = Rebus.Service.reply(call, [0], fds: [handle])
end
```

A reply that carries a closed handle returns `{:error, :unix_fd_closed}`,
sends nothing, and leaves the call open for another answer. Once the
connection has accepted a reply, though, the call is complete and its
borrowed handles are closed, even if writing the reply then fails with
`{:error, :unix_fd_send_failed}` or the handler stops waiting with
`{:error, :timeout}`. The call cannot be answered again after either.

## Signals

Rebus closes the descriptors on an inbound signal and drops the signal. A
handle has one owner, so it cannot be handed to several subscribers. The
connection stays up and later signals arrive normally.

## Timeouts and late replies

An FD-bearing `Rebus.call/3` honours its timeout like any other call. A
reply that arrives after the call has returned `{:error, :timeout}` is
dropped, and its descriptors are closed. If it arrives before the
connection learns of the timeout, the connection has already made the
calling process the owner of its handles. The calling process tells the
connection as it gives up, and the connection then closes them. It
remembers such handles for one second past the call's deadline. If that
news takes longer to arrive, on a badly overloaded system, the handles stay
open until the calling process exits or garbage-collects the dropped reply.

## Cleanup

`Rebus.close/1` closes every handle the connection still owns, including
those lent to the handlers of pending calls. So does any other way the
connection stops, even an untrappable `:kill`, because a handle closes when
its owner exits. The pins of writes still in progress go with the
connection too, so a handle closed while it was being sent has its
descriptor closed when the connection stops.

## Distribution and other details

- A handle is local to the node that created it. Sent to another node, it
  answers `{:error, :not_local}` to every operation there, and a connection
  there refuses to send it with `{:error, :unix_fd_closed}`. Once it is back
  on its own node, whether sent back or decoded with `:erlang.binary_to_term/1`,
  it works again.
- Handles are built on a small native library, which cannot be upgraded in
  place. Reloading the module that loads it in a running node, for example
  by recompiling Rebus from `iex` after changing it, fails; restart the node
  instead. This only affects development.

## Errors

These are returned when sending or answering:

| Reason | Meaning |
| --- | --- |
| `:unix_fd_unsupported` | This connection cannot pass descriptors at all. It is a TCP connection, the platform lacks support, or the application has not [enabled descriptor passing](#enabling-descriptor-passing). |
| `:unix_fd_not_negotiated` | The peer did not agree to descriptor passing. |
| `:unix_fd_closed` | A handle in the message was already closed, or belongs to another node, when the connection accepted it. Nothing was written. |
| `:unix_fd_send_failed` | The kernel refused an outbound descriptor. No byte of the frame was written and the connection remains usable. A handle you sent is as it was, but a reply has still completed its call, closing the call's borrowed handles, and the caller gets no answer. |
| `:disconnected` | The connection stopped, closing the handles it owned. |

`Rebus.UnixFD` functions return these:

| Reason | Meaning |
| --- | --- |
| `:closed` | The handle is closed. |
| `:not_owner` | Only the owner can transfer the handle. |
| `:noproc` | The process it was transferred to is not alive, so the descriptor was closed. |
| `:not_local` | The handle, or the process given, belongs to another node. |
| `:unsupported` | This build has no handle support: the platform lacks it, or the application has not [enabled descriptor passing](#enabling-descriptor-passing). |
