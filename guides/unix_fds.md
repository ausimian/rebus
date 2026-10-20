# Unix file descriptor passing

D-Bus can carry raw Unix file descriptors alongside a message body. Rebus
supports this on Linux and macOS, over local Unix sockets, when the peer
agrees to it during authentication. An FD-bearing message is rejected before
any byte is written if the transport cannot carry descriptors or the peer
declined.

Who owns a descriptor, and therefore who closes it, is the whole subject of
this guide. The rules in short:

- Rebus borrows the descriptors you send. You keep ownership and close them
  yourself.
- Descriptors in a reply to `Rebus.call/3` become yours. Close each one
  exactly once.
- Descriptors on an inbound method call are lent to the handler for the life
  of the call. Take them with `Rebus.Service.take_fds/2` to keep them.
- Signals never hand a descriptor to your code.

## Sending descriptors

Pass the raw descriptors as `fds:` when you build the message. Each `h` value
in the body is a zero-based index into that list:

```elixir
message = Rebus.Message.new!(:method_call,
  path: "/com/example/Object",
  interface: "com.example.Interface",
  destination: "com.example.Service",
  member: "TakeFile",
  signature: "h",
  body: [0],
  fds: [fd]
)

{:ok, reply} = Rebus.call(conn, message)
```

Rebus never duplicates or closes these descriptors. The sender stays
responsible for closing them once the call returns.

## Receiving descriptors in a reply

A `Rebus.call/3` reply exposes received descriptors in `message.unix_fds`,
and the calling process owns them from that moment. A D-Bus error reply
carries them the same way, so check `unix_fds` on both `{:ok, reply}` and
`{:error, {:dbus_error, reply}}`:

```elixir
case Rebus.call(conn, message) do
  {:ok, reply} ->
    handle_and_close(reply, &handle_reply/1)

  {:error, {:dbus_error, error}} ->
    handle_and_close(error, &handle_error_reply/1)

  {:error, reason} ->
    handle_call_failure(reason)
end

defp handle_and_close(%Rebus.Message{unix_fds: fds} = message, handler) do
  try do
    handler.(message)
  after
    Enum.each(fds, &Rebus.UnixFD.close/1)
  end
end
```

Close each descriptor exactly once, with `Rebus.UnixFD.close/1` or by
adopting it with an OS or OTP API of your own. Never close one twice.
Operating systems reuse descriptor numbers, so a second close can close an
unrelated resource.

`Rebus.UnixFD.close/1` returns `:ok` on success and `{:error, reason}` when
the descriptor could not be adopted or the operating system refused the
close. A failed close still spends the descriptor, and the number is never
safe to reuse, so log the reason if it matters to you but never retry.

An FD-bearing `Rebus.call/3` can return later than the timeout you gave it.
Rebus waits until ownership is settled, so that every received descriptor
has exactly one owner. If the call returns `{:error, :fd_claim_expired}`,
Rebus closed the reply's descriptors instead of handing them over, and there
is nothing left for you to close.

## Signals

Rebus closes the descriptors on an inbound signal and drops the signal. One
raw descriptor cannot be shared safely between several subscribers. The
connection stays up and later signals arrive normally.

## Inbound method calls

A method call to an object exported with `Rebus.Service.export/4` arrives
with its descriptors in `call.unix_fds`. The handler borrows them: it may
read them, or pass them on, while the call is pending, but the connection
still owns them and closes each one exactly once when the call completes.
A call completes when it is answered, including an answer to a call that
asked for no reply, when it expires, when its handler exits, when its object
is unexported, or when the connection closes. A handler that ignores the
descriptors leaks nothing, and must never close one it has not taken.

To keep the descriptors beyond the call, take ownership before answering:

```elixir
receive do
  {^ref, %Rebus.Service.Call{member: "TakeFile", unix_fds: [_]} = call} ->
    case Rebus.Service.take_fds(call) do
      {:ok, [fd]} ->
        :ok = Rebus.Service.reply(call, [])
        keep_and_later_close(fd)

      {:error, _reason} ->
        Rebus.Service.reply_error(call, "com.example.Error.Failed", "Not received")
    end
end
```

`take_fds/2` uses the same handover as an FD-bearing reply to `Rebus.call/3`,
so ownership moves only once the handover is settled, and like `call/3` it
can return later than its timeout. On `{:error, reason}` the connection has
already closed the descriptors and there is nothing left for you to close. A
second `take_fds/2` for the same call returns `{:error, :unknown_call}`.
Either way the call stays pending and still needs its answer.

When a call never reaches a handler, because nothing is exported at its
path, it names an undeclared method, or too many calls are already pending,
the connection closes its descriptors as it answers it.

### Descriptors in replies

`Rebus.Service.reply/3` and `Rebus.Service.reply_error/4` take the same
`fds:` option as `Rebus.Message.new/2`, and `Rebus.Service.reply_message/3`
sends whatever descriptors its message carries. Rebus borrows them, as it
does for every outbound message, so the handler still owns them and closes
them once the answer returns.

A reply cannot carry a descriptor the connection still owns. That includes
the call's own borrowed descriptors, which answering the call closes before
the reply is written, and descriptors borrowed by another pending call. Such
a reply returns `{:error, :unix_fd_borrowed}` and leaves the call open. Take
the descriptor with `take_fds/2` first, then send it back.

## Cleanup

`Rebus.close/1` closes any descriptor the connection still holds, including
those lent to the handlers of pending calls. An untrappable `:kill` bypasses
that cleanup, so stop connections with `Rebus.close/1`.

## Errors

| Reason | Meaning |
| --- | --- |
| `:unix_fd_unsupported` | This connection cannot pass descriptors at all. It is a TCP connection, or the platform lacks support. |
| `:unix_fd_not_negotiated` | The peer did not agree to descriptor passing. |
| `:unix_fd_send_failed` | An outbound descriptor could not be passed. No byte of the frame was written, the connection remains usable, and the sender still owns the descriptor. |
| `:unix_fd_borrowed` | A reply named a descriptor the connection still owns, such as one a pending call borrowed. Take it with `Rebus.Service.take_fds/2` first. |
| `:fd_claim_expired` | Rebus closed the received descriptors rather than transferring them. |
| `:disconnected` | The connection stopped before ownership transferred. |
