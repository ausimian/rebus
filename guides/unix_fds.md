# Unix file descriptor passing

> #### Experimental support {: .warning}
>
> Unix file descriptor support is experimental and likely to change in a
> subsequent release.

D-Bus can carry raw Unix file descriptors alongside a message body. Rebus
supports this on Linux and macOS, over local Unix sockets, when the peer
agrees to it during authentication. If the transport cannot carry descriptors
or the peer did not agree to receive them, Rebus rejects the message without
sending any bytes.

Rebus sends raw descriptor numbers, and hands every descriptor it receives
to your code as a `Rebus.UnixFD` handle. A raw number is only a number:
copying it does not duplicate the descriptor, and garbage collection does
not close it, so each one you send must be closed exactly once by whoever
owns it. A handle closes its descriptor exactly once by itself: when any
holder calls `Rebus.UnixFD.close/1`, when its owning process exits, or when
the last reference to it is garbage collected. Use the descriptor behind a
handle with `Rebus.UnixFD.with_fd/2`.

The ownership rules are:

- Rebus does not duplicate or close the raw descriptors you send. You keep
  ownership and close them yourself.
- Handles in a reply to `Rebus.call/3` are owned by the process that called
  it.
- Handles on an inbound method call are owned by the connection and lent to
  the handler for the life of the call. Take them with
  `Rebus.Service.take_fds/2` to keep them.
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

defp handle_and_close(%Rebus.Message{unix_fds: fds} = message, handler) do
  try do
    handler.(message)
  after
    Enum.each(fds, &Rebus.UnixFD.close/1)
  end
end
```

Closing a handle you no longer need releases the descriptor at once. A
handle you leave open is closed when your process exits or when nothing
refers to it any more, so it never leaks for good, but it holds the
descriptor until then. Closing a handle twice is harmless: the second
`Rebus.UnixFD.close/1` returns `{:error, :closed}`. Never close the raw
number you get inside `Rebus.UnixFD.with_fd/2` yourself.

An FD-bearing `Rebus.call/3` honours its timeout like any other call. A
reply that arrives after the call has returned `{:error, :timeout}` is
dropped, and its descriptors are closed as soon as the connection learns
that the call timed out, which the calling process tells it as it gives up.
Only if that news takes more than a second past the call's deadline to
arrive, on a badly overloaded system, do they stay open until the calling
process exits or garbage-collects the dropped reply.

## Signals

Rebus closes the descriptors on an inbound signal and drops the signal. A
handle has one owner, so it cannot be handed to several subscribers. The
connection stays up and later signals arrive normally.

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
sends whatever raw descriptors its message carries. Rebus borrows them, as
it does for every outbound message, so the handler still owns them and
closes them once the answer returns.

A reply cannot carry the number of a descriptor the connection still owns.
That includes the call's own borrowed handles, which answering the call
closes before the reply is written, and handles borrowed by another pending
call. Such a reply returns `{:error, :unix_fd_borrowed}` and leaves the call
open. Take the handle with `take_fds/2` first, then send its number from
inside `Rebus.UnixFD.with_fd/2`.

## Cleanup

`Rebus.close/1` closes every handle the connection still owns, including
those lent to the handlers of pending calls. So does any other way the
connection stops, even an untrappable `:kill`, because a handle closes when
its owner exits.

## Errors

| Reason | Meaning |
| --- | --- |
| `:unix_fd_unsupported` | This connection cannot pass descriptors at all. It is a TCP connection, or the platform lacks support. |
| `:unix_fd_not_negotiated` | The peer did not agree to descriptor passing. |
| `:unix_fd_send_failed` | An outbound descriptor could not be passed. No byte of the frame was written, the connection remains usable, and the sender still owns the descriptor. |
| `:unix_fd_borrowed` | A reply named the number of a descriptor the connection still owns, such as one a pending call borrowed. Take it with `Rebus.Service.take_fds/2` first. |
| `:disconnected` | The connection stopped, closing the handles it owned. |
