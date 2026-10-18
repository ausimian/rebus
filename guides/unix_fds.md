# Unix file descriptor passing

D-Bus can carry raw Unix file descriptors alongside a message body. Rebus
supports this on connections where the platform and the peer both allow it.

## At a glance

- Descriptor passing works on Linux and macOS, over local Unix sockets only.
- Attach outbound descriptors with `fds:`, and index them from the body with
  `h` values.
- Rebus borrows outbound descriptors. The sender keeps ownership.
- A reply's descriptors arrive in `unix_fds`. The caller owns them and must
  close each one exactly once.
- Signals never hand a descriptor to your code.
- An inbound method call lends its descriptors to the handler until the call
  completes. `Rebus.Service.take_fds/2` takes ownership of them.

## Sending descriptors

Pass the raw descriptors as `fds:` when you build the message. Each `h` value
in the body is a zero-based index into that list.

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

Rebus borrows these descriptors. It never duplicates or closes them, so the
sender stays responsible for closing them once the call returns.

Rebus asks the peer to enable descriptor passing during authentication. An
FD-bearing message is rejected before any byte is written when the transport
cannot carry descriptors, or when the peer declined.

## Receiving descriptors

A `Rebus.call/3` reply exposes received descriptors in `message.unix_fds`. The
calling process owns them from that moment. A D-Bus error reply carries them
the same way, so check `unix_fds` on both `{:ok, reply}` and
`{:error, {:dbus_error, reply}}`.

Close each descriptor exactly once with `Rebus.UnixFD.close/1`, or adopt it
with an OS or OTP API of your own. Never close one twice. Operating systems
reuse descriptor numbers, so a second close can close an unrelated resource.

`Rebus.UnixFD.close/1` returns `:ok` on success and `{:error, reason}` when the
descriptor could not be adopted or the operating system refused the close. Once
a descriptor has been adopted the result is informational: a failed close spends
it anyway, and the number is never safe to reuse. Report or log the reason if it
matters to you, but never retry the close.

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

## Signals

Rebus closes the descriptors on an inbound signal and drops that signal. One
raw descriptor cannot be shared safely between several subscribers. The
connection stays up and later signals arrive normally.

## Inbound method calls

A method call to an object exported with `Rebus.Service.export/4` arrives with
its descriptors in `call.unix_fds`. The handler borrows them: it may read them,
or pass them on, while the call is pending, but the connection still owns them
and closes each one exactly once when the call completes. That happens when the
call is answered, including the answer to a call that asked for no reply, when
it expires, when its handler exits, when its object is unexported, and when the
connection closes. A handler that ignores the descriptors leaks nothing, and
must never close one it has not taken.

To keep the descriptors beyond the call, take ownership before answering it:

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

`take_fds/2` hands the descriptors over the same way an FD-bearing reply is
handed to `Rebus.call/3`, so ownership moves only once the handover is
settled. On `{:error, reason}` the connection has already closed them, and
there is nothing left for you to close. A second `take_fds/2` for the same call
returns `{:error, :unknown_call}`. The call stays pending either way and still
needs its answer.

A call that reaches no handler, because nothing is exported at its path, it
names an undeclared method, or too many calls are already pending, has its
descriptors closed as the connection answers it.

### Descriptors in replies

`Rebus.Service.reply/3` and `Rebus.Service.reply_error/4` take the same `fds:`
option as `Rebus.Message.new/2`, and `Rebus.Service.reply_message/3` sends whatever descriptors its message
carries. Rebus borrows them, as it does for every outbound message, so the
handler still owns them and closes them once the answer returns.

A reply cannot carry a descriptor the connection still owns, such as one of
the call's own borrowed descriptors, which answering the call closes before the
reply is written, or one borrowed by another pending call. Such a reply returns
`{:error, :unix_fd_borrowed}` and leaves the call open. Take the descriptor
with `take_fds/2` first, then send it back.

## Timing

An FD-bearing `Rebus.call/3`, like `Rebus.Service.take_fds/2`, can return later
than the timeout you supplied. Rebus waits until descriptor ownership is
settled so that every received descriptor has exactly one owner.

If `Rebus.call/3` returns `{:error, :fd_claim_expired}`, Rebus closed the
reply's descriptors instead of transferring them to you. There is nothing
left for you to close.

## Cleanup

`Rebus.close/1` closes any descriptor the connection still holds, including
those lent to the handlers of pending calls. An
untrappable `:kill` bypasses that cleanup, so stop connections with
`Rebus.close/1`.

## Errors

| Reason | Meaning |
| --- | --- |
| `:unix_fd_unsupported` | This connection cannot pass descriptors at all. It is a TCP connection, or the platform lacks support. |
| `:unix_fd_not_negotiated` | The peer did not agree to descriptor passing. |
| `:unix_fd_send_failed` | An outbound descriptor could not be passed. No byte of the frame was written, the connection remains usable, and the sender still owns the descriptor. |
| `:unix_fd_borrowed` | A reply named a descriptor the connection still owns, such as one a pending call borrowed. Take it with `Rebus.Service.take_fds/2` first. |
| `:fd_claim_expired` | Rebus closed the received descriptors rather than transferring them. |
| `:disconnected` | The connection stopped before ownership transferred. |
