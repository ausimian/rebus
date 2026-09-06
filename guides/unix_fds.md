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
- Signals and inbound method calls never hand a descriptor to your code.

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
calling process owns them from that moment. An error reply carries them the
same way, so check `unix_fds` on both result shapes.

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
  {outcome, %Rebus.Message{unix_fds: fds} = reply}
  when outcome in [:ok, :error] ->
    try do
      handle(outcome, reply)
    after
      Enum.each(fds, &Rebus.UnixFD.close/1)
    end

  {:error, reason} ->
    handle_call_failure(reason)
end
```

## Signals and inbound method calls

Rebus closes the descriptors on an inbound signal and drops that signal. One
raw descriptor cannot be shared safely between several subscribers. The
connection stays up and later signals arrive normally.

Rebus also closes the descriptors on an inbound method call, then answers the
call as usual.

## Timing

An FD-bearing `Rebus.call/3` can return later than the timeout you supplied.
Rebus waits until descriptor ownership is settled so that every received
descriptor has exactly one owner.

If `Rebus.call/3` returns `{:error, :fd_claim_expired}`, Rebus closed the
reply's descriptors instead of transferring them to you. There is nothing
left for you to close.

## Cleanup

`Rebus.close/1` closes any descriptor the connection still holds. An
untrappable `:kill` bypasses that cleanup, so stop connections with
`Rebus.close/1`.

## Errors

| Reason | Meaning |
| --- | --- |
| `:unix_fd_unsupported` | This connection cannot pass descriptors at all. It is a TCP connection, or the platform lacks support. |
| `:unix_fd_not_negotiated` | The peer did not agree to descriptor passing. |
| `:unix_fd_send_failed` | An outbound descriptor could not be passed. No byte of the frame was written, the connection remains usable, and the sender still owns the descriptor. |
| `:fd_claim_expired` | Rebus closed the received descriptors rather than transferring them. |
| `:disconnected` | The connection stopped before ownership transferred. |
