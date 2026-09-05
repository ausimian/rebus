# Authentication

`Rebus.connect/2` authenticates before it returns. You do not name a
mechanism. Rebus selects one, and fails closed when no safe one remains.

## At a glance

- Rebus tries `EXTERNAL` first, then `DBUS_COOKIE_SHA1`, then `ANONYMOUS`.
- `ANONYMOUS` is only ever used when you pass `allow_anonymous: true`.
- A cookie failure is final. Rebus never falls back to `ANONYMOUS` after one.
- Cookie authentication needs a private keyring under `~/.dbus-keyrings`.
- Cookie contents, challenges and peer identities never appear in errors or
  logs.

## Mechanism order

`EXTERNAL` passes the calling user's local Unix credentials. It is what the
system and session buses normally accept, and Rebus always attempts it first.

If the peer rejects `EXTERNAL`, Rebus reads the mechanisms the peer
advertised and prefers `DBUS_COOKIE_SHA1`. `ANONYMOUS` is considered last, and
only with `allow_anonymous: true`. When no advertised mechanism is usable,
`connect/2` returns `{:error, {:auth_rejected, mechanisms}}`.

## Cookie authentication

`DBUS_COOKIE_SHA1` proves you can read a private cookie file that the peer
also holds. It suits local TCP and peer-to-peer endpoints, where Unix
credentials cannot cross the socket. It adds no encryption and no message
integrity, so keep such TCP endpoints on loopback or inside a protected
transport.

Rebus reads the effective user's name, then reads one file under
`$HOME/.dbus-keyrings`. The peer chooses which file. Authentication fails
unless all of the following hold:

- Your home directory is owned by you and is not group or other writable.
- The keyring directory and the cookie file are owned by you, and are not
  readable or writable by group or other.
- The keyring directory and the cookie file are real entries, not symlinks. A
  symlinked home directory is still allowed.
- The platform reports POSIX owner and mode metadata.

Most `:auth_cookie_unavailable` failures are a permission problem, and
`chmod go-w ~` with `chmod -R go-rwx ~/.dbus-keyrings` resolves them.

Once a cookie exchange has started, every failure is terminal. Rebus does not
cancel it and retry as `ANONYMOUS`, so a misconfigured keyring cannot silently
downgrade your connection.

## Anonymous connections

`ANONYMOUS` performs no authentication, confidentiality or integrity check.
Use it only for an endpoint that is meant to be unauthenticated.

Such an endpoint is not a message bus, so it also needs `bus: false`:

```elixir
{:ok, conn} =
  Rebus.connect(%{family: :local, path: "/tmp/peer"},
    allow_anonymous: true,
    bus: false
  )
```

`:system` and `:session` are message buses by definition and reject
`bus: false`.

## Bus addresses

`Rebus.connect(:system)` and `Rebus.connect(:session)` read a D-Bus address
list and try each supported entry in order until one connects. An entry may
carry a `guid=` value. Rebus compares it with the identity the server reports
during authentication, and a mismatch is final rather than a reason to try the
next entry. See `Rebus.connect/2` for the complete address, option and timeout
contract.

## Errors

| Reason | Meaning |
| --- | --- |
| `:auth_id_unavailable` | The local identity needed for `EXTERNAL` could not be obtained. |
| `:auth_cookie_unavailable` | The peer offered `DBUS_COOKIE_SHA1`, but the local username or a safe matching cookie could not be read. For an address list this is final and no later entry is tried. |
| `:auth_failed` | The peer sent malformed authentication data, or rejected the cookie response. |
| `{:auth_rejected, mechanisms}` | The peer rejected the attempted mechanism and advertised no usable alternative. |
| `:guid_mismatch` | The address named a `guid` that the server's identity did not match. No further address is tried. |
| `:read_timeout` | Socket setup or authentication did not finish within its budget. |
