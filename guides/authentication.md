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

A `REJECTED` line is read leniently, because implementations space its
mechanism list out differently. Rebus bounds how many space-separated segments
it will consider, then keeps the ones that are well-formed mechanism names and
ignores the rest, so a trailing or doubled space costs nothing. A line whose
segments are all unusable is an `:auth_failed`, while a bare `REJECTED`
advertises nothing and leaves no alternative to attempt.

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
  Rebus may reach it through up to eight symlinks, following them itself and
  checking the directory it finally reaches.
- The keyring directory and the cookie file are owned by you, and are not
  readable or writable by group or other.
- The keyring directory and the cookie file are real entries, not symlinks.
  Both are derived from the resolved home directory.
- Earlier components of the home path are resolved by the operating system, as
  they are for any path.
- The platform reports POSIX owner and mode metadata.

Most `:auth_cookie_unavailable` failures are a permission problem, and
`chmod go-w ~` with `chmod -R go-rwx ~/.dbus-keyrings` resolves them. A home
reached through more than eight symlinks, or whose path ends in `..`, or that
is reached through a symlink whose target does, fails with the same reason.

### Diagnosing a cookie failure

The returned reason is deliberately coarse, so Rebus logs one warning for each
failed cookie attempt to say which condition was not met:

```
D-Bus cookie authentication unavailable reason=keyring_unsafe
```

The category is also attached as `reason:` Logger metadata, so a structured
backend can filter on it. The line never carries a path, a file name, a cookie
ID, a cookie value, a challenge, a digest, a GUID, an identity, or any raw
protocol data: only the category below is ever interpolated. Nothing is logged
for `:auth_failed`, whose inputs are chosen by the peer.

The peer chooses the cookie context and ID, so a hostile or misconfigured bus
can select among the `cookie_*` categories and produce one warning per
connection attempt; the content stays bounded either way.

| Reason | What to check |
| --- | --- |
| `home_missing` | `HOME` is unset and the system reports no home, or `HOME` is not an absolute path. |
| `home_unsafe` | The home is not a directory, or `chmod go-w ~` is needed; also a chain of more than eight symlinks, a dangling or non-directory link, a path ending in `..`, or an owner other than you. |
| `keyring_unsafe` | `~/.dbus-keyrings` is missing, is a symlink, or needs `chmod 0700 ~/.dbus-keyrings` and the right owner. |
| `cookie_unsafe` | The cookie file the peer named is missing, is a symlink, is over 64 KiB, or needs `chmod 0600` and the right owner. |
| `cookie_changed` | The file was rewritten while Rebus read it. Retry; if it repeats, another process is churning the keyring. |
| `cookie_unreadable` | The file could not be opened or read. Check the mode of the file and the search permission of every directory above it. |
| `keyring_malformed` | The file holds more than 256 records, or the record for the requested ID is malformed. Let the peer regenerate it. |
| `cookie_missing` | No record in the file the peer named carries the ID it asked for. The peer's keyring and yours have diverged; remove the file and let it be regenerated. |
| `cookie_duplicate` | Two records in the file the peer named carry that ID. Rebus refuses to guess; remove the file and let it be regenerated. |
| `unsupported` | The platform reports no POSIX owner and mode metadata, so Rebus cannot prove the file is private. |
| `internal` | Never expected. Please report it. |

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
next entry.

An entry is `unix:path=`, `unix:abstract=`, or `tcp:host=,port=` with an
optional `family=ipv4` or `family=ipv6`. Values use D-Bus percent escapes. A
TCP entry without a family tries up to four resolved IPv6 addresses, then up
to four IPv4 ones, before moving to the next entry. A syntactically valid
unsupported transport is skipped so a later entry can still be used, while a
malformed entry rejects the whole list. `Rebus.BusAddress` documents the
syntax and its limits.

See `Rebus.connect/2` for the complete address, option and timeout contract.

## Errors

| Reason | Meaning |
| --- | --- |
| `:auth_id_unavailable` | The local identity needed for `EXTERNAL` could not be obtained. |
| `:auth_cookie_unavailable` | The peer offered `DBUS_COOKIE_SHA1`, but the local username or a safe matching cookie could not be read. For an address list this is final and no later entry is tried. |
| `:auth_failed` | The peer sent malformed authentication data, or rejected the cookie response. |
| `{:auth_rejected, mechanisms}` | The peer rejected the attempted mechanism and advertised no usable alternative. |
| `:guid_mismatch` | The address named a `guid` that the server's identity did not match. No further address is tried. |
| `:read_timeout` | Socket setup or authentication did not finish within its budget. |

## Candidate retry policy

When `connect/2` works through an address list, whether an authentication
failure ends the attempt or moves on to the next candidate follows from what
the failure describes.

`:auth_failed` and `:read_timeout` describe one peer's behaviour: another
candidate is a different peer, which may well answer correctly and in time, so
the next candidate is tried. The list's own budget still bounds that: once it
is exhausted the attempt ends, whatever remains untried, as `:read_timeout`
if no candidate had yet failed and `{:read_timeout, reason}` once one had.
`{:auth_rejected, mechanisms}` is retried on the same grounds: which
mechanisms a peer offers is that peer's choice, and the next candidate may
offer a usable one.

`:auth_id_unavailable` and `:auth_cookie_unavailable` describe the local
environment - no obtainable identity, or no safely readable cookie. Every
candidate would fail on them identically, so they are final and no later entry
is tried. `:guid_mismatch` is final for the same kind of reason: the address
itself named the identity that did not match.
