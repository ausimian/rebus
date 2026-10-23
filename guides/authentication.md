# Authentication and bus addresses

`Rebus.connect/2` authenticates before it returns. It tries `EXTERNAL` first,
then `DBUS_COOKIE_SHA1` if the peer offers it, and considers `ANONYMOUS` only
when `allow_anonymous: true` is set.

`EXTERNAL` presents the local Unix identity and is the normal choice for
system and session buses. If the peer answers with an empty `DATA` challenge,
Rebus repeats its identity once. An `ERROR`, or a second challenge, makes
Rebus send `CANCEL`, after which the peer must reject the mechanism.

`DBUS_COOKIE_SHA1` proves access to a private cookie file. It does not
encrypt the connection or protect message integrity, so keep TCP endpoints on
loopback or inside a protected transport. Once cookie authentication starts,
a failure is final; Rebus does not fall back to `ANONYMOUS`.

## Cookie-file troubleshooting

Cookie authentication reads a file under `~/.dbus-keyrings`. The home
directory, the keyring directory and the selected cookie file must all have
safe POSIX ownership and permissions, and neither the keyring nor the cookie
file may be a symlink. These commands fix the common permission problems:

```console
chmod go-w ~
chmod 0700 ~/.dbus-keyrings
chmod 0600 ~/.dbus-keyrings/*
```

A failure here returns the deliberately general
`{:error, :auth_cookie_unavailable}`. The detail goes to a single logged
warning that names a reason category and nothing else: no path, cookie,
challenge, identity or protocol data.

| Logged reason | What to check |
| --- | --- |
| `home_missing` | `HOME` is not an absolute path, or it is unset and the system reports no home directory. |
| `home_unsafe` | The home is not a directory, has unsafe ownership or permissions, or resolves through an unsafe symlink chain. |
| `keyring_unsafe` | `~/.dbus-keyrings` is missing, a symlink, wrongly owned, or accessible to group or other users. Mode `0700` is suitable. |
| `cookie_unsafe` | The selected file is missing, a symlink, too large, wrongly owned, or has any permissions for group or other users. Mode `0600` is suitable. |
| `cookie_changed` | The file changed while it was read. Retry once; if it repeats, look at the process updating it. |
| `cookie_unreadable` | Check the file mode and directory search permissions. |
| `keyring_malformed` | The keyring contains too many records or the requested record is malformed. Regenerate it. |
| `cookie_missing` | The requested ID is absent. Regenerate the keyring so both peers agree. |
| `cookie_duplicate` | The requested ID appears more than once. Regenerate the keyring. |
| `unsupported` | The platform cannot report the POSIX metadata needed to prove the file is private. |
| `internal` | An unexpected failure. Please report it. |

## Anonymous peers

`ANONYMOUS` verifies no identity. Use it only with an endpoint that is meant
to be unauthenticated, and only on a peer connection:

```elixir
{:ok, conn} =
  Rebus.connect(%{family: :local, path: "/tmp/peer"},
    allow_anonymous: true,
    bus: false
  )
```

`:system` and `:session` always name message buses and reject `bus: false`.

## Bus addresses

`Rebus.connect(:system)` and `Rebus.connect(:session)` read a D-Bus address
list and try the supported entries in order. Rebus accepts `unix:path=`,
`unix:abstract=` and `tcp:host=,port=` entries, with an optional
`family=ipv4` or `family=ipv6`. Values use D-Bus percent escapes, and an
optional `guid=` must match the server's identity.

A malformed entry rejects the whole list. A well-formed entry for an
unsupported transport is skipped, so that a later entry can still connect.
`Rebus.BusAddress` documents the parsing results and bounds, and
`Rebus.connect/2` the full connection, option, retry and timeout contract.

## Connection failures

| Reason | Meaning |
| --- | --- |
| `:auth_id_unavailable` | The local identity for `EXTERNAL` could not be obtained. Later address candidates are not tried. |
| `:auth_cookie_unavailable` | No safe matching cookie or local username was available. The problem is local to the client, so later address candidates are not tried. |
| `:auth_failed` | The peer sent malformed authentication data or rejected the cookie response. |
| `{:auth_rejected, mechanisms}` | The peer offered no usable mechanism. |
| `:guid_mismatch` | The server's identity did not match the address. Later candidates are not tried. |
| `:read_timeout` | Setup or authentication exceeded its budget. |

Failures that are specific to one peer, such as a rejection, malformed
authentication data or a timeout, move on to the next address candidate
while the shared setup budget lasts. `Rebus.connect/2` documents the exact
final result when earlier candidates have also failed.
