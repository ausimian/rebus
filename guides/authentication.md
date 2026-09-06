# Authentication and bus addresses

`Rebus.connect/2` authenticates before it returns. Rebus tries `EXTERNAL`
first, then `DBUS_COOKIE_SHA1` when the peer offers it. It considers
`ANONYMOUS` only when `allow_anonymous: true` is set.

`EXTERNAL` uses the local Unix identity and is the normal choice for system
and session buses. `DBUS_COOKIE_SHA1` proves access to a private cookie file;
it does not encrypt the connection or protect message integrity, so keep TCP
endpoints on loopback or within a protected transport. Once cookie
authentication starts, a failure is final and does not fall back to
`ANONYMOUS`.

## Cookie-file troubleshooting

Cookie authentication reads a file under `~/.dbus-keyrings`. Rebus requires
the home directory, keyring directory, and selected cookie file to have safe
POSIX ownership and permissions. The keyring and cookie file cannot be
symlinks. These commands fix the common permission problems:

```console
chmod go-w ~
chmod 0700 ~/.dbus-keyrings
chmod 0600 ~/.dbus-keyrings/*
```

An `{:error, :auth_cookie_unavailable}` result is deliberately general. Rebus
logs one warning with a safe reason category and no path, cookie, challenge,
identity, or protocol data.

| Logged reason | What to check |
| --- | --- |
| `home_missing` | `HOME` is not an absolute path, or it is unset and the system reports no home directory. |
| `home_unsafe` | The home is not a directory, has unsafe ownership or permissions, or resolves through an unsafe symlink chain. |
| `keyring_unsafe` | `~/.dbus-keyrings` is missing, a symlink, wrongly owned, or accessible to group or other users. Mode `0700` is suitable. |
| `cookie_unsafe` | The selected file is missing, a symlink, too large, wrongly owned, or has any permissions for group or other users. Mode `0600` is suitable. |
| `cookie_changed` | The file changed while it was read. Retry once and inspect the process updating it if this repeats. |
| `cookie_unreadable` | Check the file mode and directory search permissions. |
| `keyring_malformed` | The keyring contains too many records or the requested record is malformed. Regenerate it. |
| `cookie_missing` | The requested ID is absent. Regenerate the keyring so both peers agree. |
| `cookie_duplicate` | The requested ID appears more than once. Regenerate the keyring. |
| `unsupported` | The platform cannot report the POSIX metadata needed to prove the file is private. |
| `internal` | Report this unexpected failure. |

## Anonymous peers

`ANONYMOUS` verifies no identity. Use it only with an endpoint intended to be
unauthenticated. Such an endpoint must also be a peer connection:

```elixir
{:ok, conn} =
  Rebus.connect(%{family: :local, path: "/tmp/peer"},
    allow_anonymous: true,
    bus: false
  )
```

`:system` and `:session` always identify message buses and reject `bus: false`.

## Bus addresses

`Rebus.connect(:system)` and `Rebus.connect(:session)` read a D-Bus address
list and try supported entries in order. Rebus accepts `unix:path=`,
`unix:abstract=`, and `tcp:host=,port=` entries, with optional `family=ipv4`
or `family=ipv6`. Values use D-Bus percent escapes. An optional `guid=` must
match the server identity.

A malformed entry rejects the list. A syntactically valid unsupported
transport is skipped so that a later entry can still connect. See
`Rebus.BusAddress` for parsing results and bounds, and `Rebus.connect/2` for
the complete connection, option, retry, and timeout contract.

## Connection failures

- `:auth_id_unavailable` means the local identity for `EXTERNAL` could not be
  obtained. Later address candidates are not tried.
- `:auth_cookie_unavailable` means a safe matching cookie or local username
  was unavailable. This is local to the client, so later address candidates
  are not tried.
- `:auth_failed` means the peer sent malformed authentication data or rejected
  the cookie response.
- `{:auth_rejected, mechanisms}` means the peer offered no usable mechanism.
- `:guid_mismatch` means the server identity did not match the address. Later
  candidates are not tried.
- `:read_timeout` means setup or authentication exceeded its budget.

Peer-specific failures such as rejection, malformed authentication data, and
timeouts may move to the next address candidate while the shared setup budget
remains. `Rebus.connect/2` documents the exact final result when earlier
candidates have also failed.
