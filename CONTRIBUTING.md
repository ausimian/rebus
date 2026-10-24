# Contributing to Rebus

Create a feature branch and open a pull request against `main`. Before asking
for review, run:

```console
mix precommit
```

This compiles with warnings treated as errors, removes unused dependency
locks, formats the project, runs Credo in strict mode, and runs the unit suite.
The unit tests use an in-process test server.

## Native code

Rebus's own dev and test builds enable Unix fd support, so on Unix
`mix compile` builds the small C++17 library behind `Rebus.UnixFD` from
`c_src/` with `make`, and a C++17 compiler and `make` are needed. CI builds it
with warnings treated as errors and checks it with `cppcheck`; do the same
locally with:

```console
REBUS_NIF_WERROR=1 mix compile
make cppcheck
```

Applications get no native code unless they enable it, so CI also runs the
unit suite on a build without fd support. `REBUS_UNIX_FDS=0` makes such a
build, in `_build/no_unix_fds` so that it never mixes with the other:

```console
REBUS_UNIX_FDS=0 mix test
```

## Integration tests

The integration suite uses a real `dbus-daemon` and is excluded from the
ordinary unit suite. On Linux with D-Bus installed, run:

```console
dbus-run-session -- mix test --only integration
```

On other hosts, `mix test.integration` runs the same suite in the container
defined by `docker/Dockerfile` and requires Docker. The suite skips when no
session-bus address is available.

## Documentation

Build the documentation with warnings treated as errors:

```console
mix docs --warnings-as-errors
```
