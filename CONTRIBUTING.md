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

On Unix, `mix compile` builds the small C++17 library behind `Rebus.UnixFD`
from `c_src/` with `make`, so a C++17 compiler and `make` are needed. CI
builds it with warnings treated as errors and checks it with `cppcheck`; do
the same locally with:

```console
REBUS_NIF_WERROR=1 mix compile
make cppcheck
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
