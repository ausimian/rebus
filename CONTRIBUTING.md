# Contributing to Rebus

Create a feature branch and open a pull request against `main`. Before asking
for review, run:

```console
mix precommit
```

This compiles with warnings treated as errors, removes unused dependency
locks, formats the project, runs Credo in strict mode, and runs the unit suite.
The unit tests use an in-process test server.

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
