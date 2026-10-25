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

## Precompiled libraries

Releases publish the library precompiled for six targets; see
`.github/workflows/precompile.yml`, which CI runs on every change. Each
target can be built, checked and tested locally. On Linux, with Docker,
`bin/precompile-linux <target>` builds one Linux target in the image that
sets its oldest supported C library, checks what the library needs from the
system, and runs the unit suite against it on Erlang/OTP 27 and 29. On an
Apple silicon Mac, `bin/precompile-macos build` builds both macOS targets
and `bin/precompile-macos test aarch64-apple-darwin` tests one on the
installed Erlang/OTP. The archives land in `precompiled/`.

These scripts build in a copy of the sources. Running
`mix elixir_make.precompile` or `mix elixir_make.checksum` directly in the
repository writes `checksum.exs`, and while that file exists Rebus's own
builds install the precompiled library instead of building `c_src/`; delete
it afterwards.

## Integration tests

The integration suite uses a real `dbus-daemon` and is excluded from the
ordinary unit suite. On Linux with D-Bus installed, run:

```console
dbus-run-session -- mix test --only integration
```

On other hosts, `mix test.integration` runs the same suite in the container
defined by `docker/Dockerfile` and requires Docker. The suite skips when no
session-bus address is available.

Some tests call Rebus from `busctl`, provided by `systemd`, and `gdbus`,
provided by `libglib2.0-bin` on Debian and Ubuntu. Each of those tests skips
when its client is not installed. CI and the Docker image install both.

## Documentation

Build the documentation with warnings treated as errors:

```console
mix docs --warnings-as-errors
```
