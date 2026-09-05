# Image for the real-bus integration suite (`test/integration`).
#
# The repository is bind-mounted at run time rather than copied, so the image
# only carries the toolchain and a D-Bus daemon. `bin/integration-test` builds
# it and runs `dbus-run-session -- mix test --only integration` inside it.
FROM elixir:1.19.5-otp-28

# `dbus` provides both dbus-daemon and dbus-run-session on Debian.
RUN apt-get update \
    && apt-get install -y --no-install-recommends dbus \
    && rm -rf /var/lib/apt/lists/*

RUN mix local.hex --force && mix local.rebar --force

# The repository is bind-mounted here, so Linux build artefacts must not land
# in the `_build/test` the host uses. `_build/` is already git-ignored.
ENV MIX_ENV=test \
    MIX_BUILD_PATH=/app/_build/docker-test

WORKDIR /app

CMD ["dbus-run-session", "--", "mix", "test", "--only", "integration"]
