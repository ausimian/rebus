:ok = Rebus.TestImpl.setup!()

# The integration suite in `test/integration` needs a real `dbus-daemon` and is
# excluded from the ordinary run. Include it with
# `dbus-run-session -- mix test --only integration`, or `mix test.integration`
# to run that command inside the Docker image in `docker/Dockerfile`.
ExUnit.start(exclude: [:integration])
