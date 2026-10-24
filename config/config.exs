import Config

# Rebus's own builds pass Unix file descriptors in dev and test, unless
# REBUS_UNIX_FDS=0 builds them without the native library. mix.exs repeats
# this default, because Mix evaluates it before loading this file.
#
# This file is never loaded when Rebus is a dependency: an application
# enables Unix fd support with `config :rebus, unix_fds: true` in its own
# config/config.exs.
config :rebus,
  unix_fds: config_env() in [:dev, :test] and System.get_env("REBUS_UNIX_FDS") != "0"
