defmodule Rebus.Identity do
  @moduledoc false

  # The local credentials a connection needs to authenticate: the decimal UID
  # used for EXTERNAL, and the username used as the DBUS_COOKIE_SHA1 initial
  # response. Both are looked up within a bounded timeout and returned exactly
  # as the underlying source produced them; the connection normalises and
  # validates the result.

  @callback auth_id(timeout :: pos_integer()) :: {:ok, binary()} | {:error, term()}
  @callback username(timeout :: pos_integer()) :: {:ok, binary()} | {:error, term()}
end
