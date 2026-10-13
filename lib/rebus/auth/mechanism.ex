defmodule Rebus.Auth.Mechanism do
  @moduledoc false

  # One SASL mechanism the handshake can try. `Rebus.Connection.Handshake`
  # owns the line protocol - framing, the setup deadline, sending AUTH, DATA
  # and CANCEL, and choosing the next mechanism - and asks the mechanism only
  # what the specification leaves to it: what to send, how to answer a
  # challenge, and what the server's verdict means for this mechanism.
  #
  # Every callback that can fail returns `{:error, reason}` with `reason` as
  # the connection reports it, so a mechanism must keep peer-sent bytes and
  # credential material out of it.

  @type state :: term()

  # `auth_id` is the hex-encoded uid sent with EXTERNAL; `timeout` reports
  # what remains of the single setup deadline, for a mechanism that must do
  # bounded local work of its own.
  @type context :: %{
          auth_id: binary(),
          identity: module(),
          timeout: (-> {:ok, pos_integer()} | {:error, :read_timeout})
        }

  # The server's answer to the client's last command, other than DATA. An OK
  # carries a GUID the handshake has already validated and keeps; a REJECTED
  # carries the mechanisms the server advertised.
  @type verdict :: :ok | {:rejected, [binary()]} | :error

  # The name sent after AUTH.
  @callback name() :: binary()

  # The initial response sent with AUTH, already in its wire encoding, or
  # `nil` to send AUTH without one. `{:unavailable, reason}` means the
  # mechanism cannot start and nothing has been sent, so the handshake may
  # try the next advertised mechanism and reports `reason` only if none is
  # left. `{:error, reason}` ends the handshake.
  @callback initial_response(context()) ::
              {:ok, binary() | nil, state()} | {:unavailable, term()} | {:error, term()}

  # Answers a DATA challenge, given its payload, with the payload of a DATA
  # reply, a CANCEL, or a failure that ends the handshake.
  @callback handle_data(data :: binary(), state(), context()) ::
              {:reply, iodata(), state()} | :cancel | {:error, term()}

  # Decides what a verdict means: `:ok` accepts an OK, `:fallback` lets a
  # REJECTED move on to the next advertised mechanism, `:cancel` answers an
  # ERROR with CANCEL, and `{:error, reason}` ends the handshake.
  @callback handle_verdict(verdict(), state()) :: :ok | :fallback | :cancel | {:error, term()}
end
