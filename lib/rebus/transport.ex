defmodule Rebus.Transport do
  @moduledoc false

  # The socket operations a connection performs, named and shaped exactly like
  # their `:socket` counterparts so the production implementation is a set of
  # direct delegations and tests can substitute a module instead of reaching
  # into connection state.

  @type socket :: :socket.socket()

  @callback open(term(), term(), term()) :: {:ok, socket()} | {:error, term()}
  @callback connect(socket(), term(), term()) :: :ok | {:error, term()} | term()
  @callback send(socket(), term(), term(), term()) :: term()
  @callback sendmsg(socket(), term(), term(), term()) :: term()
  @callback recv(socket(), term(), term(), term()) :: term()
  @callback recvmsg(socket(), term(), term(), term(), term()) :: term()
  @callback cancel(socket(), term()) :: term()
  @callback setopt(socket(), term(), term()) :: term()
  @callback close(socket()) :: term()
end
