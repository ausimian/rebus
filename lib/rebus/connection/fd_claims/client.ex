defmodule Rebus.Connection.FDClaims.Client do
  @moduledoc false

  # The caller-side half of the file-descriptor claim protocol. Every function
  # here runs in the process that called `Rebus.call/3`, never in the
  # connection, and reaches the connection only through `GenServer.call/3`.
  #
  # The sequence is: the connection answers a descriptor-bearing reply with
  # `{:fd_claim, ref}`; this module claims it, receives the message on a
  # caller-local one-shot alias, and acknowledges it. Ownership of the
  # descriptors moves to the caller only when that acknowledgement is accepted.
  # Every exit from the sequence either acknowledges, resolves definitively, or
  # discards — so the connection is always left knowing whether to close.
  #
  # See `Rebus.Connection.FDClaims` for the connection-side table.

  alias Rebus.Connection.FDClaims
  alias Rebus.Message

  @doc false
  @spec receive_claim(term(), pid(), integer()) :: term()
  def receive_claim({:fd_claim, claim_ref}, conn, deadline) when is_reference(claim_ref) do
    delivery_ref = make_ref()
    # An alias is the delivery address, not a process mailbox convention. On
    # timeout `unalias/1` atomically rejects in-flight sends; the small drain
    # below merely consumes a message already enqueued before that operation.
    delivery_alias = :erlang.alias([:reply])

    await_reply(conn, claim_ref, delivery_ref, delivery_alias, deadline)
  end

  def receive_claim(%Message{} = msg, _conn, _deadline), do: reply_result(msg)

  def receive_claim(result, _conn, _deadline), do: result

  # A D-Bus error reply is a definitive peer answer, not a transport failure,
  # but callers should not have to test the type to branch on it. The complete
  # message is retained in either shape so its error name, body and any owned
  # descriptors stay available to the caller.
  @doc false
  @spec reply_result(Message.t()) :: {:ok, Message.t()} | {:error, Message.t()}
  def reply_result(%Message{type: :error} = msg), do: {:error, msg}
  def reply_result(%Message{} = msg), do: {:ok, msg}

  defp await_reply(conn, claim_ref, delivery_ref, delivery_alias, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline),
         :ok <- claim(conn, claim_ref, delivery_ref, delivery_alias, timeout) do
      receive do
        {:rebus_fd_reply, ^claim_ref, ^delivery_ref, %Message{} = msg} ->
          # Ownership moves only after the server acknowledges the claim.
          # The first acknowledgement is bounded by the original request
          # deadline plus the handoff grace. If its reply races that bound,
          # the FIFO resolver waits for the definitive transfer-or-close
          # outcome rather than returning an ambiguous raw descriptor.
          case acknowledge(conn, claim_ref, delivery_ref, deadline) do
            :ok -> reply_result(msg)
            {:error, _reason} = error -> error
          end
      after
        timeout ->
          discard(conn, claim_ref, deadline)
          {:error, :timeout}
      end
    else
      {:error, :timeout} ->
        discard(conn, claim_ref, deadline)
        {:error, :timeout}

      {:error, _reason} = error ->
        discard(conn, claim_ref, deadline)
        error
    end
  after
    :erlang.unalias(delivery_alias)
    drain_delivery(claim_ref, delivery_ref)
  end

  defp claim(conn, claim_ref, delivery_ref, delivery_alias, timeout) do
    case GenServer.call(
           conn,
           {:claim_fd_reply, claim_ref, delivery_ref, delivery_alias},
           timeout
         ) do
      :ok -> :ok
      {:error, _reason} = error -> error
      _unexpected -> {:error, :fd_claim_expired}
    end
  catch
    :exit, {:timeout, _call} -> {:error, :timeout}
    :exit, _reason -> {:error, :disconnected}
  end

  defp acknowledge(conn, claim_ref, delivery_ref, deadline) do
    case remaining_timeout(deadline) do
      {:ok, timeout} ->
        call_ack(conn, claim_ref, delivery_ref, timeout)

      :error ->
        resolve(conn, claim_ref, delivery_ref)
    end
  end

  defp call_ack(conn, claim_ref, delivery_ref, timeout) do
    case GenServer.call(conn, {:ack_fd_reply, claim_ref, delivery_ref}, timeout) do
      :ok -> :ok
      {:error, _reason} = error -> error
      _unexpected -> {:error, :fd_claim_expired}
    end
  catch
    :exit, {:timeout, _call} -> resolve(conn, claim_ref, delivery_ref)
    :exit, _reason -> {:error, :disconnected}
  end

  @doc false
  @spec resolve(pid(), reference(), reference()) :: :ok | {:error, term()}
  def resolve(conn, claim_ref, delivery_ref) do
    # The bounded acknowledgement call may time out after its message is
    # already queued. This call is deliberately FIFO and unbounded: every
    # production Connection callback after setup uses :nowait socket I/O and
    # bounded local work, so a live process will dispatch it. A test seam can
    # stall a callback to cover that ordering; the public docs make the rare
    # extended wait explicit. If the connection dies, its monitor makes the
    # only indeterminate case explicit as :disconnected.
    monitor_ref = Process.monitor(conn)

    await_resolution(conn, claim_ref, delivery_ref, monitor_ref)
  end

  defp await_resolution(conn, claim_ref, delivery_ref, monitor_ref) do
    case GenServer.call(conn, {:resolve_fd_claim, claim_ref, delivery_ref}, :infinity) do
      :acknowledged -> :ok
      _ -> {:error, :fd_claim_expired}
    end
  catch
    :exit, _reason -> {:error, :disconnected}
  after
    Process.demonitor(monitor_ref, [:flush])
  end

  defp discard(conn, claim_ref, deadline) do
    case cleanup_remaining_timeout(deadline) do
      {:ok, timeout} ->
        call_discard(conn, claim_ref, timeout)

      :error ->
        :ok
    end
  end

  defp call_discard(conn, claim_ref, timeout) do
    _ = GenServer.call(conn, {:discard_fd_claim, claim_ref}, timeout)
    :ok
  catch
    :exit, _reason -> :ok
  end

  defp drain_delivery(claim_ref, delivery_ref) do
    receive do
      {:rebus_fd_reply, ^claim_ref, ^delivery_ref, %Message{}} -> :ok
    after
      0 -> :ok
    end
  end

  defp remaining_timeout(deadline) do
    remaining = deadline + FDClaims.handoff_grace() - System.monotonic_time(:millisecond)
    if remaining > 0, do: {:ok, remaining}, else: :error
  end

  defp cleanup_remaining_timeout(deadline) do
    remaining = deadline + FDClaims.cleanup_grace() - System.monotonic_time(:millisecond)
    if remaining > 0, do: {:ok, remaining}, else: :error
  end
end
