defmodule Rebus.MatchSubscription.Operation do
  @moduledoc false

  # The bodies of the tasks that `Rebus.MatchSubscription.Worker` runs under
  # `Rebus.MatchSubscription.TaskSupervisor`. They talk to the connection and
  # the bus, they never touch worker state, and their return values are the
  # worker's `{:operation_result, token, result}` payloads: the worker's
  # `complete_*/4` clauses match on the tuples below.

  alias Rebus.Connection
  alias Rebus.MatchRule
  alias Rebus.Message
  alias Rebus.UnixFD

  @match_rule_not_found "org.freedesktop.DBus.Error.MatchRuleNotFound"

  @type error :: {:error, term()}

  @type recovery :: %{
          pending_handlers: MapSet.t(reference()),
          recovery_kind: :handlers | :rule,
          remote_may_exist?: boolean(),
          rule: MatchRule.t()
        }

  @doc false
  @spec add_new(pid(), pid(), MatchRule.t(), integer()) ::
          {:added, reference()} | {:add_failed, error(), reference() | nil}
  def add_new(conn, owner, rule, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline),
         {:ok, handler_ref} <- install_local_handler(conn, owner, rule, timeout) do
      case invoke_bus_method(conn, "AddMatch", rule, deadline) do
        :ok -> {:added, handler_ref}
        {:error, _reason} = error -> {:add_failed, error, handler_ref}
      end
    else
      {:error, _reason} = error -> {:add_failed, error, nil}
    end
  end

  @doc false
  @spec add_existing(pid(), pid(), MatchRule.t(), integer()) ::
          {:added_existing, reference()} | {:add_existing_failed, error()}
  def add_existing(conn, owner, rule, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline),
         {:ok, handler_ref} <- install_local_handler(conn, owner, rule, timeout) do
      {:added_existing, handler_ref}
    else
      {:error, _reason} = error -> {:add_existing_failed, error}
    end
  end

  @doc false
  @spec remove(pid(), MatchRule.t(), reference(), :active | :removed, boolean(), integer()) ::
          {:removed, reference(), :final | :nonfinal}
          | {:remove_failed, error(), :active}
          | {:remove_definitive_error, reference(), error()}
          | {:remove_ambiguous, reference(), error()}
  def remove(conn, rule, ref, handler, final?, deadline) do
    case ensure_handler_removed(conn, ref, handler, deadline) do
      :ok ->
        remove_bus_rule(conn, rule, ref, final?, deadline)

      {:error, _reason} = error ->
        {:remove_failed, error, :active}
    end
  end

  @doc false
  @spec recover(pid(), recovery(), integer()) ::
          :cleared
          | :handlers_cleared
          | {:retry, :handlers | :remote}
          | {:definitive_bus_error, error()}
  def recover(conn, rule, deadline) do
    case remove_pending_handlers(conn, rule.pending_handlers, deadline) do
      :ok ->
        recover_rule_state(conn, rule, deadline)

      {:error, _reason} ->
        {:retry, :handlers}
    end
  end

  # A bus error names a fault the bus is certain about, so it settles whether
  # the server-side rule exists. The worker uses this to decide whether a
  # failed AddMatch may still have installed a rule.
  @doc false
  @spec definitive_bus_error?(term()) :: boolean()
  def definitive_bus_error?({:error, {:bus_error, error_name}}) when is_binary(error_name),
    do: true

  def definitive_bus_error?({:error, {:reply_dropped, {:error, error_name}}})
      when is_binary(error_name),
      do: true

  def definitive_bus_error?(_error), do: false

  # The caller's deadline is one budget for a whole operation, so each step
  # takes what is left of it. Shared with the worker, which bounds its own
  # calls against the same deadline.
  @doc false
  @spec remaining_timeout(integer()) :: {:ok, pos_integer()} | {:error, :timeout}
  def remaining_timeout(deadline) when is_integer(deadline) do
    case deadline - System.monotonic_time(:millisecond) do
      remaining when remaining > 0 -> {:ok, remaining}
      _expired -> {:error, :timeout}
    end
  end

  defp remove_bus_rule(_conn, _rule, ref, false, _deadline), do: {:removed, ref, :nonfinal}

  defp remove_bus_rule(conn, rule, ref, true, deadline) do
    case remove_match(conn, rule, deadline) do
      :ok -> {:removed, ref, :final}
      {:definitive, error} -> {:remove_definitive_error, ref, error}
      {:ambiguous, error} -> {:remove_ambiguous, ref, error}
    end
  end

  # Shared RemoveMatch classification: a rule the bus does not know is already
  # gone, a bus error is definitive, and anything else leaves the server-side
  # rule unresolved.
  defp remove_match(conn, rule, deadline) do
    case invoke_bus_method(conn, "RemoveMatch", rule, deadline) do
      :ok ->
        :ok

      error ->
        cond do
          match_rule_not_found?(error) -> :ok
          definitive_bus_error?(error) -> {:definitive, error}
          true -> {:ambiguous, error}
        end
    end
  end

  defp recover_rule_state(_conn, %{recovery_kind: :handlers}, _deadline), do: :handlers_cleared

  defp recover_rule_state(conn, %{recovery_kind: :rule} = rule, deadline) do
    case remove_remote_rule(conn, rule, deadline) do
      :ok -> :cleared
      {:definitive, error} -> {:definitive_bus_error, error}
      :ambiguous -> {:retry, :remote}
    end
  end

  defp remove_pending_handlers(conn, handler_refs, deadline) do
    Enum.reduce_while(handler_refs, :ok, fn handler_ref, :ok ->
      case delete_local_handler(conn, handler_ref, deadline) do
        :ok -> {:cont, :ok}
        {:error, _reason} = error -> {:halt, error}
      end
    end)
  end

  defp remove_remote_rule(_conn, %{remote_may_exist?: false}, _deadline), do: :ok

  defp remove_remote_rule(conn, %{rule: rule}, deadline) do
    case remove_match(conn, rule, deadline) do
      {:ambiguous, _error} -> :ambiguous
      result -> result
    end
  end

  defp ensure_handler_removed(_conn, _ref, :removed, _deadline), do: :ok

  defp ensure_handler_removed(conn, ref, :active, deadline),
    do: delete_local_handler(conn, ref, deadline)

  defp install_local_handler(conn, owner, rule, timeout) do
    handler_ref = make_ref()

    case Connection.add_signal_handler(conn, owner, handler_ref, rule, timeout) do
      {:ok, ^handler_ref} = ok -> ok
      {:error, _reason} = error -> error
    end
  end

  defp delete_local_handler(conn, handler_ref, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline) do
      Connection.delete_signal_handler(conn, handler_ref, timeout)
    end
  end

  defp invoke_bus_method(conn, member, rule, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline) do
      # A D-Bus error reply now arrives as {:error, %Message{}}. Both reply
      # shapes must reach bus_reply_result/1 so any received descriptors are
      # closed before the reason is classified.
      case Rebus.call(conn, bus_message(member, rule), timeout) do
        {:ok, %Message{} = reply} -> bus_reply_result(reply)
        {:error, %Message{} = reply} -> bus_reply_result(reply)
        {:error, _reason} = error -> error
      end
    end
  end

  defp bus_message(member, rule) do
    Message.new!(:method_call,
      path: "/org/freedesktop/DBus",
      interface: "org.freedesktop.DBus",
      destination: "org.freedesktop.DBus",
      member: member,
      signature: "s",
      body: [MatchRule.to_string(rule)]
    )
  end

  # These internal method calls never expose a D-Bus reply to application
  # code. Retain neither its body nor any received descriptors; in particular,
  # a malicious AddMatch/RemoveMatch reply must not leak Unix FD ownership into
  # the worker process.
  defp bus_reply_result(%Message{} = reply) do
    _ = UnixFD.close_all(reply.unix_fds)

    case reply do
      %Message{type: :method_return} ->
        :ok

      %Message{type: :error, header_fields: %{error_name: error_name}}
      when is_binary(error_name) ->
        {:error, {:bus_error, :binary.copy(error_name)}}

      _ ->
        {:error, :invalid_bus_reply}
    end
  end

  defp match_rule_not_found?({:error, {:bus_error, @match_rule_not_found}}), do: true

  defp match_rule_not_found?({:error, {:reply_dropped, {:error, @match_rule_not_found}}}),
    do: true

  defp match_rule_not_found?(_error), do: false
end
