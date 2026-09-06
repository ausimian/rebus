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
  @name_has_no_owner "org.freedesktop.DBus.Error.NameHasNoOwner"
  # The tracking sequences below are not one caller operation with one budget:
  # each bus round trip gets its own deadline, so a slow AddMatch cannot spend
  # what GetNameOwner needs and turn a healthy service into an untracked one.
  # The steps that only call the connection are bounded `GenServer.call`s, not
  # bus round trips, so they take a fixed timeout of the same size.
  @tracking_step_timeout 1_000
  @tracking_call_timeout 1_000

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

  # Starts tracking the current owner of a well-known sender name, so directed
  # signals forwarded under the owner's unique name can be matched against it.
  # The steps are ordered: the name is marked tracked first, then the bus rule
  # that reports every later change is installed, and only then is the current
  # owner asked for. An owner change cannot slip between the query and the rule
  # that way, and a reply that lost the race to a signal is discarded by
  # `Rebus.Connection.seed_name_owner/4`.
  @doc false
  @spec track_owner(pid(), binary()) :: :ok | {:error, {atom(), term()}}
  def track_owner(conn, name) do
    track = Connection.track_name_owner(conn, name, @tracking_call_timeout)

    with :ok <- tagged(:track, track),
         :ok <-
           tagged(
             :add_match,
             invoke_bus_method(conn, "AddMatch", tracking_rule(name), step_deadline())
           ),
         {:ok, owner} <- tagged(:get_name_owner, get_name_owner(conn, name, step_deadline())) do
      tagged(:seed, Connection.seed_name_owner(conn, name, owner, @tracking_call_timeout))
    end
  end

  # Stops tracking a name no subscription needs any more. The local entry goes
  # whatever the bus made of the RemoveMatch: a tracking rule the bus may still
  # hold only delivers NameOwnerChanged signals that no longer change anything.
  @doc false
  @spec untrack_owner(pid(), binary()) :: :ok | {:error, {atom(), term()}}
  def untrack_owner(conn, name) do
    removed = tagged(:remove_match, remove_tracking_rule(conn, name, step_deadline()))

    untracked =
      tagged(:untrack, Connection.untrack_name_owner(conn, name, @tracking_call_timeout))

    case removed do
      :ok -> untracked
      error -> error
    end
  end

  # The canonical rule that asks the bus driver to report ownership changes for
  # one well-known name.
  @doc false
  @spec tracking_rule(binary()) :: MatchRule.t()
  def tracking_rule(name) do
    MatchRule.new!(
      sender: "org.freedesktop.DBus",
      interface: "org.freedesktop.DBus",
      member: "NameOwnerChanged",
      args: %{0 => name}
    )
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

  defp tagged(_step, :ok), do: :ok
  defp tagged(_step, {:ok, _value} = ok), do: ok
  defp tagged(step, {:error, reason}), do: {:error, {step, reason}}

  # A fresh deadline for one bus round trip of a tracking sequence.
  defp step_deadline, do: System.monotonic_time(:millisecond) + @tracking_step_timeout

  defp remove_tracking_rule(conn, name, deadline) do
    case remove_match(conn, tracking_rule(name), deadline) do
      :ok -> :ok
      {_classification, error} -> error
    end
  end

  defp get_name_owner(conn, name, deadline) do
    with {:ok, timeout} <- remaining_timeout(deadline) do
      case Rebus.call(conn, bus_message("GetNameOwner", name), timeout) do
        {:ok, %Message{} = reply} -> name_owner_result(reply)
        {:error, %Message{} = reply} -> name_owner_result(reply)
        {:error, _reason} = error -> error
      end
    end
  end

  # A name nobody owns is a definite answer, not a failure. Every other error
  # leaves the name unseeded, so directed signals for it stay rejected.
  defp name_owner_result(%Message{} = reply) do
    _ = UnixFD.close_all(reply.unix_fds)

    case reply do
      %Message{type: :method_return, body: [owner]} when is_binary(owner) ->
        {:ok, :binary.copy(owner)}

      %Message{type: :error, header_fields: %{error_name: @name_has_no_owner}} ->
        {:ok, nil}

      %Message{type: :error, header_fields: %{error_name: error_name}}
      when is_binary(error_name) ->
        {:error, {:bus_error, :binary.copy(error_name)}}

      _ ->
        {:error, :invalid_bus_reply}
    end
  end

  defp bus_message(member, %MatchRule{} = rule),
    do: bus_message(member, MatchRule.to_string(rule))

  defp bus_message(member, argument) when is_binary(argument) do
    Message.new!(:method_call,
      path: "/org/freedesktop/DBus",
      interface: "org.freedesktop.DBus",
      destination: "org.freedesktop.DBus",
      member: member,
      signature: "s",
      body: [argument]
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
