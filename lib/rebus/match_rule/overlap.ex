defmodule Rebus.MatchRule.Overlap do
  @moduledoc false

  alias Rebus.MatchRule

  # D-Bus does not tag an incoming signal with the AddMatch rule that admitted
  # it. A well-known `sender=` is deliberately bus-owned (the forwarded header
  # normally contains the owner's unique name), so it cannot safely be checked
  # locally when another overlapping rule could also have admitted the signal.
  # Refuse that ambiguous combination rather than cross-delivering a signal to
  # a sender-pinned handler. Equal well-known senders are safe: both server
  # rules enforce the same sender ownership predicate.
  @doc false
  @spec sender_routing_ambiguous?(MatchRule.t(), MatchRule.t()) :: boolean()
  def sender_routing_ambiguous?(%MatchRule{} = candidate, %MatchRule{} = existing) do
    (well_known_sender?(candidate) or well_known_sender?(existing)) and
      not same_well_known_sender?(candidate, existing) and
      may_overlap?(candidate, existing)
  end

  # This predicate may only prove rules disjoint; any criterion combination it
  # cannot prove disjoint is treated as overlapping. That conservatism keeps
  # the sender guarantee independent of D-Bus name-owner timing.
  @doc false
  @spec may_overlap?(MatchRule.t(), MatchRule.t()) :: boolean()
  def may_overlap?(%MatchRule{criteria: left}, %MatchRule{criteria: right}) do
    exact_criteria_compatible?(left, right) and
      paths_compatible?(left, right) and
      exact_arguments_compatible?(left, right) and
      arg0_namespaces_compatible?(left, right)
  end

  defp well_known_sender?(%MatchRule{criteria: %{sender: sender}}),
    do: not String.starts_with?(sender, ":")

  defp well_known_sender?(%MatchRule{}), do: false

  defp same_well_known_sender?(
         %MatchRule{criteria: %{sender: sender}},
         %MatchRule{criteria: %{sender: sender}}
       ),
       do: not String.starts_with?(sender, ":")

  defp same_well_known_sender?(_left, _right), do: false

  defp exact_criteria_compatible?(left, right) do
    Enum.all?([:interface, :member, :destination], fn key ->
      not (Map.has_key?(left, key) and Map.has_key?(right, key) and left[key] != right[key])
    end)
  end

  defp paths_compatible?(left, right) do
    paths_overlap?(rule_path(left), rule_path(right))
  end

  defp rule_path(%{path: path}), do: {:exact, path}
  defp rule_path(%{path_namespace: path}), do: {:namespace, path}
  defp rule_path(_criteria), do: :any

  defp paths_overlap?(:any, _right), do: true
  defp paths_overlap?(_left, :any), do: true
  defp paths_overlap?({:exact, left}, {:exact, right}), do: left == right

  defp paths_overlap?({:exact, path}, {:namespace, namespace}),
    do: path_in_namespace?(path, namespace)

  defp paths_overlap?({:namespace, namespace}, {:exact, path}),
    do: path_in_namespace?(path, namespace)

  defp paths_overlap?({:namespace, left}, {:namespace, right}) do
    path_in_namespace?(left, right) or path_in_namespace?(right, left)
  end

  defp exact_arguments_compatible?(left, right) do
    left_args = Map.get(left, :args, %{})
    right_args = Map.get(right, :args, %{})

    Enum.all?(left_args, fn {index, value} ->
      case Map.fetch(right_args, index) do
        {:ok, other_value} -> value == other_value
        :error -> true
      end
    end)
  end

  defp arg0_namespaces_compatible?(left, right) do
    case {Map.get(left, :arg0namespace), Map.get(right, :arg0namespace)} do
      {nil, _right} ->
        true

      {_left, nil} ->
        true

      {left_namespace, right_namespace} ->
        left_namespace == right_namespace or
          String.starts_with?(left_namespace, right_namespace <> ".") or
          String.starts_with?(right_namespace, left_namespace <> ".")
    end
  end

  defp path_in_namespace?(path, namespace) do
    path == namespace or
      (namespace == "/" and String.starts_with?(path, "/")) or
      String.starts_with?(path, namespace <> "/")
  end
end
