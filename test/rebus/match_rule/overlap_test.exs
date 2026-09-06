defmodule Rebus.MatchRule.OverlapTest do
  use ExUnit.Case, async: true

  alias Rebus.MatchRule
  alias Rebus.MatchRule.Overlap

  describe "sender_routing_ambiguous?/2" do
    test "is false when neither rule pins a well-known sender" do
      candidate = rule(sender: ":1.7")
      existing = rule(sender: ":1.8")

      refute Overlap.sender_routing_ambiguous?(candidate, existing)
      refute Overlap.sender_routing_ambiguous?(rule([]), rule([]))
    end

    test "is true when one side pins a well-known sender and the rules overlap" do
      candidate = rule(sender: "org.example.Service")
      existing = rule(sender: ":1.8")

      assert Overlap.sender_routing_ambiguous?(candidate, existing)
      assert Overlap.sender_routing_ambiguous?(existing, candidate)
      assert Overlap.sender_routing_ambiguous?(candidate, rule([]))
      assert Overlap.sender_routing_ambiguous?(rule([]), candidate)
    end

    test "is false for equal well-known senders" do
      candidate = rule(sender: "org.example.Service", member: "Changed")
      existing = rule(sender: "org.example.Service")

      refute Overlap.sender_routing_ambiguous?(candidate, existing)
    end

    test "is true for different well-known senders that overlap" do
      candidate = rule(sender: "org.example.One")
      existing = rule(sender: "org.example.Two")

      assert Overlap.sender_routing_ambiguous?(candidate, existing)
    end

    test "is false when a well-known sender rule is provably disjoint" do
      candidate = rule(sender: "org.example.Service", member: "Changed")
      existing = rule(sender: ":1.8", member: "Removed")

      refute Overlap.sender_routing_ambiguous?(candidate, existing)
    end
  end

  describe "may_overlap?/2 exact criteria" do
    test "differing interfaces are disjoint" do
      refute Overlap.may_overlap?(
               rule(interface: "org.example.A"),
               rule(interface: "org.example.B")
             )
    end

    test "differing members are disjoint" do
      refute Overlap.may_overlap?(rule(member: "Added"), rule(member: "Removed"))
    end

    test "differing destinations are disjoint" do
      refute Overlap.may_overlap?(rule(destination: ":1.1"), rule(destination: ":1.2"))
    end

    test "equal criteria and absent criteria overlap" do
      assert Overlap.may_overlap?(
               rule(interface: "org.example.A"),
               rule(interface: "org.example.A")
             )

      assert Overlap.may_overlap?(rule(member: "Added"), rule([]))
      assert Overlap.may_overlap?(rule([]), rule(destination: ":1.2"))
    end
  end

  describe "may_overlap?/2 paths" do
    test "differing exact paths are disjoint" do
      refute Overlap.may_overlap?(rule(path: "/org/example/a"), rule(path: "/org/example/b"))
      assert Overlap.may_overlap?(rule(path: "/org/example/a"), rule(path: "/org/example/a"))
    end

    test "an exact path inside a namespace overlaps in both directions" do
      inside = rule(path: "/org/example/a")
      namespace = rule(path_namespace: "/org/example")

      assert Overlap.may_overlap?(inside, namespace)
      assert Overlap.may_overlap?(namespace, inside)
    end

    test "an exact path outside a namespace is disjoint in both directions" do
      outside = rule(path: "/org/other/a")
      namespace = rule(path_namespace: "/org/example")

      refute Overlap.may_overlap?(outside, namespace)
      refute Overlap.may_overlap?(namespace, outside)
    end

    test "a namespace matches itself as an exact path" do
      assert Overlap.may_overlap?(
               rule(path: "/org/example"),
               rule(path_namespace: "/org/example")
             )
    end

    test "a path segment prefix that is not a namespace member is disjoint" do
      # `/org/exampleer` starts with `/org/example` but is not below it: the
      # namespace test is on whole segments.
      refute Overlap.may_overlap?(
               rule(path: "/org/exampleer"),
               rule(path_namespace: "/org/example")
             )
    end

    test "the root namespace contains every path" do
      assert Overlap.may_overlap?(rule(path: "/org/example/a"), rule(path_namespace: "/"))
      assert Overlap.may_overlap?(rule(path_namespace: "/"), rule(path: "/"))
      assert Overlap.may_overlap?(rule(path_namespace: "/org/example"), rule(path_namespace: "/"))
    end

    test "nested namespaces overlap and sibling namespaces do not" do
      assert Overlap.may_overlap?(
               rule(path_namespace: "/org/example"),
               rule(path_namespace: "/org/example/a")
             )

      assert Overlap.may_overlap?(
               rule(path_namespace: "/org/example/a"),
               rule(path_namespace: "/org/example")
             )

      assert Overlap.may_overlap?(
               rule(path_namespace: "/org/example"),
               rule(path_namespace: "/org/example")
             )

      refute Overlap.may_overlap?(
               rule(path_namespace: "/org/example/a"),
               rule(path_namespace: "/org/example/b")
             )
    end

    test "a rule without a path constraint overlaps any path" do
      assert Overlap.may_overlap?(rule([]), rule(path: "/org/example/a"))
      assert Overlap.may_overlap?(rule(path_namespace: "/org/example"), rule([]))
    end
  end

  describe "may_overlap?/2 exact arguments" do
    test "a conflicting value at the same index is disjoint" do
      refute Overlap.may_overlap?(rule(args: %{0 => "one"}), rule(args: %{0 => "two"}))
    end

    test "the same value at the same index overlaps" do
      assert Overlap.may_overlap?(rule(args: %{0 => "one"}), rule(args: %{0 => "one"}))
    end

    test "values at different indexes overlap" do
      assert Overlap.may_overlap?(rule(args: %{0 => "one"}), rule(args: %{1 => "two"}))
    end

    test "a conflict at any shared index is enough" do
      refute Overlap.may_overlap?(
               rule(args: %{0 => "one", 1 => "two"}),
               rule(args: %{1 => "three"})
             )
    end

    test "an unconstrained rule overlaps any arguments" do
      assert Overlap.may_overlap?(rule([]), rule(args: %{0 => "one"}))
      assert Overlap.may_overlap?(rule(args: %{0 => "one"}), rule([]))
    end
  end

  describe "may_overlap?/2 arg0 namespaces" do
    test "equal namespaces overlap" do
      assert Overlap.may_overlap?(
               rule(arg0namespace: "org.example"),
               rule(arg0namespace: "org.example")
             )
    end

    test "a namespace below another overlaps in both directions" do
      assert Overlap.may_overlap?(
               rule(arg0namespace: "org.example"),
               rule(arg0namespace: "org.example.Sub")
             )

      assert Overlap.may_overlap?(
               rule(arg0namespace: "org.example.Sub"),
               rule(arg0namespace: "org.example")
             )
    end

    test "a dotless prefix is not a namespace prefix" do
      # `org.exampleer` starts with `org.example` but is a different namespace:
      # the prefix test requires the `.` boundary.
      refute Overlap.may_overlap?(
               rule(arg0namespace: "org.exampleer"),
               rule(arg0namespace: "org.example")
             )
    end

    test "unrelated namespaces are disjoint" do
      refute Overlap.may_overlap?(
               rule(arg0namespace: "org.example"),
               rule(arg0namespace: "com.example")
             )
    end

    test "an absent namespace overlaps any namespace" do
      assert Overlap.may_overlap?(rule([]), rule(arg0namespace: "org.example"))
      assert Overlap.may_overlap?(rule(arg0namespace: "org.example"), rule([]))
    end
  end

  describe "may_overlap?/2 conservative default" do
    test "rules that cannot be proved disjoint overlap" do
      assert Overlap.may_overlap?(rule([]), rule([]))

      # `arg_paths` is not a criterion this predicate reasons about, so two
      # rules that differ only there are treated as overlapping.
      assert Overlap.may_overlap?(
               rule(arg_paths: %{0 => "/org/example/a"}),
               rule(arg_paths: %{0 => "/org/other/b"})
             )

      # Nor is `sender`: a differing sender never proves disjointness, which is
      # exactly why `sender_routing_ambiguous?/2` exists.
      assert Overlap.may_overlap?(rule(sender: ":1.7"), rule(sender: "org.example.Service"))

      # Every criterion the predicate does reason about agrees here.
      assert Overlap.may_overlap?(
               rule(
                 interface: "org.example.Iface",
                 member: "Changed",
                 path_namespace: "/org/example",
                 args: %{0 => "one"},
                 arg0namespace: "org.example"
               ),
               rule(
                 interface: "org.example.Iface",
                 member: "Changed",
                 path: "/org/example/a",
                 args: %{1 => "two"},
                 arg0namespace: "org.example.Sub"
               )
             )
    end

    test "a single disjoint criterion outweighs every compatible one" do
      refute Overlap.may_overlap?(
               rule(interface: "org.example.Iface", member: "Changed", path: "/org/example/a"),
               rule(interface: "org.example.Iface", member: "Changed", path: "/org/example/b")
             )
    end
  end

  defp rule(criteria), do: MatchRule.new!(criteria)
end
