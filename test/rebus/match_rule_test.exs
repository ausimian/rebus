defmodule Rebus.MatchRuleTest.PlaceholderConnection do
  @moduledoc false
  # Stands in for a connection in the unit-level worker tests, which drive the
  # worker's callbacks directly rather than through a bus. A transition under
  # test may start a recovery or reset task that calls into the connection, and
  # a process that answers such a call by crashing would take its own persisted
  # rows down with it and race every post-state assertion. Answering keeps the
  # placeholder alive and the assertions deterministic. It is deliberately
  # started unlinked for the same reason; the test's `on_exit` kills it.
  use GenServer

  def start, do: GenServer.start(__MODULE__, :ok)

  @impl true
  def init(:ok), do: {:ok, :placeholder}

  @impl true
  def handle_call(_request, _from, state), do: {:reply, {:error, :not_connected}, state}

  @impl true
  def handle_cast(_request, state), do: {:noreply, state}
end

defmodule Rebus.MatchRuleTest do
  use ExUnit.Case

  import ExUnit.CaptureLog
  import Rebus.TestRestartBudget, only: [kill_supervised: 2]

  alias Rebus.MatchRule
  alias Rebus.MatchRuleTest.PlaceholderConnection
  alias Rebus.MatchSubscription.Operation
  alias Rebus.MatchSubscription.Store
  alias Rebus.MatchSubscription.Worker
  alias Rebus.Message
  alias Rebus.TestServer

  describe "MatchRule" do
    test "builds canonical, quoted signal rules" do
      assert {:ok, rule} =
               MatchRule.new(
                 member: "Changed",
                 sender: "org.example.Service",
                 args: %{2 => "two", 0 => "don't"},
                 arg_paths: [{1, "/org/example/"}]
               )

      assert MatchRule.to_string(rule) ==
               "type='signal',sender='org.example.Service',member='Changed',arg0='don'\\''t',arg2='two',arg1path='/org/example/'"
    end

    test "rejects unsafe, conflicting, and oversized criteria" do
      assert {:error, :invalid_match_option} = MatchRule.new(eavesdrop: true)
      assert {:error, :invalid_match_rule} = MatchRule.new(["not a keyword entry"])
      assert {:error, :conflicting_match_paths} = MatchRule.new(path: "/a", path_namespace: "/a")
      assert {:error, :invalid_match_argument} = MatchRule.new(args: %{64 => "too-far"})
      assert {:error, :invalid_match_value} = MatchRule.new(destination: "org.example.NotUnique")

      assert {:error, :match_rule_too_long} =
               MatchRule.new(args: %{0 => String.duplicate("x", 1_100)})
    end

    test "validates names against the D-Bus specification's name grammar" do
      # D-Bus specification, "Valid Names": "Bus names must contain at least
      # one '.' (period) character (and thus at least two elements)", and a
      # match rule's destination is a unique connection name.
      assert {:error, :invalid_match_value} = MatchRule.new(destination: ":1")
      assert {:ok, _rule} = MatchRule.new(destination: ":1.7")

      assert {:error, :invalid_match_value} = MatchRule.new(sender: ":1")
      assert {:ok, _rule} = MatchRule.new(sender: ":1.7")

      assert {:error, :invalid_match_value} = MatchRule.new(interface: "Foo")
      assert {:ok, _rule} = MatchRule.new(interface: "org.example.Foo")

      assert {:error, :invalid_match_value} = MatchRule.new(member: "org.example.Changed")
      assert {:ok, _rule} = MatchRule.new(member: "Changed")

      assert {:error, :invalid_match_value} = MatchRule.new(arg0namespace: ":1.7")
      assert {:ok, _rule} = MatchRule.new(arg0namespace: "org")
    end

    test "filters only safely supported criteria after bus routing" do
      rule =
        MatchRule.new!(
          sender: "org.example.Service",
          path_namespace: "/org/example",
          args: %{0 => "changed"},
          arg_paths: %{1 => "/org/example/"},
          arg0namespace: "changed"
        )

      matching =
        Message.new!(:signal,
          sender: "org.example.Service",
          path: "/org/example/child",
          interface: "org.example.Interface",
          member: "Changed",
          signature: "sos",
          body: ["changed", "/org/example/child", "unused"]
        )

      assert MatchRule.matches?(rule, matching)

      refute MatchRule.matches?(rule, %{
               matching
               | body: ["other", "/org/example/child", "unused"]
             })

      assert MatchRule.matches?(rule, %{
               matching
               | header_fields: %{matching.header_fields | sender: ":1.42"}
             })

      refute MatchRule.matches?(rule, %{matching | type: :method_call})
    end

    test "locally filters a unique sender without treating a well-known name literally" do
      rule = MatchRule.new!(sender: ":1.42", member: "Changed")

      assert MatchRule.matches?(
               rule,
               Message.new!(:signal,
                 sender: ":1.42",
                 path: "/org/example",
                 interface: "org.example.Interface",
                 member: "Changed"
               )
             )

      refute MatchRule.matches?(
               rule,
               Message.new!(:signal,
                 sender: ":1.43",
                 path: "/org/example",
                 interface: "org.example.Interface",
                 member: "Changed"
               )
             )
    end

    test "filters directed signals by an exact well-known sender" do
      rule = MatchRule.new!(sender: "org.example.Service", member: "Changed")

      broadcast =
        Message.new!(:signal,
          sender: ":1.42",
          path: "/org/example",
          interface: "org.example.Interface",
          member: "Changed"
        )

      assert MatchRule.matches?(rule, broadcast)

      refute MatchRule.matches?(rule, %{
               broadcast
               | header_fields: Map.put(broadcast.header_fields, :destination, ":1.100")
             })

      assert MatchRule.matches?(rule, %{
               broadcast
               | header_fields:
                   broadcast.header_fields
                   |> Map.put(:sender, "org.example.Service")
                   |> Map.put(:destination, ":1.100")
             })
    end

    test "matches a directed well-known sender against the tracked owner" do
      rule = MatchRule.new!(sender: "org.example.Service", member: "Changed")
      owners = %{"org.example.Service" => ":1.42"}

      directed = fn sender ->
        Message.new!(:signal,
          sender: sender,
          destination: ":1.100",
          path: "/org/example",
          interface: "org.example.Interface",
          member: "Changed"
        )
      end

      # The bus driver's own name still matches literally, and so does the
      # unique name it currently reports as the owner. Nothing else does.
      assert MatchRule.matches?(rule, directed.("org.example.Service"), owners)
      assert MatchRule.matches?(rule, directed.(":1.42"), owners)
      refute MatchRule.matches?(rule, directed.(":1.99"), owners)

      # Tracked but unseeded, unowned, untracked, and no sender header at all.
      refute MatchRule.matches?(rule, directed.(":1.42"), %{"org.example.Service" => :unknown})
      refute MatchRule.matches?(rule, directed.(":1.42"), %{"org.example.Service" => nil})
      refute MatchRule.matches?(rule, directed.(":1.42"), %{})

      anonymous = directed.(":1.42")

      refute MatchRule.matches?(
               rule,
               %{anonymous | header_fields: Map.delete(anonymous.header_fields, :sender)},
               owners
             )

      # A broadcast signal is still left to the bus, and a unique-name sender
      # criterion is still compared directly.
      broadcast =
        Message.new!(:signal,
          sender: ":1.99",
          path: "/org/example",
          interface: "org.example.Interface",
          member: "Changed"
        )

      assert MatchRule.matches?(rule, broadcast, %{})

      unique = MatchRule.new!(sender: ":1.42", member: "Changed")
      assert MatchRule.matches?(unique, directed.(":1.42"), owners)
      refute MatchRule.matches?(unique, directed.(":1.99"), owners)

      # `matches?/2` knows no owners, so it is `matches?/3` with an empty table.
      assert MatchRule.matches?(rule, directed.("org.example.Service"))
      refute MatchRule.matches?(rule, directed.(":1.42"))
    end

    test "matches a directed signal for a unique sender" do
      rule = MatchRule.new!(sender: ":1.42", member: "Changed")

      assert MatchRule.matches?(
               rule,
               Message.new!(:signal,
                 sender: ":1.42",
                 destination: ":1.100",
                 path: "/org/example",
                 interface: "org.example.Interface",
                 member: "Changed"
               )
             )
    end
  end

  describe "bus match subscriptions" do
    setup do
      {:ok, server} = start_supervised({TestServer, tap: self()})
      {:ok, address} = TestServer.get_listen_addr(server)
      {:ok, connection} = Rebus.connect(address)
      assert_receive {^server, %Message{header_fields: %{member: "Hello"}}}, 1_000

      on_exit(fn ->
        _ = Rebus.close(connection)
      end)

      %{server: server, connection: connection}
    end

    test "adds once, filters locally, and removes after the final reference", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner_a = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref_a = assert_subscription(owner_a)
      :ok = answer_tracking(server)

      owner_b = subscribe_owner(self(), connection, rule)
      ref_b = assert_subscription(owner_b)
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100
      assert rule_status(connection, rule) == :active
      assert rule_ref_count(connection, rule) == 2

      :ok = TestServer.push(server, test_signal("other"))
      refute_receive {:matched, ^owner_a, ^ref_a, _message}, 100
      refute_receive {:matched, ^owner_b, ^ref_b, _message}, 100

      :ok = TestServer.push(server, test_signal("changed"))
      assert_receive {:matched, ^owner_a, ^ref_a, %Message{body: ["changed"]}}, 1_000
      assert_receive {:matched, ^owner_b, ^ref_b, %Message{body: ["changed"]}}, 1_000

      assert :ok = Rebus.remove_match(connection, ref_a)
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 100

      removal = Task.async(fn -> Rebus.remove_match(connection, ref_b, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(removal, 1_000)

      :ok = TestServer.push(server, test_signal("changed"))
      refute_receive {:matched, ^owner_a, ^ref_a, _message}, 100
      refute_receive {:matched, ^owner_b, ^ref_b, _message}, 100

      # The final reference took the rule with it, leaving nothing to restore.
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      assert wait_until(fn -> not Store.persisted?(connection) end)

      send(owner_a, :stop)
      send(owner_b, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "queues a same-rule add while the first AddMatch is still in flight", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      first = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)

      second = subscribe_owner(self(), connection, rule)
      assert wait_until(fn -> rule_queue_length(connection, rule) == 1 end)
      assert rule_status(connection, rule) == :installing
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100

      :ok = TestServer.push(server, method_return(add.serial))
      first_ref = assert_subscription(first)
      second_ref = assert_subscription(second)
      refute first_ref == second_ref
      :ok = answer_tracking(server)

      # The queued caller was served from the rule the first one installed.
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100
      assert rule_status(connection, rule) == :active
      assert rule_ref_count(connection, rule) == 2
      assert rule_queue_length(connection, rule) == 0

      send(first, :stop)
      send(second, :stop)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
    end

    test "filters directed signals by an exact well-known sender subscription", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      :ok =
        TestServer.push(server, test_signal("changed", sender: ":1.99", destination: ":1.100"))

      refute_receive {:matched, ^owner, ^ref, _message}, 100

      :ok =
        TestServer.push(
          server,
          test_signal("changed", sender: "org.example.Service", destination: ":1.100")
        )

      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

      send(owner, :stop)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
    end

    test "delivers a directed signal from the well-known sender's tracked owner", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)

      :ok = answer_tracking(server, "org.example.Service", ":1.42")
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.42" end)

      :ok = TestServer.push(server, directed_signal(":1.42"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

      :ok = TestServer.push(server, directed_signal(":1.99"))
      refute_receive {:matched, ^owner, ^ref, _message}, 100

      # The literal well-known name still matches: that is the bus driver.
      :ok = TestServer.push(server, directed_signal("org.example.Service"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

      send(owner, :stop)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
    end

    test "installs the owner tracking rule before asking who owns the name", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)

      # The rule that reports every later change must be in place before the
      # current owner is asked for, so no change can slip between the two.
      tracking_add = assert_add_match(server, tracking_rule())
      assert tracking_add.body == [MatchRule.to_string(tracking_rule())]
      refute_receive {^server, %Message{header_fields: %{member: "GetNameOwner"}}}, 100

      :ok = TestServer.push(server, method_return(tracking_add.serial))
      get = assert_get_name_owner(server, "org.example.Service")
      :ok = TestServer.push(server, name_owner_reply(get.serial, ":1.42"))
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.42" end)

      send(owner, :stop)
    end

    test "follows the tracked name to its new owner and to none", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)

      :ok = answer_tracking(server, "org.example.Service", ":1.42")
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.42" end)

      :ok =
        TestServer.push(server, name_owner_changed("org.example.Service", ":1.42", ":1.77"))

      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.77" end)

      :ok = TestServer.push(server, directed_signal(":1.77"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

      :ok = TestServer.push(server, directed_signal(":1.42"))
      refute_receive {:matched, ^owner, ^ref, _message}, 100

      :ok = TestServer.push(server, name_owner_changed("org.example.Service", ":1.77", ""))
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == nil end)

      :ok = TestServer.push(server, directed_signal(":1.77"))
      :ok = TestServer.push(server, directed_signal(":1.42"))
      refute_receive {:matched, ^owner, ^ref, _message}, 100

      send(owner, :stop)
    end

    test "ignores a NameOwnerChanged a peer sent directly", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)

      :ok = answer_tracking(server, "org.example.Service", ":1.42")
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.42" end)

      # Same interface, member and body, but sent by a peer rather than the
      # bus driver, whose sender header a peer cannot forge.
      :ok =
        TestServer.push(
          server,
          name_owner_changed("org.example.Service", ":1.42", ":1.99",
            sender: ":1.99",
            destination: ":1.100"
          )
        )

      :ok = TestServer.push(server, directed_signal(":1.42"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000
      assert name_owner(connection, "org.example.Service") == ":1.42"

      :ok = TestServer.push(server, directed_signal(":1.99"))
      refute_receive {:matched, ^owner, ^ref, _message}, 100

      send(owner, :stop)
    end

    test "rejects directed signals while the tracked name has no owner", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)

      :ok = answer_tracking(server, "org.example.Service", nil)
      assert wait_until(fn -> tracking_idle?(connection) end)
      assert tracked?(connection, "org.example.Service")
      assert name_owner(connection, "org.example.Service") == nil

      :ok = TestServer.push(server, directed_signal(":1.42"))
      :ok = TestServer.push(server, directed_signal(":1.99"))
      refute_receive {:matched, ^owner, ^ref, _message}, 100

      :ok = TestServer.push(server, name_owner_changed("org.example.Service", "", ":1.42"))
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.42" end)

      :ok = TestServer.push(server, directed_signal(":1.42"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

      send(owner, :stop)
    end

    test "keeps the owner a signal reported over a later GetNameOwner reply", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)

      tracking_add = assert_add_match(server, tracking_rule())
      :ok = TestServer.push(server, method_return(tracking_add.serial))
      get = assert_get_name_owner(server, "org.example.Service")

      # The change arrives first, so the reply describing the older state must
      # not overwrite it.
      :ok = TestServer.push(server, name_owner_changed("org.example.Service", "", ":1.77"))
      assert wait_until(fn -> name_owner(connection, "org.example.Service") == ":1.77" end)

      :ok = TestServer.push(server, name_owner_reply(get.serial, ":1.42"))
      :ok = TestServer.push(server, directed_signal(":1.42"))
      refute_receive {:matched, ^owner, ^ref, _message}, 100
      assert name_owner(connection, "org.example.Service") == ":1.77"

      :ok = TestServer.push(server, directed_signal(":1.77"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

      send(owner, :stop)
    end

    test "tracks one well-known sender once for two rules", %{
      server: server,
      connection: connection
    } do
      first_rule = test_rule("Changed")
      second_rule = test_rule("Altered")

      first = subscribe_owner(self(), connection, first_rule)
      first_add = assert_add_match(server, first_rule)
      :ok = TestServer.push(server, method_return(first_add.serial))
      _first_ref = assert_subscription(first)
      :ok = answer_tracking(server, "org.example.Service", ":1.42")

      second = subscribe_owner(self(), connection, second_rule)
      second_add = assert_add_match(server, second_rule)
      :ok = TestServer.push(server, method_return(second_add.serial))
      _second_ref = assert_subscription(second)

      # The second rule needs the same name, which is already tracked.
      refute_receive {^server, %Message{header_fields: %{member: "GetNameOwner"}}}, 100

      send(first, :stop)
      first_remove = assert_remove_match(server, first_rule)
      :ok = TestServer.push(server, method_return(first_remove.serial))
      assert wait_until(fn -> rule_status(connection, first_rule) == nil end)

      # The second rule still needs the name, so the tracking rule stays.
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 100
      assert name_owner(connection, "org.example.Service") == ":1.42"

      send(second, :stop)
      second_remove = assert_remove_match(server, second_rule)
      :ok = TestServer.push(server, method_return(second_remove.serial))

      untrack = assert_remove_match(server, tracking_rule())
      :ok = TestServer.push(server, method_return(untrack.serial))
      assert wait_until(fn -> not tracked?(connection, "org.example.Service") end)
    end

    test "keeps a subscription when owner tracking fails", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()

      log =
        capture_log(fn ->
          owner = subscribe_owner(self(), connection, rule)
          add = assert_add_match(server, rule)
          :ok = TestServer.push(server, method_return(add.serial))
          ref = assert_subscription(owner)

          tracking_add = assert_add_match(server, tracking_rule())

          :ok =
            TestServer.push(
              server,
              bus_error(tracking_add.serial, "org.freedesktop.DBus.Error.AccessDenied")
            )

          assert wait_until(fn -> tracking_idle?(connection) end)

          # The caller kept its reference and its broadcast delivery; only
          # directed delivery for the name is off, with the name left unseeded.
          assert name_owner(connection, "org.example.Service") == :unknown
          :ok = TestServer.push(server, test_signal("changed"))
          assert_receive {:matched, ^owner, ^ref, %Message{body: ["changed"]}}, 1_000

          :ok = TestServer.push(server, directed_signal(":1.42"))
          refute_receive {:matched, ^owner, ^ref, _message}, 100

          send(owner, :stop)
        end)

      assert log =~ "D-Bus name owner tracking failed name=org.example.Service step=add_match"
    end

    test "delivers a directed bus-driver signal with an exact sender", %{
      server: server,
      connection: connection
    } do
      rule =
        MatchRule.new!(
          sender: "org.freedesktop.DBus",
          interface: "org.freedesktop.DBus",
          member: "NameLost",
          path: "/org/freedesktop/DBus"
        )

      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)

      signal =
        Message.new!(:signal,
          sender: "org.freedesktop.DBus",
          destination: ":1.100",
          path: "/org/freedesktop/DBus",
          interface: "org.freedesktop.DBus",
          member: "NameLost",
          signature: "s",
          body: [":1.100"]
        )

      assert MatchRule.matches?(rule, signal)
      :ok = TestServer.push(server, signal)
      assert_receive {:matched, ^owner, ^ref, %Message{body: [":1.100"]}}, 1_000

      send(owner, :stop)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
    end

    test "rejects a broad overlapping rule that would bypass a well-known sender", %{
      server: server,
      connection: connection
    } do
      sender_rule = test_rule()
      owner = subscribe_owner(self(), connection, sender_rule)
      add = assert_add_match(server, sender_rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      broad_rule =
        MatchRule.new!(
          interface: "org.example.Interface",
          member: "Changed",
          path_namespace: "/org/example",
          args: %{0 => "changed"}
        )

      assert {:error, :sender_routing_ambiguous} = Rebus.add_match(connection, broad_rule, 1_000)
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100

      send(owner, :stop)
      remove = assert_remove_match(server, sender_rule)
      :ok = TestServer.push(server, method_return(remove.serial))
    end

    test "stops local delivery before retrying an ambiguous remove timeout", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      assert {:error, :timeout} = Rebus.remove_match(connection, ref, 10)
      assert_remove_match(server, rule)

      # The rule left `:active` above, so the name is no longer needed and the
      # tracking rule is withdrawn concurrently with the rule's own removal.
      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))

      :ok = TestServer.push(server, test_signal("changed"))
      refute_receive {:matched, ^owner, ^ref, %Message{}}, 200

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      retry = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(retry.serial))
      assert :ok = Task.await(removal, 1_000)
      send(owner, :stop)
    end

    test "returns only a bounded D-Bus error name when AddMatch is rejected", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      task = Task.async(fn -> Rebus.add_match(connection, rule, 1_000) end)
      add = assert_add_match(server, rule)
      error_name = "org.freedesktop.DBus.Error.AccessDenied"

      :ok =
        TestServer.push(
          server,
          Message.new!(:error,
            reply_serial: add.serial,
            error_name: error_name,
            signature: "s",
            body: ["policy details are intentionally not returned"]
          )
        )

      assert {:error, {:bus_error, ^error_name}} = Task.await(task, 1_000)
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 100
    end

    test "treats a missing final match rule as removed", %{server: server, connection: connection} do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)

      :ok =
        TestServer.push(
          server,
          Message.new!(:error,
            reply_serial: remove.serial,
            error_name: "org.freedesktop.DBus.Error.MatchRuleNotFound"
          )
        )

      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      send(owner, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "treats a dropped named AddMatch error as definitive", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      task = Task.async(fn -> Rebus.add_match(connection, rule, 1_000) end)
      add = assert_add_match(server, rule)
      error_name = "org.example.ResourceLimited"

      :ok = TestServer.push_raw(server, raw_resource_limited_error_reply(add.serial, error_name))

      assert {:error, {:reply_dropped, {:error, ^error_name}}} = Task.await(task, 1_000)
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 100
    end

    test "removes the final remote rule when its owner exits", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      send(owner, :stop)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))

      untrack = assert_remove_match(server, tracking_rule())
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "retries owner cleanup after a definitive D-Bus error", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)
      :ok = answer_tracking(server)
      worker = subscription_worker(connection)

      send(owner, :stop)
      remove = assert_remove_match(server, rule)

      # The rule left `:active` above, so the name is no longer needed and the
      # tracking rule is withdrawn concurrently with the rule's own removal.
      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))

      :ok =
        TestServer.push(
          server,
          Message.new!(:error,
            reply_serial: remove.serial,
            error_name: "org.freedesktop.DBus.Error.AccessDenied"
          )
        )

      retry = assert_remove_match(server, rule, 1_500)
      :ok = TestServer.push(server, method_return(retry.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      assert subscription_worker(connection) == worker
      assert Process.alive?(worker)
    end

    test "keeps the worker alive when owner down races a timed-out final removal", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)
      worker = subscription_worker(connection)

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 300) end)
      _remove = assert_remove_match(server, rule)
      send(owner, :stop)

      assert wait_until(fn -> rule_ref_count(connection, rule) == 0 end)
      assert {:error, :timeout} = Task.await(removal, 1_000)

      retry = assert_remove_match(server, rule, 1_500)
      :ok = TestServer.push(server, method_return(retry.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      assert subscription_worker(connection) == worker
      assert Process.alive?(worker)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "keeps the worker alive when owner down races a definitive final removal", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)
      worker = subscription_worker(connection)

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      send(owner, :stop)
      assert wait_until(fn -> rule_ref_count(connection, rule) == 0 end)

      error_name = "org.freedesktop.DBus.Error.AccessDenied"

      :ok =
        TestServer.push(
          server,
          Message.new!(:error, reply_serial: remove.serial, error_name: error_name)
        )

      assert {:error, {:bus_error, ^error_name}} = Task.await(removal, 1_000)

      retry = assert_remove_match(server, rule, 1_500)
      :ok = TestServer.push(server, method_return(retry.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      assert subscription_worker(connection) == worker
      assert Process.alive?(worker)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "cleans up a timed-out AddMatch before a later re-add", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      _first_add = assert_add_match(server, rule)
      assert_receive {:subscription_error, ^owner, :timeout}, 1_500

      cleanup = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(cleanup.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)

      replacement = subscribe_owner(self(), connection, rule)
      second_add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(second_add.serial))
      replacement_ref = assert_subscription(replacement)
      :ok = answer_tracking(server)

      removal = Task.async(fn -> Rebus.remove_match(connection, replacement_ref, 1_000) end)
      final_remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(final_remove.serial))
      assert :ok = Task.await(removal, 1_000)
      send(replacement, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "does not let a slow connection block a healthy connection", %{
      server: slow_server,
      connection: slow_connection
    } do
      {:ok, healthy_server} =
        start_supervised(%{
          id: {:healthy_test_server, make_ref()},
          start: {TestServer, :start_link, [[tap: self()]]}
        })

      {:ok, healthy_address} = TestServer.get_listen_addr(healthy_server)
      {:ok, healthy_connection} = Rebus.connect(healthy_address)
      assert_receive {^healthy_server, %Message{header_fields: %{member: "Hello"}}}, 1_000

      on_exit(fn ->
        _ = Rebus.close(healthy_connection)
      end)

      rule = test_rule()
      slow = Task.async(fn -> Rebus.add_match(slow_connection, rule, 250) end)
      _slow_add = assert_add_match(slow_server, rule)

      healthy = Task.async(fn -> Rebus.add_match(healthy_connection, rule, 500) end)
      healthy_add = assert_add_match(healthy_server, rule)
      :ok = TestServer.push(healthy_server, method_return(healthy_add.serial))
      assert {:ok, _ref} = Task.await(healthy, 500)
      :ok = answer_tracking(healthy_server)
      assert {:error, :timeout} = Task.await(slow, 1_000)

      slow_cleanup = assert_remove_match(slow_server, rule)
      :ok = TestServer.push(slow_server, method_return(slow_cleanup.serial))
    end

    test "retains a timed-out handler removal until ordered retry completes", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      :ok = :sys.suspend(connection)
      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 10) end)
      assert {:error, :timeout} = Task.await(removal, 100)
      :ok = :sys.resume(connection)

      assert wait_until(fn ->
               not Map.has_key?(:sys.get_state(connection).handlers, ref)
             end)

      :ok = TestServer.push(server, test_signal("changed"))
      refute_receive {:matched, ^owner, ^ref, %Message{}}, 200

      retry = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(retry, 1_000)
      send(owner, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "cleans up many dead owners with one final RemoveMatch", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      first = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _first_ref = assert_subscription(first)
      :ok = answer_tracking(server)

      owners = [
        first | Enum.map(1..11, fn _index -> subscribe_owner(self(), connection, rule) end)
      ]

      Enum.each(owners -- [first], &assert_subscription/1)
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100

      Enum.each(owners, &send(&1, :stop))
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)

      # The last rule for the tracked sender took the tracking rule with it.
      untrack = assert_remove_match(server, tracking_rule())
      :ok = TestServer.push(server, method_return(untrack.serial))
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 100
    end

    test "queues more than 64 ordinary owner-exit cleanups without resetting", %{
      server: server,
      connection: connection
    } do
      previous = Application.get_env(:rebus, :match_recovery_max_rules)
      Application.put_env(:rebus, :match_recovery_max_rules, 1)

      on_exit(fn ->
        if is_nil(previous),
          do: Application.delete_env(:rebus, :match_recovery_max_rules),
          else: Application.put_env(:rebus, :match_recovery_max_rules, previous)
      end)

      subscriptions =
        Enum.map(1..65, fn index ->
          rule = test_rule("Changed#{index}")
          owner = subscribe_owner(self(), connection, rule)
          add = assert_add_match(server, rule)
          :ok = TestServer.push(server, method_return(add.serial))
          _ref = assert_subscription(owner)
          {owner, rule}
        end)

      :ok = answer_tracking(server)

      Enum.each(subscriptions, fn {owner, _rule} -> send(owner, :stop) end)

      Enum.each(1..65, fn _index ->
        remove = assert_any_remove_match(server, 1_500)
        :ok = TestServer.push(server, method_return(remove.serial))
      end)

      assert Process.alive?(connection)

      assert wait_until(fn ->
               Enum.all?(subscriptions, fn {_owner, rule} ->
                 rule_status(connection, rule) == nil
               end)
             end)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "persists only stable rule and reference rows across a worker restart", %{
      server: server,
      connection: connection
    } do
      subscriptions =
        Enum.map(1..20, fn index ->
          rule = test_rule("Changed#{index}")
          owner = subscribe_owner(self(), connection, rule)
          add = assert_add_match(server, rule)
          :ok = TestServer.push(server, method_return(add.serial))
          ref = assert_subscription(owner)
          {owner, rule, ref}
        end)

      :ok = answer_tracking(server)

      assert {:ok, %{uncertain?: false, rules: rules, subscriptions: refs}} =
               Store.load_state(connection)

      assert map_size(rules) == 20
      assert map_size(refs) == 20

      worker = subscription_worker(connection)
      kill_worker(worker)

      assert wait_until(fn ->
               case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
                 [{replacement, _value}] -> replacement != worker and Process.alive?(replacement)
                 [] -> false
               end
             end)

      # The restarted worker restores the rules and tracks the name again.
      :ok = answer_tracking(server)

      Enum.each(subscriptions, fn {owner, _rule, ref} ->
        send(owner, :stop)
        assert is_reference(ref)
      end)

      Enum.each(1..20, fn _index ->
        remove = assert_any_remove_match(server, 1_500)
        :ok = TestServer.push(server, method_return(remove.serial))
      end)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "clears a restored cleaning rule before serving a new add for it", %{
      server: server,
      connection: connection
    } do
      # No well-known sender, so nothing here waits on owner tracking. The row
      # is written directly because a worker only ever persists a cleaning rule
      # alongside the operation cleaning it, and that snapshot is uncertain: the
      # stable snapshot this restores from is what a worker leaves behind once
      # the operation-in-flight window closes.
      rule = MatchRule.new!(interface: "org.example.Interface", member: "Restarted")
      key = MatchRule.to_string(rule)

      :ok =
        Store.persist_state(
          connection,
          false,
          persistence_changes(key),
          %{key => cleaning_row(rule)},
          %{}
        )

      adding = Task.async(fn -> Rebus.add_match(connection, rule, 2_000) end)

      # Starting the worker restores the rule and re-runs the cleanup it owed.
      # Without that the add would queue behind a cleanup that never runs.
      remove = assert_remove_match(server, rule, 1_500)
      :ok = TestServer.push(server, method_return(remove.serial))

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))

      assert {:ok, ref} = Task.await(adding, 2_000)

      # The adding task owns the subscription and exits as soon as it has the
      # reference, so the worker races its own owner-DOWN cleanup against this
      # removal. Whichever wins, exactly one RemoveMatch reaches the bus and the
      # final removal only replies once that reply arrives, so answer it before
      # awaiting rather than calling synchronously.
      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      final_remove = assert_remove_match(server, rule, 1_500)
      :ok = TestServer.push(server, method_return(final_remove.serial))
      assert :ok = Task.await(removal, 2_000)
    end

    test "retries ambiguous owner cleanup until RemoveMatch is known", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      send(owner, :stop)
      _first_remove = assert_remove_match(server, rule)

      # The rule left `:active` above, so the name is no longer needed and the
      # tracking rule is withdrawn concurrently with the rule's own removal.
      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))

      # The unanswered RemoveMatch cannot prove what the bus did, so the rule
      # graduates from its one best-effort cleanup into bounded recovery.
      assert wait_until(fn -> rule_status(connection, rule) == :cleaning end)
      retry = assert_remove_match(server, rule, 1_500)
      assert wait_until(fn -> rule_status(connection, rule) == :recovering end)
      :ok = TestServer.push(server, method_return(retry.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      assert Process.alive?(connection)
    end

    test "does not let a slow rule block a different rule on the same connection", %{
      server: server,
      connection: connection
    } do
      slow_rule = test_rule()
      healthy_rule = test_rule("Other")

      slow = Task.async(fn -> Rebus.add_match(connection, slow_rule, 250) end)
      _slow_add = assert_add_match(server, slow_rule)

      healthy = subscribe_owner(self(), connection, healthy_rule)
      healthy_add = assert_add_match(server, healthy_rule)
      :ok = TestServer.push(server, method_return(healthy_add.serial))
      healthy_ref = assert_subscription(healthy)
      :ok = answer_tracking(server)
      assert {:error, :timeout} = Task.await(slow, 1_000)

      slow_cleanup = assert_remove_match(server, slow_rule)
      :ok = TestServer.push(server, method_return(slow_cleanup.serial))

      removal = Task.async(fn -> Rebus.remove_match(connection, healthy_ref, 1_000) end)
      healthy_remove = assert_remove_match(server, healthy_rule)
      :ok = TestServer.push(server, method_return(healthy_remove.serial))
      assert :ok = Task.await(removal, 1_000)
      send(healthy, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "handles owner death while another rule operation is slow", %{
      server: server,
      connection: connection
    } do
      slow_rule = test_rule()
      healthy_rule = test_rule("Other")
      owner = subscribe_owner(self(), connection, slow_rule)

      _slow_add = assert_add_match(server, slow_rule)
      send(owner, :stop)

      healthy = subscribe_owner(self(), connection, healthy_rule)
      healthy_add = assert_add_match(server, healthy_rule)
      :ok = TestServer.push(server, method_return(healthy_add.serial))
      healthy_ref = assert_subscription(healthy)
      :ok = answer_tracking(server)

      slow_cleanup = assert_remove_match(server, slow_rule, 1_500)
      :ok = TestServer.push(server, method_return(slow_cleanup.serial))

      removal = Task.async(fn -> Rebus.remove_match(connection, healthy_ref, 1_000) end)
      healthy_remove = assert_remove_match(server, healthy_rule)
      :ok = TestServer.push(server, method_return(healthy_remove.serial))
      assert :ok = Task.await(removal, 1_000)
      send(healthy, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "does not commit an AddMatch result after the caller deadline", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      add_task = Task.async(fn -> Rebus.add_match(connection, rule, 100) end)
      add = assert_add_match(server, rule)
      worker = subscription_worker(connection)

      :ok = :sys.suspend(worker)
      :ok = TestServer.push(server, method_return(add.serial))
      Process.sleep(120)
      :ok = :sys.resume(worker)

      assert {:error, :timeout} = Task.await(add_task, 500)
      cleanup = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(cleanup.serial))

      assert wait_until(fn ->
               :sys.get_state(connection).handlers == %{}
             end)

      assert wait_until(fn -> rule_status(connection, rule) == nil end)
    end

    test "queues a same-rule add while ambiguous cleanup is in progress", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      first = Task.async(fn -> Rebus.add_match(connection, rule, 50) end)
      _first_add = assert_add_match(server, rule)
      assert {:error, :timeout} = Task.await(first, 500)

      cleanup = assert_remove_match(server, rule)
      replacement = Task.async(fn -> Rebus.add_match(connection, rule, 1_000) end)
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100

      :ok = TestServer.push(server, method_return(cleanup.serial))
      replacement_add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(replacement_add.serial))
      assert {:ok, replacement_ref} = Task.await(replacement, 1_000)
      :ok = answer_tracking(server)

      removal = Task.async(fn -> Rebus.remove_match(connection, replacement_ref, 1_000) end)
      final_remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(final_remove.serial))
      assert :ok = Task.await(removal, 1_000)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "restarts a crashed idle worker and accepts a new subscription", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      initial_add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(initial_add.serial))
      initial_ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      initial_remove = Task.async(fn -> Rebus.remove_match(connection, initial_ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(initial_remove, 1_000)
      send(owner, :stop)

      untrack = assert_remove_match(server, tracking_rule())
      :ok = TestServer.push(server, method_return(untrack.serial))
      assert wait_until(fn -> not Store.persisted?(connection) end)

      worker = subscription_worker(connection)
      kill_worker(worker)

      assert wait_until(fn ->
               case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
                 [{replacement, _value}] -> replacement != worker and Process.alive?(replacement)
                 [] -> false
               end
             end)

      replacement = Task.async(fn -> Rebus.add_match(connection, rule, 1_000) end)
      replacement_add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(replacement_add.serial))
      assert {:ok, replacement_ref} = Task.await(replacement, 1_000)
      :ok = answer_tracking(server)

      final_remove = Task.async(fn -> Rebus.remove_match(connection, replacement_ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(final_remove, 1_000)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "restores a live subscription after worker restart instead of accepting a lost ref", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      worker = subscription_worker(connection)
      kill_worker(worker)

      assert wait_until(fn ->
               case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
                 [{replacement, _value}] -> replacement != worker and Process.alive?(replacement)
                 [] -> false
               end
             end)

      # The restarted worker restores the rule and tracks the name again.
      :ok = answer_tracking(server)

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      send(owner, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "keeps persisted rows when the match-subscription supervisor restarts", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      table = :ets.whereis(state_table())
      table_owner = :ets.info(table, :owner)
      size = :ets.info(table, :size)
      assert size > 0
      assert {:ok, persisted} = Store.load_state(connection)

      worker = subscription_worker(connection)
      supervisor = Process.whereis(Rebus.MatchSubscription)

      kill_and_await_restart(supervisor, [worker], [{Rebus.MatchSubscription, supervisor}])

      # Same table, same owner, same rows: the worker supervisor no longer
      # takes the persisted state down with it.
      assert :ets.whereis(state_table()) == table
      assert :ets.info(table, :owner) == table_owner
      assert table_owner == Process.whereis(Store)
      assert :ets.info(table, :size) == size
      assert Store.load_state(connection) == {:ok, persisted}

      # A worker started afterwards restores them, so the reference is still
      # honoured and the bus rule is removed rather than left behind. That
      # worker also tracks the name again.
      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      :ok = answer_tracking(server)
      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> not Store.persisted?(connection) end)
      send(owner, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "keeps an active subscription usable across a task-supervisor restart", %{
      server: server,
      connection: connection
    } do
      # No `arg0` criterion, so each signal below can carry its own body.
      rule =
        MatchRule.new!(
          sender: "org.example.Service",
          interface: "org.example.Interface",
          member: "Changed",
          path_namespace: "/org/example"
        )

      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      :ok = TestServer.push(server, test_signal("before the restart"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["before the restart"]}}, 1_000

      table = :ets.whereis(state_table())
      size = :ets.info(table, :size)
      worker = subscription_worker(connection)
      task_supervisor = Process.whereis(Rebus.MatchSubscription.TaskSupervisor)
      worker_supervisor = Process.whereis(Rebus.MatchSubscription)

      # `rest_for_one` restarts the worker supervisor behind the task
      # supervisor, so the worker goes with it.
      kill_and_await_restart(task_supervisor, [worker], [
        {Rebus.MatchSubscription.TaskSupervisor, task_supervisor},
        {Rebus.MatchSubscription, worker_supervisor}
      ])

      assert :ets.whereis(state_table()) == table
      assert :ets.info(table, :size) == size

      :ok = TestServer.push(server, test_signal("after the restart"))
      assert_receive {:matched, ^owner, ^ref, %Message{body: ["after the restart"]}}, 1_000

      # The removal starts the worker the supervisor restart left unstarted:
      # it restores the rule and tracks the name again before removing it.
      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      :ok = answer_tracking(server)
      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> not Store.persisted?(connection) end)
      send(owner, :stop)

      untrack = assert_remove_match(server, tracking_rule(), 1_500)
      :ok = TestServer.push(server, method_return(untrack.serial))
    end

    test "reaps persisted rows for a connection that dies with no worker", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)
      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      assert Store.persisted?(connection)
      assert persisted_rows(connection, :rule) != []
      assert persisted_rows(connection, :subscription) != []

      worker = subscription_worker(connection)
      supervisor = Process.whereis(Rebus.MatchSubscription)

      kill_and_await_restart(supervisor, [worker], [{Rebus.MatchSubscription, supervisor}])

      # Nothing watches the connection now: its worker died with the
      # supervisor and none is started until the next call.
      assert wait_until(fn ->
               Registry.lookup(Rebus.MatchSubscription.Registry, connection) == []
             end)

      assert Store.persisted?(connection)

      :ok = Rebus.close(connection)

      # `delete_state/1` drops the meta row first, so wait for all three
      # rather than letting the later assertions race the rest of the delete.
      assert wait_until(fn -> reaped?(connection) end)
      refute :ets.member(state_table(), {:meta, connection})
      assert persisted_rows(connection, :rule) == []
      assert persisted_rows(connection, :subscription) == []

      send(owner, :stop)
    end

    test "resets a supervisor-owned connection when subscription state is lost", %{
      connection: connection
    } do
      {:ok, worker} = Worker.start_link(connection)

      send(worker, :reset_state_lost)

      assert wait_until(fn -> not Process.alive?(connection) end)
      assert wait_until(fn -> not Process.alive?(worker) end)
      refute Store.persisted?(connection)
    end

    test "keeps state loss explicit for a directly started connection" do
      # ExUnit owns the server's lifecycle: stopping it from `on_exit` races
      # the shutdown a linked server starts when the test process exits. The
      # id must be unique because the describe setup supervises a server too.
      {:ok, server} =
        start_supervised(%{
          id: {:direct_test_server, make_ref()},
          start: {TestServer, :start_link, [[tap: self()]]}
        })

      {direct_connection, worker} = start_direct_connection(server)

      on_exit(fn ->
        stop_worker(worker)
        if Process.alive?(direct_connection), do: Process.exit(direct_connection, :shutdown)
        _ = Store.delete_state(direct_connection)
      end)

      send(worker, :reset_state_lost)

      assert wait_until(fn ->
               state = :sys.get_state(worker)
               state.state_lost? and not state.resetting?
             end)

      assert Process.alive?(direct_connection)

      assert {:error, :match_subscription_state_lost} =
               Rebus.remove_match(direct_connection, make_ref(), 100)
    end

    test "rehydrates persisted removal state and prunes missing rows" do
      conn = start_placeholder_conn()
      stale_key = "stale-rule"

      on_exit(fn ->
        case Registry.lookup(Rebus.MatchSubscription.Registry, conn) do
          [{worker, _value}] when is_pid(worker) -> stop_worker(worker)
          [] -> :ok
        end
      end)

      :ok =
        Store.persist_state(
          conn,
          false,
          %{persistence_changes(stale_key) | dirty_rules: MapSet.new([stale_key])},
          %{},
          %{}
        )

      assert Store.persisted?(conn)
      assert :ok = Rebus.remove_match(conn, make_ref(), 100)
    end

    test "covers reset guard transitions without a live bus" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)
      rule = test_rule()
      deadline = System.monotonic_time(:millisecond) + 1_000
      from = {self(), make_ref()}

      lost = %{state | state_lost?: true}

      assert {:reply, {:error, :match_subscription_state_lost}, ^lost} =
               Worker.handle_call({:add, self(), rule, deadline}, from, lost)

      assert {:reply, {:error, :match_subscription_state_lost}, ^lost} =
               Worker.handle_call({:remove, make_ref(), deadline}, from, lost)

      assert {:reply, {:error, :timeout}, ^state} =
               Worker.handle_call({:add, self(), rule, deadline - 2_000}, from, state)

      key = MatchRule.to_string(rule)
      full = put_rule_state(state, key, rule, List.duplicate(make_ref(), 64))

      assert {:reply, {:error, :match_rule_cleanup_pending}, ^full} =
               Worker.handle_call({:add, self(), rule, deadline}, from, full)

      ref = make_ref()
      full = %{full | subscriptions: %{ref => %{key: key}}}

      assert {:reply, {:error, :timeout}, ^full} =
               Worker.handle_call({:remove, ref, deadline - 2_000}, from, full)

      assert {:reply, {:error, :match_rule_cleanup_pending}, ^full} =
               Worker.handle_call({:remove, ref, deadline}, from, full)
    end

    test "covers stale, failed, and completed reset notifications" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)

      assert {:noreply, ^state} =
               Worker.handle_info({:connection_reset_result, make_ref(), :ok}, state)

      monitor = Process.monitor(conn)
      token = make_ref()
      resetting = %{state | reset_token: token, reset_task_monitor: monitor, resetting?: true}

      assert {:noreply, completed} =
               Worker.handle_info({:connection_reset_result, token, :ok}, resetting)

      # The reset was requested but the connection has not gone down yet, so
      # the latch stays up while the monitored task is released.
      assert completed.state_lost?
      assert completed.resetting?
      assert is_nil(completed.reset_token)
      assert is_nil(completed.reset_task_monitor)

      monitor = Process.monitor(conn)
      token = make_ref()
      resetting = %{state | reset_token: token, reset_task_monitor: monitor, resetting?: true}

      assert {:noreply, failed} =
               Worker.handle_info(
                 {:connection_reset_result, token, {:error, :not_connection}},
                 resetting
               )

      # Nothing was closed, so the latch is dropped and the loss made explicit
      # rather than left to a reset that will never arrive.
      assert failed.state_lost?
      refute failed.resetting?
      assert is_nil(failed.reset_token)
      assert is_nil(failed.reset_task_monitor)
    end

    test "covers stale worker events and reset task loss" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)

      assert {:noreply, ^state} = Worker.handle_info({:resume_recovery, "missing"}, state)
      assert {:noreply, ^state} = Worker.handle_info({:retry_recovery, "missing"}, state)

      assert {:noreply, ^state} =
               Worker.handle_info({:operation_result, make_ref(), :ignored}, state)

      assert {:noreply, ^state} = Worker.handle_info({:request_timeout, make_ref()}, state)

      assert {:noreply, ^state} =
               Worker.handle_info({:DOWN, make_ref(), :process, conn, :normal}, state)

      monitor = Process.monitor(conn)
      resetting = %{state | reset_task_monitor: monitor, resetting?: true}

      assert {:noreply, lost} =
               Worker.handle_info({:DOWN, monitor, :process, conn, :normal}, resetting)

      assert lost.state_lost?
      refute lost.resetting?
      assert is_nil(lost.reset_task_monitor)
      assert is_nil(lost.reset_token)
    end

    test "covers failed add, reuse, and removal operation completions" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      bus_error = {:error, {:bus_error, "denied"}}

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_new)
      {state, request_id, tag} = live_request(state, key, :add)
      state = attach_request(state, token, request_id)

      assert {:noreply, add_failed} =
               Worker.handle_info(
                 {:operation_result, token, {:add_failed, bus_error, nil}},
                 state
               )

      # A definitive AddMatch error that installed no local handler leaves
      # nothing behind to recover: the caller is answered and the rule that was
      # only ever installing is dropped along with its persisted row.
      assert_receive {^tag, ^bus_error}
      assert add_failed.requests == %{}
      assert add_failed.request_monitors == %{}
      assert add_failed.operations == %{}
      refute Map.has_key?(add_failed.rules, key)
      assert add_failed.recovering_rules == MapSet.new()
      refute Store.persisted?(conn)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)
      {state, request_id, tag} = live_request(state, key, :add)
      state = attach_request(state, token, request_id)

      assert {:noreply, reuse_failed} =
               Worker.handle_info(
                 {:operation_result, token, {:add_existing_failed, {:error, :disconnected}}},
                 state
               )

      # Reusing an existing rule owns no bus state, so a failure answers the
      # caller and dispatches the rule onwards rather than entering recovery.
      assert_receive {^tag, {:error, :disconnected}}
      assert reuse_failed.requests == %{}
      assert reuse_failed.operations == %{}
      refute Map.has_key?(reuse_failed.rules, key)
      assert reuse_failed.recovering_rules == MapSet.new()
      refute Store.persisted?(conn)

      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)
      {state, request_id, tag} = live_request(state, key, :remove, ref: ref)
      state = attach_request(state, token, request_id)

      assert {:noreply, remove_failed} =
               Worker.handle_info(
                 {:operation_result, token, {:remove_failed, {:error, :not_connected}, :active}},
                 state
               )

      # The local handler is still installed, so the operation's own error is
      # reported verbatim and the still-referenced rule survives, idle, for the
      # caller to retry against.
      assert_receive {^tag, {:error, :not_connected}}
      assert remove_failed.requests == %{}
      assert remove_failed.operations == %{}
      assert %{status: :active, operation: nil, queue: []} = remove_failed.rules[key]
      assert MapSet.equal?(remove_failed.rules[key].refs, MapSet.new([ref]))
      assert remove_failed.recovering_rules == MapSet.new()
      assert remove_failed.subscriptions[ref].handler == :active

      assert {:ok, %{uncertain?: false, rules: %{^key => %{status: :active}}}} =
               Store.load_state(conn)
    end

    test "keeps an installing rule's state when its operation never starts" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_new)
      {state, request_id, tag} = live_request(state, key, :add)
      state = attach_request(state, token, request_id)

      log =
        capture_log(fn ->
          assert {:noreply, alone} =
                   Worker.handle_info({:operation_result, token, :not_started}, state)

          # The task never ran, so no AddMatch was sent and no handler was
          # installed: the rule created for it is simply dropped, the caller is
          # told the operation never started, and the connection is left alone.
          assert_receive {^tag, {:error, :not_started}}
          refute Map.has_key?(alone.rules, key)
          assert alone.operations == %{}
          assert alone.requests == %{}
          assert alone.recovering_rules == MapSet.new()
          refute alone.resetting?
          refute alone.state_lost?
          assert is_nil(alone.reset_token)
          assert Process.alive?(conn)
          refute Store.persisted?(conn)
        end)

      assert log =~ "D-Bus match operation not started type=add_new"

      :ok = Store.delete_state(conn)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_new)
      {state, request_id, tag} = live_request(state, key, :add)
      state = attach_request(state, token, request_id)
      {state, queued_id, _queued_tag} = live_request(state, key, :add)
      state = put_in(state.rules[key].queue, [queued_id])

      capture_log(fn ->
        assert {:noreply, queued} =
                 Worker.handle_info({:operation_result, token, :not_started}, state)

        # A request waiting behind the failed one is not collateral damage: the
        # rule survives for it and it starts an operation of its own.
        assert_receive {^tag, {:error, :not_started}}
        assert %{status: :installing, queue: [], operation: operation} = queued.rules[key]
        assert is_reference(operation)
        assert %{key: ^key, type: :add_new, request_id: ^queued_id} = queued.operations[operation]
        refute queued.resetting?
      end)
    end

    test "keeps an active rule's references when a reuse or removal never starts" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :add_existing, status: :active)

      state = put_subscription_state(state, key, ref)
      {state, request_id, tag} = live_request(state, key, :add)
      state = attach_request(state, token, request_id)

      log =
        capture_log(fn ->
          assert {:noreply, reuse} =
                   Worker.handle_info({:operation_result, token, :not_started}, state)

          # Nothing was added, so the rule keeps exactly the references it had.
          assert_receive {^tag, {:error, :not_started}}
          assert %{status: :active, operation: nil, queue: []} = reuse.rules[key]
          assert MapSet.equal?(reuse.rules[key].refs, MapSet.new([ref]))
          assert reuse.subscriptions[ref].handler == :active
          assert reuse.recovering_rules == MapSet.new()
          refute reuse.resetting?
          refute reuse.state_lost?
          assert Process.alive?(conn)
        end)

      assert log =~ "D-Bus match operation not started type=add_existing"

      :ok = Store.delete_state(conn)
      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)
      {state, request_id, tag} = live_request(state, key, :remove, ref: ref)
      state = attach_request(state, token, request_id)

      log =
        capture_log(fn ->
          assert {:noreply, removal} =
                   Worker.handle_info({:operation_result, token, :not_started}, state)

          # No RemoveMatch was sent and no handler was detached, so the
          # caller's reference is still good and worth retrying with.
          assert_receive {^tag, {:error, :not_started}}
          assert %{status: :active, operation: nil, queue: []} = removal.rules[key]
          assert MapSet.equal?(removal.rules[key].refs, MapSet.new([ref]))
          assert removal.subscriptions[ref].handler == :active
          assert removal.recovering_rules == MapSet.new()
          refute removal.resetting?
          refute removal.state_lost?
          assert Process.alive?(conn)

          assert {:ok, %{uncertain?: false, rules: %{^key => %{status: :active}}}} =
                   Store.load_state(conn)
        end)

      assert log =~ "D-Bus match operation not started type=remove"
    end

    test "retries a recovery whose operation never starts" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      handler_ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      state = put_in(state.rules[key].pending_handlers, MapSet.new([handler_ref]))
      state = put_in(state.rules[key].remote_may_exist?, true)

      log =
        capture_log(fn ->
          assert {:noreply, retrying} =
                   Worker.handle_info({:operation_result, token, :not_started}, state)

          # Nothing was cleared, so the attempt is simply owed again: the rule
          # stays in recovery with its handler removals still pending, and the
          # connection is not reset over an outcome that never happened.
          assert %{status: :recovering, recovery_attempt: 1, operation: nil} = retrying.rules[key]
          assert is_reference(retrying.rules[key].retry_timer)
          assert MapSet.equal?(retrying.rules[key].pending_handlers, MapSet.new([handler_ref]))
          assert retrying.rules[key].remote_may_exist?
          assert retrying.operations == %{}
          refute retrying.resetting?
          refute retrying.state_lost?
          assert Process.alive?(conn)
        end)

      refute log =~ "D-Bus match operation not started"
    end

    test "releases the slot when an initial cleanup never starts" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      waiting_rule =
        MatchRule.new!(interface: "org.example.Waiting", member: "Changed")

      waiting_key = MatchRule.to_string(waiting_rule)

      {state, token} = cleaning_state(conn, key, rule)

      waiting = %{state.rules[key] | rule: waiting_rule, operation: nil, remote_may_exist?: true}

      state = %{
        state
        | rules: Map.put(state.rules, waiting_key, waiting),
          initial_cleanup_queue: :queue.in(waiting_key, state.initial_cleanup_queue)
      }

      log =
        capture_log(fn ->
          assert {:noreply, recovering} =
                   Worker.handle_info({:operation_result, token, :not_started}, state)

          # The RemoveMatch is still owed, so the rule graduates to the bounded
          # recovery set rather than being forgotten, and gives its
          # initial-cleanup slot to the key that was waiting for one.
          assert %{status: :recovering, recovery_kind: :rule} = recovering.rules[key]
          assert MapSet.member?(recovering.recovering_rules, key)
          refute MapSet.member?(recovering.initial_cleanup_keys, key)

          assert %{status: :cleaning, operation: waiting_operation} =
                   recovering.rules[waiting_key]

          assert is_reference(waiting_operation)
          assert MapSet.member?(recovering.initial_cleanup_keys, waiting_key)
          assert :queue.is_empty(recovering.initial_cleanup_queue)
          refute recovering.resetting?
          refute recovering.state_lost?
          assert Process.alive?(conn)
        end)

      refute log =~ "D-Bus match operation not started"
    end

    test "answers the caller when the operation task cannot be started at all" do
      conn = start_placeholder_conn()
      {:ok, supervisor} = Task.Supervisor.start_link(max_children: 0)
      rule = test_rule()
      key = MatchRule.to_string(rule)
      tag = make_ref()
      deadline = System.monotonic_time(:millisecond) + 10_000

      state = %{fresh_worker_state(conn) | bus?: true, task_supervisor: supervisor}

      capture_log(fn ->
        assert {:noreply, dispatched} =
                 Worker.handle_call({:add, self(), rule, deadline}, {self(), tag}, state)

        # A supervisor that will not start children makes the real
        # `start_child/2` failure, and the synthesised result travels the
        # ordinary operation path from there.
        assert [token] = Map.keys(dispatched.operations)
        assert_received {:operation_result, ^token, :not_started}

        assert {:noreply, answered} =
                 Worker.handle_info({:operation_result, token, :not_started}, dispatched)

        assert_receive {^tag, {:error, :not_started}}
        refute Map.has_key?(answered.rules, key)
        assert answered.operations == %{}
        refute answered.resetting?
        refute answered.state_lost?
        assert Process.alive?(conn)
      end)
    end

    test "still resets the connection when a removal task dies mid-flight" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)
      {state, request_id, tag} = live_request(state, key, :remove, ref: ref)
      state = attach_request(state, token, request_id)

      capture_log(fn ->
        assert {:noreply, reset} =
                 Worker.handle_info(
                   {:operation_result, token, {:operation_failed, :disconnected}},
                   state
                 )

        # A task that died in flight may have sent the RemoveMatch, so this
        # path is unchanged: the outcome is unknown and the connection goes.
        assert_receive {^tag, {:error, :disconnected}}
        assert reset.resetting?
        assert reset.state_lost?
        assert is_reference(reset.reset_token)
      end)
    end

    test "covers recovery result classifications and stale operation cleanup" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      {state, token} = operation_state(state, key, rule, :recovery, status: :recovering)

      assert {:noreply, retrying} =
               Worker.handle_info(
                 {:operation_result, token,
                  {:definitive_bus_error, {:error, {:bus_error, "denied"}}}},
                 state
               )

      assert %{status: :recovering, retry_timer: retry_timer} = retrying.rules[key]
      assert is_reference(retry_timer)
      assert %{recovery_attempt: 1, operation: nil} = retrying.rules[key]
      assert retrying.operations == %{}

      # A rule waiting on its next attempt is stable enough to persist: only an
      # in-flight request or operation makes the snapshot uncertain.
      assert {:ok, %{uncertain?: false, rules: %{^key => %{status: :recovering}}}} =
               Store.load_state(conn)

      :ok = Store.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      handler_ref = make_ref()
      state = put_in(state.rules[key].pending_handlers, MapSet.new([handler_ref]))

      assert {:noreply, handler_retry} =
               Worker.handle_info({:operation_result, token, {:retry, :handlers}}, state)

      # Handlers that could not be removed are still owed a removal, so they
      # survive into the next attempt.
      assert %{status: :recovering, recovery_attempt: 1, pending_handlers: pending} =
               handler_retry.rules[key]

      assert MapSet.equal?(pending, MapSet.new([handler_ref]))
      assert is_reference(handler_retry.rules[key].retry_timer)

      assert {:ok, %{uncertain?: false, rules: %{^key => %{recovery_attempt: 1}}}} =
               Store.load_state(conn)

      :ok = Store.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      state = put_in(state.rules[key].pending_handlers, MapSet.new([make_ref()]))

      assert {:noreply, remote_retry} =
               Worker.handle_info({:operation_result, token, {:retry, :remote}}, state)

      # Reaching the bus rule means every handler was removed, so only the
      # remote rule is retried.
      assert %{status: :recovering, recovery_attempt: 1, pending_handlers: pending} =
               remote_retry.rules[key]

      assert MapSet.size(pending) == 0
      assert is_reference(remote_retry.rules[key].retry_timer)

      orphaned = %{state | rules: %{}}

      assert {:noreply, ^orphaned} =
               Worker.handle_info({:operation_result, make_ref(), :late}, orphaned)
    end

    test "resets the connection when a rule's recovery budget is spent" do
      put_recovery_env(:match_recovery_max_attempts, 2)

      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      state = recovering_state(conn, key, rule)

      first = fail_recovery(state, key)
      assert %{status: :recovering, recovery_attempt: 1} = first.rules[key]
      assert is_reference(first.rules[key].retry_timer)
      refute first.resetting?

      second = fail_recovery(first, key)
      assert %{status: :recovering, recovery_attempt: 2} = second.rules[key]
      assert is_reference(second.rules[key].retry_timer)
      refute second.resetting?

      log =
        capture_log(fn ->
          exhausted = fail_recovery(second, key)

          # The budget is spent, so nothing is armed for another attempt. The
          # rule is not forgotten either: the reset owns it from here, exactly
          # as it owns the rules held back by the capacity branch.
          assert %{status: :recovering, operation: nil, retry_timer: nil} = exhausted.rules[key]
          assert exhausted.resetting?
          assert exhausted.state_lost?
          assert is_reference(exhausted.reset_token)
        end)

      assert log =~ "D-Bus match reset transition=recovery_exhausted"
    end

    test "retries a recovery to the default budget before resetting" do
      previous = Application.get_env(:rebus, :match_recovery_max_attempts)
      Application.delete_env(:rebus, :match_recovery_max_attempts)

      on_exit(fn ->
        unless is_nil(previous),
          do: Application.put_env(:rebus, :match_recovery_max_attempts, previous)
      end)

      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      capture_log(fn ->
        spent =
          Enum.reduce(1..30, recovering_state(conn, key, rule), fn attempt, acc ->
            acc = fail_recovery(acc, key)

            assert %{status: :recovering, recovery_attempt: ^attempt} = acc.rules[key]
            assert is_reference(acc.rules[key].retry_timer)
            refute acc.resetting?

            acc
          end)

        exhausted = fail_recovery(spent, key)

        assert %{recovery_attempt: 30, retry_timer: nil} = exhausted.rules[key]
        assert exhausted.resetting?
      end)
    end

    test "warns once when a rule's recovery backoff saturates" do
      put_recovery_env(:match_recovery_max_attempts, 100)

      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      fifth =
        Enum.reduce(1..5, recovering_state(conn, key, rule), fn _attempt, acc ->
          fail_recovery(acc, key)
        end)

      assert %{recovery_attempt: 5} = fifth.rules[key]

      {sixth, log} = with_log(fn -> fail_recovery(fifth, key) end)

      assert %{recovery_attempt: 6} = sixth.rules[key]

      lines = for line <- String.split(log, "\n"), line =~ "still retrying", do: line
      assert [line] = lines
      assert line =~ "attempt=6"
      assert line =~ "delay_ms=1000"

      quiet =
        capture_log(fn ->
          seventh = fail_recovery(sixth, key)
          assert %{recovery_attempt: 7} = seventh.rules[key]
          refute seventh.resetting?
        end)

      refute quiet =~ "still retrying"
    end

    test "carries a rule's recovery budget across a worker restart" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      row = %{rule_state(rule) | status: :recovering, recovery_kind: :rule, recovery_attempt: 29}

      :ok = Store.persist_state(conn, false, persistence_changes(key), %{key => row}, %{})

      assert {:ok, restored} = Worker.init(conn)

      # A restart is not evidence that the bus resolved anything, so the count
      # the rule had accrued is restored with it.
      assert %{status: :recovering, recovery_attempt: 29} = restored.rules[key]
      assert MapSet.member?(restored.recovering_rules, key)
      assert_receive {:resume_recovery, ^key}

      log =
        capture_log(fn ->
          # One attempt is left of the default budget of 30, not a fresh 30.
          last = fail_recovery(restored, key)
          assert %{recovery_attempt: 30} = last.rules[key]
          assert is_reference(last.rules[key].retry_timer)
          refute last.resetting?

          exhausted = fail_recovery(last, key)

          assert %{recovery_attempt: 30, retry_timer: nil} = exhausted.rules[key]
          assert exhausted.resetting?
        end)

      assert log =~ "D-Bus match reset transition=recovery_exhausted"

      :ok = Store.delete_state(conn)
    end

    test "spends recovery budget on an attempt whose task never starts" do
      put_recovery_env(:match_recovery_max_attempts, 1)

      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      state = recovering_state(conn, key, rule)

      log =
        capture_log(fn ->
          first = fail_recovery(state, key, :not_started)

          assert %{status: :recovering, recovery_attempt: 1} = first.rules[key]
          assert is_reference(first.rules[key].retry_timer)
          refute first.resetting?

          exhausted = fail_recovery(first, key, :not_started)

          assert %{recovery_attempt: 1, retry_timer: nil} = exhausted.rules[key]
          assert exhausted.resetting?
          assert exhausted.state_lost?
        end)

      assert log =~ "D-Bus match reset transition=recovery_exhausted"
    end

    test "covers final removal and successful recovery transitions" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)
      ref = make_ref()

      {state, token} = operation_state(state, key, rule, :remove, status: :active)
      state = put_subscription_state(state, key, ref)

      assert {:noreply, removed} =
               Worker.handle_info({:operation_result, token, {:removed, ref, :final}}, state)

      refute Map.has_key?(removed.subscriptions, ref)
      refute Map.has_key?(removed.rules, key)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      assert {:noreply, cleared} = Worker.handle_info({:operation_result, token, :cleared}, state)
      refute Map.has_key?(cleared.rules, key)

      :ok = Store.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      pending_handler = make_ref()
      state = put_in(state.rules[key].pending_handlers, MapSet.new([pending_handler]))

      assert {:noreply, handlers_cleared} =
               Worker.handle_info({:operation_result, token, :handlers_cleared}, state)

      assert %{status: :cleaning, pending_handlers: pending_handlers} =
               handlers_cleared.rules[key]

      assert MapSet.size(pending_handlers) == 0
    end

    test "rejects overlapping well-known sender subscriptions locally" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)

      existing =
        MatchRule.new!(
          sender: "org.example.First",
          interface: "org.example.Interface",
          member: "Changed",
          path_namespace: "/org/example",
          args: %{0 => "changed"},
          arg0namespace: "org.example"
        )

      candidate =
        MatchRule.new!(
          sender: "org.example.Second",
          interface: "org.example.Interface",
          member: "Changed",
          path: "/org/example/child",
          args: %{0 => "changed"},
          arg0namespace: "org.example.child"
        )

      state = put_rule_state(state, MatchRule.to_string(existing), existing, [])
      deadline = System.monotonic_time(:millisecond) + 1_000

      assert {:reply, {:error, :sender_routing_ambiguous}, ^state} =
               Worker.handle_call(
                 {:add, self(), candidate, deadline},
                 {self(), make_ref()},
                 state
               )
    end

    test "covers late successful add completions and failed operation resets" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_new)
      handler_ref = make_ref()

      assert {:noreply, late_add} =
               Worker.handle_info({:operation_result, token, {:added, handler_ref}}, state)

      # Nobody is left to hand the subscription to, so the rule owns both the
      # local handler and the bus rule and starts recovering them.
      assert %{
               status: :recovering,
               recovery_kind: :rule,
               remote_may_exist?: true,
               pending_handlers: pending,
               operation: recovery
             } = late_add.rules[key]

      assert MapSet.equal?(pending, MapSet.new([handler_ref]))
      assert MapSet.member?(late_add.recovering_rules, key)
      assert late_add.subscriptions == %{}
      assert is_reference(recovery)

      assert %{key: ^key, type: :recovery, request_id: nil, monitor: monitor} =
               late_add.operations[recovery]

      assert late_add.operation_monitors[monitor] == recovery

      # An operation is in flight, so the snapshot must not claim the rule is
      # stable enough to restore.
      assert {:ok, %{uncertain?: true}} = Store.load_state(conn)

      :ok = Store.delete_state(conn)
      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)
      handler_ref = make_ref()

      assert {:noreply, late_reuse} =
               Worker.handle_info(
                 {:operation_result, token, {:added_existing, handler_ref}},
                 state
               )

      # The rule kept no other reference, so the handler installed for the
      # departed caller is recovered together with the bus rule.
      assert %{status: :recovering, recovery_kind: :rule, pending_handlers: pending} =
               late_reuse.rules[key]

      assert MapSet.equal?(pending, MapSet.new([handler_ref]))
      refute late_reuse.rules[key].remote_may_exist?
      assert is_reference(late_reuse.rules[key].operation)
      assert MapSet.member?(late_reuse.recovering_rules, key)
      assert late_reuse.subscriptions == %{}

      :ok = Store.delete_state(conn)
      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_new)

      assert {:noreply, reset} =
               Worker.handle_info(
                 {:operation_result, token, {:operation_failed, :disconnected}},
                 state
               )

      assert reset.state_lost?
      assert reset.resetting?
    end

    test "covers nonfinal and ambiguous removal completions" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)

      assert {:noreply, nonfinal} =
               Worker.handle_info({:operation_result, token, {:removed, ref, :nonfinal}}, state)

      # A non-final removal drops only its own subscription. The rule itself
      # stays, and having lost its last reference it takes an initial-cleanup
      # slot rather than being deleted outright.
      refute Map.has_key?(nonfinal.subscriptions, ref)
      assert Map.has_key?(nonfinal.rules, key)
      assert %{status: :cleaning, refs: refs} = nonfinal.rules[key]
      assert MapSet.equal?(refs, MapSet.new())
      assert MapSet.member?(nonfinal.initial_cleanup_keys, key)

      :ok = Store.delete_state(conn)
      ref = make_ref()
      definitive_error = {:error, {:bus_error, "denied"}}

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)
      {state, request_id, tag} = live_request(state, key, :remove, ref: ref)
      state = attach_request(state, token, request_id)

      assert {:noreply, definitive} =
               Worker.handle_info(
                 {:operation_result, token, {:remove_definitive_error, ref, definitive_error}},
                 state
               )

      # A definitive RemoveMatch error proves the server-side rule is settled,
      # so the caller is told why and the rule is dispatched onwards without
      # recovering anything.
      assert_receive {^tag, ^definitive_error}
      assert definitive.subscriptions[ref].handler == :removed
      assert %{status: :active, operation: nil, queue: []} = definitive.rules[key]
      assert definitive.recovering_rules == MapSet.new()
      assert definitive.operations == %{}

      :ok = Store.delete_state(conn)
      ref = make_ref()
      ambiguous_error = {:error, {:reply_dropped, :closed}}

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)
      {state, request_id, tag} = live_request(state, key, :remove, ref: ref)
      state = attach_request(state, token, request_id)

      assert {:noreply, ambiguous} =
               Worker.handle_info(
                 {:operation_result, token, {:remove_ambiguous, ref, ambiguous_error}},
                 state
               )

      # An ambiguous RemoveMatch leaves the server-side rule unresolved, so the
      # caller gets the error and the rule enters rule recovery with a fresh
      # attempt in flight.
      assert_receive {^tag, ^ambiguous_error}
      assert ambiguous.subscriptions[ref].handler == :removed

      assert %{status: :recovering, recovery_kind: :rule, operation: operation} =
               ambiguous.rules[key]

      assert is_reference(operation)
      assert MapSet.member?(ambiguous.recovering_rules, key)
    end

    test "restores a stable snapshot and resets an uncertain snapshot" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      rule_state = rule_state(rule)

      :ok =
        Store.persist_state(
          conn,
          false,
          persistence_changes(key),
          %{key => rule_state},
          %{}
        )

      assert {:ok, restored} = Worker.init(conn)
      assert %{^key => %{rule: ^rule}} = restored.rules

      :ok = Store.delete_state(conn)

      :ok =
        Store.persist_state(
          conn,
          true,
          persistence_changes(key),
          %{key => rule_state},
          %{}
        )

      assert {:ok, uncertain} = Worker.init(conn)
      assert uncertain.state_lost?
      assert_receive :reset_state_lost
    end

    test "restarts the best-effort cleanup a restored rule still owes the bus" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      :ok =
        Store.persist_state(
          conn,
          false,
          persistence_changes(key),
          %{key => cleaning_row(rule)},
          %{}
        )

      assert {:ok, restored} = Worker.init(conn)

      # The rule takes an initial-cleanup slot rather than a place in the
      # bounded recovery set: it has not failed a cleanup yet, it has only lost
      # the worker that was running one.
      assert MapSet.member?(restored.initial_cleanup_keys, key)
      refute MapSet.member?(restored.recovering_rules, key)
      assert :queue.is_empty(restored.initial_cleanup_queue)

      assert [{token, %{key: ^key, type: :initial_cleanup}}] = Map.to_list(restored.operations)
      assert restored.rules[key].operation == token

      :ok = Store.delete_state(conn)
    end

    test "admits restored cleaning rules under the live initial-cleanup cap" do
      conn = start_placeholder_conn()

      # One more rule than `@max_initial_cleanups`, which is private to the
      # worker: a restart can present as many cleaning rules at once as a burst
      # of owner exits can, so it must not start more bus work than that burst
      # would have been allowed to.
      rules =
        Map.new(1..17, fn index ->
          rule = test_rule("Changed#{index}")
          {MatchRule.to_string(rule), cleaning_row(rule)}
        end)

      keys = Map.keys(rules)
      changes = Enum.reduce(keys, empty_persistence(), &Store.rule_changed(&2, &1))

      :ok = Store.persist_state(conn, false, changes, rules, %{})

      assert {:ok, restored} = Worker.init(conn)

      assert MapSet.size(restored.initial_cleanup_keys) == 16
      assert map_size(restored.operations) == 16
      assert MapSet.size(restored.recovering_rules) == 0
      assert [queued] = :queue.to_list(restored.initial_cleanup_queue)
      refute MapSet.member?(restored.initial_cleanup_keys, queued)

      [{token, %{key: cleared_key}} | _rest] = Map.to_list(restored.operations)

      assert {:noreply, drained} =
               Worker.handle_info({:operation_result, token, :cleared}, restored)

      # The freed slot goes to the key that had to wait, exactly as it does for
      # a cleanup queued by the live path.
      refute MapSet.member?(drained.initial_cleanup_keys, cleared_key)
      assert MapSet.member?(drained.initial_cleanup_keys, queued)
      assert :queue.is_empty(drained.initial_cleanup_queue)

      :ok = Store.delete_state(conn)
    end

    test "covers initial cleanup and recovery operation-loss outcomes" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      for {result, kind} <- [
            {{:retry, :handlers}, :handlers},
            {{:retry, :remote}, :rule},
            {{:definitive_bus_error, {:error, {:bus_error, "denied"}}}, :rule}
          ] do
        {state, token} = cleaning_state(conn, key, rule)

        assert {:noreply, recovering} =
                 Worker.handle_info({:operation_result, token, result}, state)

        # A first cleanup that cannot prove what the bus did graduates to the
        # bounded recovery set and gives its initial-cleanup slot back.
        assert %{status: :recovering, recovery_kind: ^kind, recovery_attempt: 0} =
                 recovering.rules[key]

        assert is_reference(recovering.rules[key].operation)
        assert MapSet.member?(recovering.recovering_rules, key)
        refute MapSet.member?(recovering.initial_cleanup_keys, key)
      end

      {state, token} = cleaning_state(conn, key, rule)

      assert {:noreply, lost} =
               Worker.handle_info(
                 {:operation_result, token, {:operation_failed, :disconnected}},
                 state
               )

      # A lost cleanup task cannot report a safe outcome, so the connection is
      # reset instead of the rule being retried, and the slot is still released.
      assert lost.state_lost?
      assert lost.resetting?
      assert is_reference(lost.reset_token)
      assert is_reference(lost.reset_task_monitor)
      assert %{status: :cleaning, operation: nil} = lost.rules[key]
      assert lost.recovering_rules == MapSet.new()
      refute MapSet.member?(lost.initial_cleanup_keys, key)

      :ok = Store.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      assert {:noreply, reset} =
               Worker.handle_info(
                 {:operation_result, token, {:operation_failed, :disconnected}},
                 state
               )

      assert reset.state_lost?
    end

    test "covers owner, caller, and operation monitor losses" do
      conn = start_placeholder_conn()
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      owner_monitor = make_ref()
      owner_ref = make_ref()

      owner_lost = %{
        state
        | owner_monitors: %{owner_monitor => owner_ref},
          ref_monitors: %{owner_ref => owner_monitor}
      }

      assert {:noreply, stale_owner} =
               Worker.handle_info({:DOWN, owner_monitor, :process, self(), :normal}, owner_lost)

      # The monitor outlived the subscription it watched, so only its
      # bookkeeping goes and nothing is persisted for the connection.
      assert stale_owner.owner_monitors == %{}
      assert stale_owner.ref_monitors == %{}
      assert stale_owner.subscriptions == %{}
      assert stale_owner.rules == %{}
      refute Store.persisted?(conn)

      ref = make_ref()
      monitor = make_ref()
      subscribed = put_rule_state(state, key, rule, [])
      subscribed = put_in(subscribed.rules[key].status, :active)
      subscribed = put_subscription_state(subscribed, key, ref)

      subscribed = %{
        subscribed
        | owner_monitors: %{monitor => ref},
          ref_monitors: %{ref => monitor}
      }

      assert {:noreply, cleaning} =
               Worker.handle_info({:DOWN, monitor, :process, self(), :normal}, subscribed)

      # The last owner of an active rule died: its handler becomes pending and
      # the rule takes an initial-cleanup slot to remove the bus rule.
      assert cleaning.subscriptions == %{}
      assert cleaning.owner_monitors == %{}
      assert cleaning.ref_monitors == %{}

      assert %{status: :cleaning, recovery_kind: :rule, refs: refs, pending_handlers: pending} =
               cleaning.rules[key]

      assert MapSet.size(refs) == 0
      assert MapSet.equal?(pending, MapSet.new([ref]))
      assert is_reference(cleaning.rules[key].operation)
      assert MapSet.member?(cleaning.initial_cleanup_keys, key)

      assert {:ok,
              %{uncertain?: true, subscriptions: %{}, rules: %{^key => %{status: :cleaning}}}} =
               Store.load_state(conn)

      :ok = Store.delete_state(conn)
      request_id = make_ref()
      request_monitor = make_ref()
      tag = make_ref()
      timer = Process.send_after(self(), :unused_request_timer, 10_000)

      request_lost = %{
        state
        | requests: %{
            request_id => %{
              from: {self(), tag},
              owner: self(),
              key: key,
              timer: timer,
              monitor: request_monitor
            }
          },
          request_monitors: %{request_monitor => request_id}
      }

      assert {:noreply, caller_lost} =
               Worker.handle_info(
                 {:DOWN, request_monitor, :process, self(), :normal},
                 request_lost
               )

      # A caller that dies while waiting is answered rather than left in the
      # request table holding its rule's queue open.
      assert_receive {^tag, {:error, :disconnected}}
      assert caller_lost.requests == %{}
      assert caller_lost.request_monitors == %{}
      refute Store.persisted?(conn)

      {state, token} = operation_state(state, key, rule, :add_new)
      operation_monitor = make_ref()
      state = put_in(state.operations[token].monitor, operation_monitor)
      state = %{state | operation_monitors: %{operation_monitor => token}}

      assert {:noreply, reset} =
               Worker.handle_info({:DOWN, operation_monitor, :process, self(), :killed}, state)

      assert reset.state_lost?
      assert reset.resetting?
      assert reset.operations == %{}
      assert reset.operation_monitors == %{}
      assert %{operation: nil} = reset.rules[key]
    end

    test "covers expired reuse and operation-failure completion branches" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)
      {state, request_id, tag} = expired_request(state, key, :add)
      state = attach_request(state, token, request_id)
      handler_ref = make_ref()

      assert {:noreply, expired} =
               Worker.handle_info(
                 {:operation_result, token, {:added_existing, handler_ref}},
                 state
               )

      # The caller's deadline passed while the handler was being installed, so
      # it is told so and the handler is recovered instead of handed over.
      assert_receive {^tag, {:error, :timeout}}
      assert expired.requests == %{}
      assert expired.request_monitors == %{}
      assert expired.subscriptions == %{}

      assert %{status: :recovering, recovery_kind: :rule, pending_handlers: pending} =
               expired.rules[key]

      assert MapSet.equal?(pending, MapSet.new([handler_ref]))
      assert MapSet.member?(expired.recovering_rules, key)

      :ok = Store.delete_state(conn)
      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)

      assert {:noreply, reset} =
               Worker.handle_info(
                 {:operation_result, token, {:operation_failed, :disconnected}},
                 state
               )

      assert reset.state_lost?

      :ok = Store.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      assert {:noreply, reset} =
               Worker.handle_info(
                 {:operation_result, token, {:operation_failed, :disconnected}},
                 state
               )

      assert reset.state_lost?
    end

    test "covers retry dispatch and initial handler cleanup completion" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      {state, token} = cleaning_state(conn, key, rule)
      state = put_in(state.rules[key].pending_handlers, MapSet.new([make_ref()]))

      assert {:noreply, handlers_cleared} =
               Worker.handle_info({:operation_result, token, :handlers_cleared}, state)

      # With its handlers gone the rule is briefly active again, and, still
      # holding no references, immediately starts cleaning up the bus rule.
      assert %{status: :cleaning, recovery_kind: :rule, pending_handlers: pending} =
               handlers_cleared.rules[key]

      assert MapSet.size(pending) == 0
      assert is_reference(handlers_cleared.rules[key].operation)
      assert handlers_cleared.recovering_rules == MapSet.new()
      assert MapSet.member?(handlers_cleared.initial_cleanup_keys, key)

      :ok = Store.delete_state(conn)
      state = put_rule_state(fresh_worker_state(conn), key, rule, [])
      timer = Process.send_after(self(), {:retry_recovery, key}, 10_000)
      state = put_in(state.rules[key].status, :recovering)
      state = put_in(state.rules[key].recovery_kind, :rule)
      state = put_in(state.rules[key].recovery_attempt, 2)
      state = put_in(state.rules[key].retry_timer, timer)

      assert {:noreply, retried} = Worker.handle_info({:retry_recovery, key}, state)

      # The scheduled retry consumes its timer and starts the next attempt
      # without counting itself as a further failure.
      assert %{
               status: :recovering,
               recovery_kind: :rule,
               recovery_attempt: 2,
               retry_timer: nil,
               operation: operation
             } = retried.rules[key]

      assert is_reference(operation)
      assert %{key: ^key, type: :recovery, request_id: nil} = retried.operations[operation]
    end

    test "covers worker no-ops and stale recovery state" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      assert {:error, :timeout} =
               Worker.call(self(), :ignored, System.monotonic_time(:millisecond) - 1, 0)

      assert {:error, :disconnected} =
               Worker.call(self(), :ignored, System.monotonic_time(:millisecond) + 1_000, 0)

      # The call must expire against a process that never answers and never
      # exits: an exit while the call is still in flight would be reported as
      # `{:error, :disconnected}` instead of the timeout under test.
      silent =
        spawn_link(fn ->
          receive do
            {:"$gen_call", _from, _request} ->
              receive do
                :stop -> :ok
              end
          end
        end)

      assert {:error, :timeout} =
               Worker.call(silent, :ignored, System.monotonic_time(:millisecond) + 10, 0)

      send(silent, :stop)

      {state, _request_id, tag} = live_request(fresh_worker_state(conn), key, :add)

      assert :ok = Worker.terminate(:normal, state)

      # A terminating worker cannot finish what its callers asked for, so each
      # of them is told rather than left waiting for its own deadline.
      assert_receive {^tag, {:error, :disconnected}}

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)
      {state, request_id, tag} = live_request(state, key, :add)
      state = attach_request(state, token, request_id)
      state = %{state | rules: %{}}

      assert {:noreply, orphaned} =
               Worker.handle_info(
                 {:operation_result, token, {:add_existing_failed, {:error, :disconnected}}},
                 state
               )

      # The rule was already gone. The caller is still answered, and no rule is
      # recreated for a result nothing is waiting on.
      assert_receive {^tag, {:error, :disconnected}}
      assert orphaned.rules == %{}
      assert orphaned.operations == %{}
      assert orphaned.requests == %{}
      refute Store.persisted?(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      timer = Process.send_after(self(), {:retry_recovery, key}, 10_000)
      state = put_in(state.rules[key].retry_timer, timer)

      assert {:noreply, retained} =
               Worker.handle_info(
                 {:operation_result, token,
                  {:definitive_bus_error, {:error, {:bus_error, "denied"}}}},
                 state
               )

      # A retry is already scheduled, so this result must neither schedule a
      # second one nor charge the rule another attempt.
      assert %{status: :recovering, retry_timer: ^timer, recovery_attempt: 0, operation: nil} =
               retained.rules[key]

      :ok = Store.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      state = put_in(state.rules[key].queue, [make_ref()])

      assert {:noreply, cleared} = Worker.handle_info({:operation_result, token, :cleared}, state)

      # The only queued request had already gone, so the cleared rule is
      # dropped rather than reinstalled for it.
      refute Map.has_key?(cleared.rules, key)
      assert cleared.recovering_rules == MapSet.new()
      refute Store.persisted?(conn)
    end

    test "reinstalls a cleared rule for the requests still queued behind it" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      {state, remove_id, remove_tag} = live_request(state, key, :remove, ref: make_ref())
      {state, add_id, add_tag} = live_request(state, key, :add)
      state = put_in(state.rules[key].queue, [remove_id, add_id])

      assert {:noreply, resumed} = Worker.handle_info({:operation_result, token, :cleared}, state)

      # Clearing the rule satisfies the queued removal outright and reinstalls
      # the rule for the caller still waiting to add it.
      assert_receive {^remove_tag, :ok}
      refute_received {^add_tag, _reply}
      refute Map.has_key?(resumed.requests, remove_id)
      assert Map.has_key?(resumed.requests, add_id)
      assert resumed.recovering_rules == MapSet.new()

      assert %{status: :installing, queue: [], refs: refs, operation: operation} =
               resumed.rules[key]

      assert MapSet.size(refs) == 0
      assert is_reference(operation)

      assert %{key: ^key, type: :add_new, request_id: ^add_id} = resumed.operations[operation]
    end

    test "resets a connection when ambiguous cleanup reaches its bounded capacity", %{
      server: server,
      connection: connection
    } do
      previous = Application.get_env(:rebus, :match_recovery_max_rules)
      Application.put_env(:rebus, :match_recovery_max_rules, 1)

      on_exit(fn ->
        if is_nil(previous),
          do: Application.delete_env(:rebus, :match_recovery_max_rules),
          else: Application.put_env(:rebus, :match_recovery_max_rules, previous)
      end)

      first_rule = test_rule()
      second_rule = test_rule("Other")

      first = Task.async(fn -> Rebus.add_match(connection, first_rule, 50) end)
      _first_add = assert_add_match(server, first_rule)
      assert {:error, :timeout} = Task.await(first, 500)
      _first_cleanup = assert_remove_match(server, first_rule)

      second = Task.async(fn -> Rebus.add_match(connection, second_rule, 50) end)
      _second_add = assert_add_match(server, second_rule)
      assert {:error, :timeout} = Task.await(second, 500)
      assert wait_until(fn -> not Process.alive?(connection) end)
    end

    test "drops local subscription state without RemoveMatch after connection teardown", %{
      server: server,
      connection: connection
    } do
      rule = test_rule()
      owner = subscribe_owner(self(), connection, rule)

      add = assert_add_match(server, rule)
      :ok = TestServer.push(server, method_return(add.serial))
      _ref = assert_subscription(owner)
      :ok = answer_tracking(server)

      assert :ok = Rebus.close(connection)
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 150
      send(owner, :stop)
    end

    test "reads a torn write as uncertain" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      # The first two phases of a write and no third: exactly what a worker
      # that dies part-way through `persist_state/5` leaves behind.
      :ok = Store.mark_writing(conn)
      :ok = Store.write_rows(conn, persistence_changes(key), %{key => rule_state(rule)}, %{})

      assert {:ok, %{uncertain?: true, rules: rules, subscriptions: %{}}} = Store.load_state(conn)
      assert Map.has_key?(rules, key)

      # A worker starting on that snapshot must not restore any of it.
      assert {:ok, state} = Worker.init(conn)
      assert state.state_lost?
      assert state.rules == %{}
      assert state.subscriptions == %{}
      assert_received :reset_state_lost

      :ok = Store.delete_state(conn)
    end

    test "completing the write clears the uncertainty a torn one would leave" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)
      changes = persistence_changes(key)
      row = rule_state(rule)

      :ok = Store.mark_writing(conn)
      assert {:ok, %{uncertain?: true, rules: %{}}} = Store.load_state(conn)

      :ok = Store.write_rows(conn, changes, %{key => row}, %{})
      :ok = Store.write_meta(conn, false)

      assert {:ok, %{uncertain?: false, rules: %{^key => ^row}}} = Store.load_state(conn)

      # The whole-write entry point leaves the same result.
      :ok = Store.persist_state(conn, false, changes, %{key => row}, %{})
      assert {:ok, %{uncertain?: false, rules: %{^key => ^row}}} = Store.load_state(conn)

      :ok = Store.delete_state(conn)
    end

    test "discards the rows an uncertain restart does not restore" do
      conn = start_placeholder_conn()
      rule = test_rule()
      key = MatchRule.to_string(rule)

      :ok =
        Store.persist_state(conn, true, persistence_changes(key), %{key => rule_state(rule)}, %{})

      assert persisted_rows(conn, :rule) != []

      assert {:ok, state} = Worker.init(conn)
      assert state.state_lost?
      assert_received :reset_state_lost

      # The rows are gone, but the meta row stays: the connection is still
      # watched, and the next read still says the snapshot is uncertain.
      assert Store.persisted?(conn)
      assert {:ok, %{uncertain?: true, rules: %{}, subscriptions: %{}}} = Store.load_state(conn)

      :ok = Store.delete_state(conn)
    end

    test "does not resurrect a meta row the owner has already reaped" do
      conn = spawn(fn -> Process.sleep(:infinity) end)
      monitor = Process.monitor(conn)

      :ok = Store.mark_writing(conn)
      assert Store.persisted?(conn)

      Process.exit(conn, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^conn, :killed}, 1_000
      assert wait_until(fn -> not Store.persisted?(conn) end)

      # The rest of the write lands after the reap. `write_meta/2` replaces in
      # place, so it strands no meta row; the rows it wrote alongside are the
      # worker's to clean up.
      rule = test_rule()
      key = MatchRule.to_string(rule)
      :ok = Store.write_rows(conn, persistence_changes(key), %{key => rule_state(rule)}, %{})
      :ok = Store.write_meta(conn, false)

      refute Store.persisted?(conn)
      assert persisted_rows(conn, :rule) != []

      # A worker holds its own monitor on the connection, and that `:DOWN` is
      # the backstop for exactly these rows.
      assert {:ok, state} = Worker.init(conn)

      assert {:stop, :normal, _state} =
               Worker.handle_info(
                 {:DOWN, state.connection_monitor, :process, conn, :killed},
                 state
               )

      assert persisted_rows(conn, :rule) == []
      refute Store.persisted?(conn)
    end
  end

  defp test_rule(member \\ "Changed") do
    MatchRule.new!(
      sender: "org.example.Service",
      interface: "org.example.Interface",
      member: member,
      path_namespace: "/org/example",
      args: %{0 => "changed"}
    )
  end

  defp test_signal(argument, opts \\ []) do
    signal_opts = [
      sender: "org.example.Service",
      path: "/org/example/child",
      interface: "org.example.Interface",
      member: "Changed",
      signature: "s",
      body: [argument]
    ]

    Message.new!(:signal, Keyword.merge(signal_opts, opts))
  end

  defp subscribe_owner(parent, connection, rule) do
    spawn(fn ->
      case Rebus.add_match(connection, rule, 1_000) do
        {:ok, ref} ->
          send(parent, {:subscribed, self(), ref})
          subscription_loop(parent, ref)

        {:error, reason} ->
          send(parent, {:subscription_error, self(), reason})
      end
    end)
  end

  defp subscription_loop(parent, ref) do
    receive do
      {^ref, %Message{} = message} ->
        send(parent, {:matched, self(), ref, message})
        subscription_loop(parent, ref)

      :stop ->
        :ok
    end
  end

  defp assert_subscription(owner) do
    assert_receive {:subscribed, ^owner, ref}, 1_000
    ref
  end

  # The rule string is part of the received pattern rather than a following
  # assertion, so an unrelated AddMatch/RemoveMatch in flight - the owner
  # tracking rule a well-known sender installs - is skipped rather than
  # mistaken for this rule's.
  #
  # Do not tighten this back to matching the next AddMatch and asserting the
  # rule string afterwards: a second rule added for a well-known sender while
  # the first rule's tracking task is still running puts the caller's AddMatch
  # and the tracking AddMatch in flight from different processes, and their
  # arrival order at the tap is nondeterministic, so the strict form flakes.
  # The pinned pattern plus an explicit `answer_tracking/1` drain is the
  # deterministic combination.
  defp assert_add_match(server, rule) do
    rule_string = MatchRule.to_string(rule)

    assert_receive {^server,
                    %Message{
                      type: :method_call,
                      header_fields: %{
                        destination: "org.freedesktop.DBus",
                        interface: "org.freedesktop.DBus",
                        member: "AddMatch",
                        path: "/org/freedesktop/DBus"
                      },
                      body: [^rule_string]
                    } = message},
                   1_000

    message
  end

  defp assert_remove_match(server, rule, timeout \\ 1_000) do
    rule_string = MatchRule.to_string(rule)

    assert_receive {^server,
                    %Message{
                      type: :method_call,
                      header_fields: %{member: "RemoveMatch"},
                      body: [^rule_string]
                    } = message},
                   timeout

    message
  end

  defp tracking_rule(name \\ "org.example.Service"), do: Operation.tracking_rule(name)

  defp directed_signal(sender),
    do: test_signal("changed", sender: sender, destination: ":1.100")

  defp bus_error(serial, error_name),
    do: Message.new!(:error, reply_serial: serial, error_name: error_name)

  defp name_owner(connection, name), do: Map.get(:sys.get_state(connection).name_owners, name)

  defp tracked?(connection, name), do: Map.has_key?(:sys.get_state(connection).name_owners, name)

  defp tracking_idle?(connection) do
    connection
    |> subscription_worker()
    |> :sys.get_state()
    |> Map.fetch!(:tracking_ops)
    |> map_size() == 0
  end

  # Plays the bus for the owner-tracking sequence a well-known sender
  # subscription runs after its own AddMatch: the tracking rule, then the
  # GetNameOwner whose reply seeds the owner.
  defp answer_tracking(server, name \\ "org.example.Service", owner \\ nil) do
    add = assert_add_match(server, tracking_rule(name))
    :ok = TestServer.push(server, method_return(add.serial))
    get = assert_get_name_owner(server, name)
    :ok = TestServer.push(server, name_owner_reply(get.serial, owner))
    :ok
  end

  defp assert_get_name_owner(server, name) do
    assert_receive {^server,
                    %Message{
                      type: :method_call,
                      header_fields: %{
                        destination: "org.freedesktop.DBus",
                        interface: "org.freedesktop.DBus",
                        member: "GetNameOwner",
                        path: "/org/freedesktop/DBus"
                      },
                      body: [^name]
                    } = message},
                   1_000

    message
  end

  defp name_owner_reply(serial, nil) do
    Message.new!(:error,
      reply_serial: serial,
      error_name: "org.freedesktop.DBus.Error.NameHasNoOwner"
    )
  end

  defp name_owner_reply(serial, owner) do
    Message.new!(:method_return, reply_serial: serial, signature: "s", body: [owner])
  end

  defp name_owner_changed(name, old_owner, new_owner, opts \\ []) do
    signal_opts = [
      sender: "org.freedesktop.DBus",
      path: "/org/freedesktop/DBus",
      interface: "org.freedesktop.DBus",
      member: "NameOwnerChanged",
      signature: "sss",
      body: [name, old_owner, new_owner]
    ]

    Message.new!(:signal, Keyword.merge(signal_opts, opts))
  end

  # A RemoveMatch for any subscription rule. The owner-tracking rule's own
  # removal is not one of the removals these tests count, so it is skipped.
  defp assert_any_remove_match(server, timeout) do
    tracking = MatchRule.to_string(tracking_rule())

    assert_receive {^server,
                    %Message{
                      type: :method_call,
                      header_fields: %{member: "RemoveMatch"},
                      body: [rule_string]
                    } = message}
                   when rule_string != tracking,
                   timeout

    message
  end

  defp method_return(serial), do: Message.new!(:method_return, reply_serial: serial)

  defp rule_entry(connection, rule) do
    case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
      [{worker, _value}] -> Map.get(:sys.get_state(worker).rules, MatchRule.to_string(rule))
      [] -> nil
    end
  end

  defp rule_status(connection, rule) do
    case rule_entry(connection, rule) do
      nil -> nil
      entry -> entry.status
    end
  end

  defp rule_ref_count(connection, rule) do
    case rule_entry(connection, rule) do
      nil -> 0
      entry -> MapSet.size(entry.refs)
    end
  end

  defp rule_queue_length(connection, rule) do
    case rule_entry(connection, rule) do
      nil -> 0
      entry -> length(entry.queue)
    end
  end

  defp state_table, do: Store.table()

  defp persisted_rows(connection, kind) do
    :ets.match_object(state_table(), {{kind, connection, :_}, :_})
  end

  defp reaped?(connection) do
    not :ets.member(state_table(), {:meta, connection}) and
      persisted_rows(connection, :rule) == [] and
      persisted_rows(connection, :subscription) == []
  end

  # Kills `pid`, waits for the processes in `dead` that `rest_for_one` takes
  # with it, and for every `{name, pid}` in `replaced` to be re-registered
  # under a new pid.
  defp kill_and_await_restart(pid, dead, replaced) do
    monitors = Enum.map([pid | dead], &{&1, Process.monitor(&1)})
    capture_log(fn -> await_restart(pid, monitors, replaced) end)
  end

  defp await_restart(pid, monitors, replaced) do
    kill_supervised(pid, Rebus.MatchSubscription.Supervisor)
    Enum.each(monitors, &assert_down/1)
    assert wait_until(fn -> Enum.all?(replaced, &replaced?/1) end)
  end

  defp kill_worker(worker), do: kill_supervised(worker, Rebus.MatchSubscription)

  defp assert_down({target, monitor}) do
    assert_receive {:DOWN, ^monitor, :process, ^target, _reason}, 1_000
  end

  defp replaced?({name, previous}) do
    current = Process.whereis(name)
    is_pid(current) and current != previous
  end

  defp subscription_worker(connection) do
    [{worker, _value}] = Registry.lookup(Rebus.MatchSubscription.Registry, connection)
    worker
  end

  # `Registry.lookup/2` and `GenServer.stop/1` are necessarily separate
  # operations. The worker can observe its connection monitor and terminate
  # normally in that gap, especially on older OTP schedulers. Cleanup must not
  # turn that successful shutdown into a test failure.
  defp stop_worker(worker) when is_pid(worker) do
    GenServer.stop(worker)
  catch
    :exit, {:noproc, _} -> :ok
  end

  defp start_direct_connection(server) do
    {:ok, address} = TestServer.get_listen_addr(server)
    connect_ref = make_ref()

    {:ok, connection} =
      Rebus.Connection.start_link({[], %{addr: address, connect_waiter: {self(), connect_ref}}})

    Process.unlink(connection)

    assert_receive {^connect_ref, {:ok, ^connection}}, 1_000
    send(connection, {connect_ref, :accepted})
    assert_receive {^connect_ref, :accepted}, 1_000
    assert_receive {^server, %Message{header_fields: %{member: "Hello"}}}, 1_000

    {:ok, worker} = Worker.start_link(connection)
    {connection, worker}
  end

  defp start_placeholder_conn do
    {:ok, conn} = PlaceholderConnection.start()

    on_exit(fn ->
      if Process.alive?(conn), do: Process.exit(conn, :kill)
      _ = Store.delete_state(conn)
    end)

    conn
  end

  defp fresh_worker_state(conn) do
    {:ok, state} = Worker.init(conn)
    state
  end

  defp put_rule_state(state, key, rule, queue) do
    rule_state = %{struct!(Worker.Rule, rule_state(rule)) | queue: queue}

    %{state | rules: %{key => rule_state}}
  end

  # The persisted shape of a rule: a plain map of the record's fields, which is
  # both what a row holds and what the record restores from.
  defp rule_state(rule) do
    %{
      rule: rule,
      refs: MapSet.new(),
      pending_handlers: MapSet.new(),
      remote_may_exist?: false,
      status: :installing,
      operation: nil,
      queue: [],
      recovery_kind: nil,
      recovery_attempt: 0,
      retry_timer: nil
    }
  end

  # The persisted shape of a rule whose final owner has gone and whose one
  # best-effort RemoveMatch is still owed to the bus, as `start_initial_cleanup`
  # leaves it.
  defp cleaning_row(rule) do
    %{rule_state(rule) | status: :cleaning, recovery_kind: :rule, remote_may_exist?: true}
  end

  defp empty_persistence do
    %{
      dirty_rules: MapSet.new(),
      removed_rules: MapSet.new(),
      dirty_subscriptions: MapSet.new(),
      removed_subscriptions: MapSet.new()
    }
  end

  defp persistence_changes(key) do
    %{empty_persistence() | dirty_rules: MapSet.new([key])}
  end

  defp operation_state(state, key, rule, type, opts \\ []) do
    state = put_rule_state(state, key, rule, [])
    token = make_ref()
    request_id = make_ref()
    status = Keyword.get(opts, :status, :installing)
    rule_state = %{state.rules[key] | status: status, operation: token}

    state = %{
      state
      | rules: %{key => rule_state},
        operations: %{token => %{key: key, type: type, request_id: request_id, monitor: nil}}
    }

    {state, token}
  end

  # A rule already in the bounded recovery set, at attempt zero, as
  # `enter_recovery/3` leaves it once its first unproven cleanup graduates.
  defp recovering_state(conn, key, rule) do
    :ok = Store.delete_state(conn)

    state = put_rule_state(fresh_worker_state(conn), key, rule, [])
    rule_state = %{state.rules[key] | status: :recovering, recovery_kind: :rule}

    %{state | rules: %{key => rule_state}, recovering_rules: MapSet.new([key])}
  end

  # One failed recovery attempt for a rule that is already `:recovering`. The
  # attempt count it has accrued is preserved, and the timer the previous
  # attempt armed is cleared first, as the `{:retry_recovery, key}` path does
  # before it starts the next attempt.
  defp fail_recovery(state, key, result \\ {:definitive_bus_error, {:error, {:bus_error, "no"}}}) do
    token = make_ref()
    rule_state = %{state.rules[key] | operation: token, retry_timer: nil}

    state = %{
      state
      | rules: Map.put(state.rules, key, rule_state),
        operations:
          Map.put(state.operations, token, %{
            key: key,
            type: :recovery,
            request_id: nil,
            monitor: nil
          })
    }

    assert {:noreply, state} = Worker.handle_info({:operation_result, token, result}, state)

    state
  end

  defp put_recovery_env(key, value) do
    previous = Application.get_env(:rebus, key)
    Application.put_env(:rebus, key, value)

    on_exit(fn ->
      if is_nil(previous),
        do: Application.delete_env(:rebus, key),
        else: Application.put_env(:rebus, key, previous)
    end)
  end

  # A rule holding one of the bounded initial-cleanup slots, with its
  # best-effort cleanup operation in flight. Not purely a state builder: it
  # also clears any rows a previous block persisted for `conn`, so the caller
  # starts from an empty table.
  defp cleaning_state(conn, key, rule) do
    :ok = Store.delete_state(conn)

    {state, token} =
      operation_state(fresh_worker_state(conn), key, rule, :initial_cleanup, status: :cleaning)

    {%{state | initial_cleanup_keys: MapSet.new([key])}, token}
  end

  defp put_subscription_state(state, key, ref) do
    rule = %{state.rules[key] | refs: MapSet.new([ref])}

    %{
      state
      | rules: %{key => rule},
        subscriptions: %{ref => %{owner: self(), key: key, handler: :active}},
        owner_monitors: %{},
        ref_monitors: %{}
    }
  end

  # A request the worker will treat as still waiting. The `from` tag is
  # returned so a test can assert the reply the caller receives, since
  # `GenServer.reply/2` delivers it here as `{tag, reply}`.
  defp live_request(state, key, kind, opts \\ []) do
    deadline = System.monotonic_time(:millisecond) + 10_000
    put_test_request(state, key, kind, deadline, Keyword.get(opts, :ref))
  end

  defp expired_request(state, key, kind) do
    put_test_request(state, key, kind, System.monotonic_time(:millisecond) - 1, nil)
  end

  defp put_test_request(state, key, kind, deadline, ref) do
    request_id = make_ref()
    tag = make_ref()
    monitor = Process.monitor(self())
    timer = Process.send_after(self(), :unused_test_request_timer, 10_000)

    request = %{
      from: {self(), tag},
      owner: self(),
      kind: kind,
      key: key,
      ref: ref,
      deadline: deadline,
      timer: timer,
      monitor: monitor
    }

    state = %{
      state
      | requests: Map.put(state.requests, request_id, request),
        request_monitors: Map.put(state.request_monitors, monitor, request_id)
    }

    {state, request_id, tag}
  end

  defp attach_request(state, token, request_id) do
    put_in(state.operations[token].request_id, request_id)
  end

  defp raw_resource_limited_error_reply(reply_serial, error_name) do
    raw_wire_message(
      3,
      [[4, {"s", error_name}], [5, {"u", reply_serial}], [8, {"g", "ay"}]],
      scalar_limited_body()
    )
  end

  defp scalar_limited_body do
    sentinel = "resource-limit-body-sentinel"

    <<1_000_001::little-32, sentinel::binary>> <>
      :binary.copy(<<1>>, 1_000_001 - byte_size(sentinel))
  end

  defp raw_wire_message(type, fields, body) do
    header =
      Rebus.Encoder.encode_at_position("a(yv)", [fields], :little, 12)
      |> IO.iodata_to_binary()

    padding = :binary.copy(<<0>>, rem(8 - rem(12 + byte_size(header), 8), 8))

    <<?l, type, 0, 1, byte_size(body)::little-32, 1::little-32, header::binary, padding::binary,
      body::binary>>
  end

  defp wait_until(predicate, attempts \\ 100)

  defp wait_until(predicate, attempts) when attempts > 0 do
    if predicate.() do
      true
    else
      Process.sleep(10)
      wait_until(predicate, attempts - 1)
    end
  end

  defp wait_until(_predicate, 0), do: false
end
