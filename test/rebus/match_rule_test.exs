defmodule Rebus.MatchRuleTest do
  use ExUnit.Case

  import ExUnit.CaptureLog

  alias Rebus.MatchRule
  alias Rebus.MatchSubscription.Store
  alias Rebus.MatchSubscription.Worker
  alias Rebus.Message
  alias Rebus.TestServer

  # Upper bound on `kill_supervised/2` waiting for restart-budget room, in
  # `wait_until/2` attempts of 10 ms each: one budget period plus slack. Past
  # that the wait cannot help, and the kill would take the supervisor down, so
  # the helper fails there instead of leaving a later assertion to explain it.
  @restart_room_attempts 600

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

      owner_b = subscribe_owner(self(), connection, rule)
      ref_b = assert_subscription(owner_b)
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100

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

      send(owner_a, :stop)
      send(owner_b, :stop)
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

      assert {:error, :timeout} = Rebus.remove_match(connection, ref, 10)
      assert_remove_match(server, rule)

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

      send(owner, :stop)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
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
      worker = subscription_worker(connection)

      send(owner, :stop)
      remove = assert_remove_match(server, rule)

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

      removal = Task.async(fn -> Rebus.remove_match(connection, replacement_ref, 1_000) end)
      final_remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(final_remove.serial))
      assert :ok = Task.await(removal, 1_000)
      send(replacement, :stop)
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

      owners = [
        first | Enum.map(1..11, fn _index -> subscribe_owner(self(), connection, rule) end)
      ]

      Enum.each(owners -- [first], &assert_subscription/1)
      refute_receive {^server, %Message{header_fields: %{member: "AddMatch"}}}, 100

      Enum.each(owners, &send(&1, :stop))
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
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

      assert {:ok, %{uncertain?: false, rules: rules, subscriptions: refs}} =
               Rebus.MatchSubscription.load_state(connection)

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

      Enum.each(subscriptions, fn {owner, _rule, ref} ->
        send(owner, :stop)
        assert is_reference(ref)
      end)

      Enum.each(1..20, fn _index ->
        remove = assert_any_remove_match(server, 1_500)
        :ok = TestServer.push(server, method_return(remove.serial))
      end)
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

      send(owner, :stop)
      _first_remove = assert_remove_match(server, rule)
      retry = assert_remove_match(server, rule, 1_500)
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
      assert {:error, :timeout} = Task.await(slow, 1_000)

      slow_cleanup = assert_remove_match(server, slow_rule)
      :ok = TestServer.push(server, method_return(slow_cleanup.serial))

      removal = Task.async(fn -> Rebus.remove_match(connection, healthy_ref, 1_000) end)
      healthy_remove = assert_remove_match(server, healthy_rule)
      :ok = TestServer.push(server, method_return(healthy_remove.serial))
      assert :ok = Task.await(removal, 1_000)
      send(healthy, :stop)
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

      slow_cleanup = assert_remove_match(server, slow_rule, 1_500)
      :ok = TestServer.push(server, method_return(slow_cleanup.serial))

      removal = Task.async(fn -> Rebus.remove_match(connection, healthy_ref, 1_000) end)
      healthy_remove = assert_remove_match(server, healthy_rule)
      :ok = TestServer.push(server, method_return(healthy_remove.serial))
      assert :ok = Task.await(removal, 1_000)
      send(healthy, :stop)
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

      removal = Task.async(fn -> Rebus.remove_match(connection, replacement_ref, 1_000) end)
      final_remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(final_remove.serial))
      assert :ok = Task.await(removal, 1_000)
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

      initial_remove = Task.async(fn -> Rebus.remove_match(connection, initial_ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(initial_remove, 1_000)
      send(owner, :stop)
      assert wait_until(fn -> not Rebus.MatchSubscription.persisted?(connection) end)

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

      final_remove = Task.async(fn -> Rebus.remove_match(connection, replacement_ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(final_remove, 1_000)
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

      worker = subscription_worker(connection)
      kill_worker(worker)

      assert wait_until(fn ->
               case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
                 [{replacement, _value}] -> replacement != worker and Process.alive?(replacement)
                 [] -> false
               end
             end)

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> rule_status(connection, rule) == nil end)
      send(owner, :stop)
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

      table = :ets.whereis(state_table())
      table_owner = :ets.info(table, :owner)
      size = :ets.info(table, :size)
      assert size > 0
      assert {:ok, persisted} = Rebus.MatchSubscription.load_state(connection)

      worker = subscription_worker(connection)
      supervisor = Process.whereis(Rebus.MatchSubscription)

      kill_and_await_restart(supervisor, [worker], [{Rebus.MatchSubscription, supervisor}])

      # Same table, same owner, same rows: the worker supervisor no longer
      # takes the persisted state down with it.
      assert :ets.whereis(state_table()) == table
      assert :ets.info(table, :owner) == table_owner
      assert table_owner == Process.whereis(Store)
      assert :ets.info(table, :size) == size
      assert Rebus.MatchSubscription.load_state(connection) == {:ok, persisted}

      # A worker started afterwards restores them, so the reference is still
      # honoured and the bus rule is removed rather than left behind.
      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> not Rebus.MatchSubscription.persisted?(connection) end)
      send(owner, :stop)
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

      removal = Task.async(fn -> Rebus.remove_match(connection, ref, 1_000) end)
      remove = assert_remove_match(server, rule)
      :ok = TestServer.push(server, method_return(remove.serial))
      assert :ok = Task.await(removal, 1_000)
      assert wait_until(fn -> not Rebus.MatchSubscription.persisted?(connection) end)
      send(owner, :stop)
    end

    test "resets a supervisor-owned connection when subscription state is lost", %{
      connection: connection
    } do
      {:ok, worker} = Worker.start_link(connection)

      send(worker, :reset_state_lost)

      assert wait_until(fn -> not Process.alive?(connection) end)
      assert wait_until(fn -> not Process.alive?(worker) end)
      refute Rebus.MatchSubscription.persisted?(connection)
    end

    test "keeps state loss explicit for a directly started connection" do
      {:ok, server} = TestServer.start_link(tap: self())

      on_exit(fn ->
        if Process.alive?(server), do: GenServer.stop(server)
      end)

      {direct_connection, worker} = start_direct_connection(server)

      on_exit(fn ->
        stop_worker(worker)
        if Process.alive?(direct_connection), do: Process.exit(direct_connection, :shutdown)
        _ = Rebus.MatchSubscription.delete_state(direct_connection)
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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      stale_key = "stale-rule"

      on_exit(fn ->
        case Registry.lookup(Rebus.MatchSubscription.Registry, conn) do
          [{worker, _value}] when is_pid(worker) -> stop_worker(worker)
          [] -> :ok
        end

        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      :ok =
        Rebus.MatchSubscription.persist_state(
          conn,
          false,
          %{persistence_changes(stale_key) | dirty_rules: MapSet.new([stale_key])},
          %{},
          %{}
        )

      assert Rebus.MatchSubscription.persisted?(conn)
      assert :ok = Rebus.remove_match(conn, make_ref(), 100)
    end

    test "covers reset guard transitions without a live bus" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)
      rule = test_rule()
      deadline = System.monotonic_time(:millisecond) + 1_000
      from = {self(), make_ref()}

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      assert {:noreply, ^state} =
               Worker.handle_info({:connection_reset_result, make_ref(), :ok}, state)

      monitor = Process.monitor(conn)
      token = make_ref()
      resetting = %{state | reset_token: token, reset_task_monitor: monitor, resetting?: true}

      assert {:noreply, completed} =
               Worker.handle_info({:connection_reset_result, token, :ok}, resetting)

      assert completed.state_lost?
      assert completed.resetting?
      assert is_nil(completed.reset_token)

      monitor = Process.monitor(conn)
      token = make_ref()
      resetting = %{state | reset_token: token, reset_task_monitor: monitor, resetting?: true}

      assert {:noreply, failed} =
               Worker.handle_info(
                 {:connection_reset_result, token, {:error, :not_connection}},
                 resetting
               )

      assert failed.state_lost?
      refute failed.resetting?
      assert is_nil(failed.reset_token)
    end

    test "covers stale worker events and reset task loss" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

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
    end

    test "covers failed add, reuse, and removal operation completions" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      {state, token} = operation_state(state, key, rule, :add_new)

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token, {:add_failed, {:error, {:bus_error, "denied"}}, nil}},
                 state
               )

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token, {:add_existing_failed, {:error, :disconnected}}},
                 state
               )

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :remove)

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token, {:remove_failed, {:error, :timeout}, :active}},
                 state
               )
    end

    test "covers recovery result classifications and stale operation cleanup" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      {state, token} = operation_state(state, key, rule, :recovery, status: :recovering)

      assert {:noreply, retrying} =
               Worker.handle_info(
                 {:operation_result, token,
                  {:definitive_bus_error, {:error, {:bus_error, "denied"}}}},
                 state
               )

      assert %{status: :recovering, retry_timer: retry_timer} = retrying.rules[key]
      assert is_reference(retry_timer)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      assert {:noreply, _state} =
               Worker.handle_info({:operation_result, token, {:retry, :handlers}}, state)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      assert {:noreply, _state} =
               Worker.handle_info({:operation_result, token, {:retry, :remote}}, state)

      orphaned = %{state | rules: %{}}

      assert {:noreply, ^orphaned} =
               Worker.handle_info({:operation_result, make_ref(), :late}, orphaned)
    end

    test "covers final removal and successful recovery transitions" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)
      ref = make_ref()

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

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

      :ok = Rebus.MatchSubscription.delete_state(conn)

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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_new)

      assert {:noreply, _state} =
               Worker.handle_info({:operation_result, token, {:added, make_ref()}}, state)

      :ok = Rebus.MatchSubscription.delete_state(conn)
      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token, {:added_existing, make_ref()}},
                 state
               )

      :ok = Rebus.MatchSubscription.delete_state(conn)
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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)

      assert {:noreply, nonfinal} =
               Worker.handle_info({:operation_result, token, {:removed, ref, :nonfinal}}, state)

      refute Map.has_key?(nonfinal.subscriptions, ref)

      :ok = Rebus.MatchSubscription.delete_state(conn)
      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)

      assert {:noreply, definitive} =
               Worker.handle_info(
                 {:operation_result, token,
                  {:remove_definitive_error, ref, {:error, {:bus_error, "denied"}}}},
                 state
               )

      assert definitive.subscriptions[ref].handler == :removed

      :ok = Rebus.MatchSubscription.delete_state(conn)
      ref = make_ref()

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :remove, status: :active)

      state = put_subscription_state(state, key, ref)

      assert {:noreply, ambiguous} =
               Worker.handle_info(
                 {:operation_result, token, {:remove_ambiguous, ref, {:error, :timeout}}},
                 state
               )

      assert ambiguous.subscriptions[ref].handler == :removed
    end

    test "restores a stable snapshot and resets an uncertain snapshot" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)
      rule_state = rule_state(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      :ok =
        Rebus.MatchSubscription.persist_state(
          conn,
          false,
          persistence_changes(key),
          %{key => rule_state},
          %{}
        )

      assert {:ok, restored} = Worker.init(conn)
      assert %{^key => %{rule: ^rule}} = restored.rules

      :ok = Rebus.MatchSubscription.delete_state(conn)

      :ok =
        Rebus.MatchSubscription.persist_state(
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

    test "covers initial cleanup and recovery operation-loss outcomes" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      for result <- [
            {:retry, :handlers},
            {:retry, :remote},
            {:definitive_bus_error, {:error, {:bus_error, "denied"}}},
            {:operation_failed, :disconnected}
          ] do
        :ok = Rebus.MatchSubscription.delete_state(conn)

        {state, token} =
          operation_state(fresh_worker_state(conn), key, rule, :initial_cleanup,
            status: :cleaning
          )

        assert {:noreply, _state} = Worker.handle_info({:operation_result, token, result}, state)
      end

      :ok = Rebus.MatchSubscription.delete_state(conn)

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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      state = fresh_worker_state(conn)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      owner_monitor = make_ref()
      owner_ref = make_ref()

      owner_lost = %{
        state
        | owner_monitors: %{owner_monitor => owner_ref},
          ref_monitors: %{owner_ref => owner_monitor}
      }

      assert {:noreply, _state} =
               Worker.handle_info({:DOWN, owner_monitor, :process, self(), :normal}, owner_lost)

      request_id = make_ref()
      request_monitor = make_ref()
      timer = Process.send_after(self(), :unused_request_timer, 10_000)

      request_lost = %{
        state
        | requests: %{
            request_id => %{
              from: {self(), make_ref()},
              owner: self(),
              key: key,
              timer: timer,
              monitor: request_monitor
            }
          },
          request_monitors: %{request_monitor => request_id}
      }

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:DOWN, request_monitor, :process, self(), :normal},
                 request_lost
               )

      {state, token} = operation_state(state, key, rule, :add_new)
      operation_monitor = make_ref()
      state = put_in(state.operations[token].monitor, operation_monitor)
      state = %{state | operation_monitors: %{operation_monitor => token}}

      assert {:noreply, reset} =
               Worker.handle_info({:DOWN, operation_monitor, :process, self(), :killed}, state)

      assert reset.state_lost?
    end

    test "covers expired reuse and operation-failure completion branches" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)
      {state, request_id} = expired_request(state, key, :add)
      state = put_in(state.operations[token].request_id, request_id)

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token, {:added_existing, make_ref()}},
                 state
               )

      :ok = Rebus.MatchSubscription.delete_state(conn)
      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)

      assert {:noreply, reset} =
               Worker.handle_info(
                 {:operation_result, token, {:operation_failed, :disconnected}},
                 state
               )

      assert reset.state_lost?

      :ok = Rebus.MatchSubscription.delete_state(conn)

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
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :initial_cleanup, status: :cleaning)

      assert {:noreply, _state} =
               Worker.handle_info({:operation_result, token, :handlers_cleared}, state)

      :ok = Rebus.MatchSubscription.delete_state(conn)
      state = put_rule_state(fresh_worker_state(conn), key, rule, [])
      timer = Process.send_after(self(), {:retry_recovery, key}, 10_000)
      state = put_in(state.rules[key].status, :recovering)
      state = put_in(state.rules[key].recovery_kind, :rule)
      state = put_in(state.rules[key].retry_timer, timer)

      assert {:noreply, _state} = Worker.handle_info({:retry_recovery, key}, state)
    end

    test "covers worker no-ops and stale recovery state" do
      {:ok, conn} = Agent.start_link(fn -> :connection_placeholder end)
      rule = test_rule()
      key = MatchRule.to_string(rule)

      on_exit(fn ->
        if Process.alive?(conn), do: Agent.stop(conn)
        _ = Rebus.MatchSubscription.delete_state(conn)
      end)

      assert {:error, :timeout} =
               Worker.call(self(), :ignored, System.monotonic_time(:millisecond) - 1, 0)

      assert {:error, :disconnected} =
               Worker.call(self(), :ignored, System.monotonic_time(:millisecond) + 1_000, 0)

      silent =
        spawn(fn ->
          receive do
            _message -> Process.sleep(50)
          end
        end)

      assert {:error, :timeout} =
               Worker.call(silent, :ignored, System.monotonic_time(:millisecond) + 10, 0)

      request_id = make_ref()
      request_monitor = Process.monitor(self())
      timer = Process.send_after(self(), :unused_terminate_timer, 10_000)

      state = %{
        fresh_worker_state(conn)
        | requests: %{
            request_id => %{
              from: {self(), make_ref()},
              owner: self(),
              key: key,
              timer: timer,
              monitor: request_monitor
            }
          }
      }

      assert :ok = Worker.terminate(:normal, state)

      {state, token} = operation_state(fresh_worker_state(conn), key, rule, :add_existing)
      state = %{state | rules: %{}}

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token, {:add_existing_failed, {:error, :disconnected}}},
                 state
               )

      :ok = Rebus.MatchSubscription.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      timer = Process.send_after(self(), {:retry_recovery, key}, 10_000)
      state = put_in(state.rules[key].retry_timer, timer)

      assert {:noreply, _state} =
               Worker.handle_info(
                 {:operation_result, token,
                  {:definitive_bus_error, {:error, {:bus_error, "denied"}}}},
                 state
               )

      :ok = Rebus.MatchSubscription.delete_state(conn)

      {state, token} =
        operation_state(fresh_worker_state(conn), key, rule, :recovery, status: :recovering)

      state = put_in(state.rules[key].queue, [make_ref()])

      assert {:noreply, _state} = Worker.handle_info({:operation_result, token, :cleared}, state)
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

      assert :ok = Rebus.close(connection)
      refute_receive {^server, %Message{header_fields: %{member: "RemoveMatch"}}}, 150
      send(owner, :stop)
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

  defp assert_add_match(server, rule) do
    assert_receive {^server,
                    %Message{
                      type: :method_call,
                      header_fields: %{
                        destination: "org.freedesktop.DBus",
                        interface: "org.freedesktop.DBus",
                        member: "AddMatch",
                        path: "/org/freedesktop/DBus"
                      },
                      body: [rule_string]
                    } = message},
                   1_000

    assert rule_string == MatchRule.to_string(rule)
    message
  end

  defp assert_remove_match(server, rule, timeout \\ 1_000) do
    assert_receive {^server,
                    %Message{
                      type: :method_call,
                      header_fields: %{member: "RemoveMatch"},
                      body: [rule_string]
                    } = message},
                   timeout

    assert rule_string == MatchRule.to_string(rule)
    message
  end

  defp assert_any_remove_match(server, timeout) do
    assert_receive {^server,
                    %Message{type: :method_call, header_fields: %{member: "RemoveMatch"}} =
                      message},
                   timeout

    message
  end

  defp method_return(serial), do: Message.new!(:method_return, reply_serial: serial)

  defp rule_status(connection, rule) do
    case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
      [{worker, _value}] ->
        state = :sys.get_state(worker)

        case Map.get(state.rules, MatchRule.to_string(rule)) do
          nil -> nil
          entry -> entry.status
        end

      [] ->
        nil
    end
  end

  defp rule_ref_count(connection, rule) do
    case Registry.lookup(Rebus.MatchSubscription.Registry, connection) do
      [{worker, _value}] ->
        case Map.get(:sys.get_state(worker).rules, MatchRule.to_string(rule)) do
          %{refs: refs} -> MapSet.size(refs)
          nil -> 0
        end

      [] ->
        0
    end
  end

  defp state_table, do: Store.table()

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

  # Every kill in this module lands under `Rebus.MatchSubscription.Supervisor`
  # or the `Rebus.MatchSubscription` dynamic supervisor beneath it, and each of
  # those carries the default budget of three restarts in five seconds. The
  # tests here are synchronous, so a kill in one test still counts against the
  # next. A kill made once `supervisor` has no budget left does not restart the
  # target locally: it takes `supervisor` down, and the escalation eventually
  # restarts `Rebus.MatchSubscription.Store`, replacing the state table these
  # tests assert on. Waiting for room first keeps each kill local. The common
  # case has room and does not wait.
  defp kill_supervised(pid, supervisor) when is_pid(pid) do
    assert wait_until(
             fn -> not restart_budget_full?(supervisor) end,
             @restart_room_attempts
           ),
           "restart budget of #{inspect(supervisor)} did not free up"

    Process.exit(pid, :kill)
  end

  defp restart_budget_full?(name) do
    case Process.whereis(name) do
      nil -> false
      pid -> pid |> :sys.get_state() |> budget_full?()
    end
  end

  # `DynamicSupervisor` keeps its budget in a struct, so the fields are named.
  # The OTP `supervisor` record is positional: intensity, period and the list
  # of restart timestamps sit at positions 5, 6 and 7 as verified on OTP 27 and
  # 28, and the matrix also runs OTP 29. Any shape not recognised here is
  # false, so the kill proceeds at once and degrades to the pre-fix behaviour
  # rather than failing.
  defp budget_full?(%{max_restarts: max, max_seconds: period, restarts: restarts})
       when is_integer(max) and is_integer(period) and is_list(restarts),
       do: recent_restarts(restarts, period) >= max

  defp budget_full?(state)
       when is_tuple(state) and tuple_size(state) > 7 and elem(state, 0) == :state do
    intensity = elem(state, 5)
    period = elem(state, 6)
    restarts = elem(state, 7)

    is_integer(intensity) and is_integer(period) and is_list(restarts) and
      recent_restarts(restarts, period) >= intensity
  end

  defp budget_full?(_state), do: false

  # The supervisors record restart times with `:erlang.monotonic_time(1)` and
  # only prune the list when they add to it, so age the entries out here.
  defp recent_restarts(restarts, period) do
    now = System.monotonic_time(:second)
    Enum.count(restarts, &(is_integer(&1) and now <= &1 + period))
  end

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

  defp fresh_worker_state(conn) do
    {:ok, state} = Worker.init(conn)
    state
  end

  defp put_rule_state(state, key, rule, queue) do
    rule_state = %{rule_state(rule) | queue: queue}

    %{state | rules: %{key => rule_state}}
  end

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

  defp persistence_changes(key) do
    %{
      dirty_rules: MapSet.new([key]),
      removed_rules: MapSet.new(),
      dirty_subscriptions: MapSet.new(),
      removed_subscriptions: MapSet.new()
    }
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

  defp expired_request(state, key, kind) do
    request_id = make_ref()
    monitor = Process.monitor(self())
    timer = Process.send_after(self(), :unused_expired_request_timer, 10_000)

    request = %{
      from: {self(), make_ref()},
      owner: self(),
      kind: kind,
      key: key,
      ref: nil,
      deadline: System.monotonic_time(:millisecond) - 1,
      timer: timer,
      monitor: monitor
    }

    {%{
       state
       | requests: %{request_id => request},
         request_monitors: %{monitor => request_id}
     }, request_id}
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
