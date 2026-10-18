defmodule Rebus.BusTest do
  use ExUnit.Case

  alias Rebus.Bus
  alias Rebus.Message
  alias Rebus.TestServer

  @driver "org.freedesktop.DBus"
  @driver_path "/org/freedesktop/DBus"
  @name "org.example.Calculator"
  @unique_name ":1.100"

  describe "request_name/4" do
    setup [:server_setup, :client_setup]

    test "asks the driver for the name with the flags as a bitmask", %{cli: cli, svr: svr} do
      task =
        Task.async(fn ->
          Bus.request_name(cli, @name, [:allow_replacement, :replace_existing, :do_not_queue])
        end)

      request = await_driver_call(svr, "RequestName")

      assert %Message{
               header_fields: %{
                 destination: @driver,
                 path: @driver_path,
                 interface: @driver
               },
               body: [@name, 7]
             } = request

      assert Message.signature(request) == "su"

      :ok = reply(svr, request, 1)
      assert {:ok, :primary_owner} = Task.await(task)
    end

    test "sends no flags by default", %{cli: cli, svr: svr} do
      task = Task.async(fn -> Bus.request_name(cli, @name) end)

      request = await_driver_call(svr, "RequestName")
      assert request.body == [@name, 0]

      :ok = reply(svr, request, 1)
      assert {:ok, :primary_owner} = Task.await(task)
    end

    for {code, result} <- [{1, :primary_owner}, {2, :in_queue}, {3, :exists}, {4, :already_owner}] do
      test "decodes reply #{code} as #{inspect(result)}", %{cli: cli, svr: svr} do
        task = Task.async(fn -> Bus.request_name(cli, @name, [], owner: nil) end)

        :ok = reply(svr, await_driver_call(svr, "RequestName"), unquote(code))
        assert {:ok, unquote(result)} = Task.await(task)
      end
    end

    test "rejects invalid arguments without contacting the bus", %{cli: cli, svr: svr} do
      assert {:error, :invalid_name} = Bus.request_name(cli, @unique_name)
      assert {:error, :invalid_name} = Bus.request_name(cli, "nodots")
      assert {:error, :invalid_name} = Bus.request_name(cli, :name)
      assert {:error, :invalid_flags} = Bus.request_name(cli, @name, [:bogus])
      assert {:error, :invalid_flags} = Bus.request_name(cli, @name, :do_not_queue)
      assert {:error, :invalid_owner} = Bus.request_name(cli, @name, [], owner: :me)
      assert {:error, :invalid_timeout} = Bus.request_name(cli, @name, [], timeout: -1)

      refute_receive {^svr, %Message{header_fields: %{member: "RequestName"}}}, 100
      assert :sys.get_state(cli).handlers == %{}
    end

    test "returns a refusal by the bus as a bus error", %{cli: cli, svr: svr} do
      task = Task.async(fn -> Bus.request_name(cli, @name) end)

      request = await_driver_call(svr, "RequestName")

      :ok =
        TestServer.push(
          svr,
          Message.new!(:error,
            reply_serial: request.serial,
            error_name: "org.freedesktop.DBus.Error.AccessDenied",
            signature: "s",
            body: ["denied"]
          )
        )

      assert {:error, {:bus_error, "org.freedesktop.DBus.Error.AccessDenied"}} = Task.await(task)
      assert wait_until(fn -> :sys.get_state(cli).handlers == %{} end)
    end

    test "rejects a reply that is not a known result code", %{cli: cli, svr: svr} do
      task = Task.async(fn -> Bus.request_name(cli, @name, [], owner: nil) end)
      :ok = reply(svr, await_driver_call(svr, "RequestName"), 9)
      assert {:error, :invalid_bus_reply} = Task.await(task)

      task = Task.async(fn -> Bus.request_name(cli, @name, [], owner: nil) end)
      request = await_driver_call(svr, "RequestName")

      :ok =
        TestServer.push(
          svr,
          Message.new!(:method_return, reply_serial: request.serial, signature: "i", body: [1])
        )

      assert {:error, :invalid_bus_reply} = Task.await(task)
    end

    test "times out with the whole request's budget", %{cli: cli, svr: svr} do
      assert {:error, :timeout} = Bus.request_name(cli, @name, [], timeout: 200)
      assert %Message{} = await_driver_call(svr, "RequestName")

      # The bus may still have applied the request, so the owner keeps
      # listening for the name.
      assert [%{name: @name}] = Map.values(:sys.get_state(cli).handlers)
    end
  end

  describe "release_name/3" do
    setup [:server_setup, :client_setup]

    for {code, result} <- [{1, :released}, {2, :non_existent}, {3, :not_owner}] do
      test "decodes reply #{code} as #{inspect(result)}", %{cli: cli, svr: svr} do
        task = Task.async(fn -> Bus.release_name(cli, @name) end)

        request = await_driver_call(svr, "ReleaseName")

        assert %Message{
                 header_fields: %{destination: @driver, path: @driver_path, interface: @driver},
                 body: [@name]
               } = request

        assert Message.signature(request) == "s"

        :ok = reply(svr, request, unquote(code))
        assert {:ok, unquote(result)} = Task.await(task)
      end
    end

    test "rejects invalid arguments without contacting the bus", %{cli: cli, svr: svr} do
      assert {:error, :invalid_name} = Bus.release_name(cli, @unique_name)
      assert {:error, :invalid_timeout} = Bus.release_name(cli, @name, timeout: :soon)

      refute_receive {^svr, %Message{header_fields: %{member: "ReleaseName"}}}, 100
    end
  end

  describe "ownership notices" do
    setup [:server_setup, :client_setup]

    test "reach the owner of the name requested", %{cli: cli, svr: svr} do
      owner = self()
      task = Task.async(fn -> Bus.request_name(cli, @name, [], owner: owner) end)
      request = await_driver_call(svr, "RequestName")

      # The driver announces the name before it answers the request.
      :ok = TestServer.push(svr, name_signal("NameAcquired", @name))
      :ok = reply(svr, request, 1)
      assert {:ok, :primary_owner} = Task.await(task)

      assert_receive {:rebus_name, ^cli, :acquired, @name}, 1_000

      :ok = TestServer.push(svr, name_signal("NameLost", @name))
      assert_receive {:rebus_name, ^cli, :lost, @name}, 1_000
    end

    test "default to the calling process", %{cli: cli, svr: svr} do
      task =
        Task.async(fn ->
          result = Bus.request_name(cli, @name)

          receive do
            {:rebus_name, ^cli, event, name} -> {result, event, name}
          after
            1_000 -> {result, :no_notice}
          end
        end)

      request = await_driver_call(svr, "RequestName")
      :ok = TestServer.push(svr, name_signal("NameAcquired", @name))
      :ok = reply(svr, request, 1)

      assert {{:ok, :primary_owner}, :acquired, @name} = Task.await(task)
    end

    test "are not sent with owner: nil", %{cli: cli, svr: svr} do
      :ok = request(cli, svr, [owner: nil], 1)

      assert :sys.get_state(cli).handlers == %{}
      :ok = TestServer.push(svr, name_signal("NameAcquired", @name))
      refute_receive {:rebus_name, _cli, _event, _name}, 200
    end

    test "are sent once to an owner that requested the name twice", %{cli: cli, svr: svr} do
      :ok = request(cli, svr, [], 1)
      :ok = request(cli, svr, [], 4)

      :ok = TestServer.push(svr, name_signal("NameLost", @name))
      assert_receive {:rebus_name, ^cli, :lost, @name}, 1_000
      refute_receive {:rebus_name, ^cli, :lost, @name}, 200
    end

    test "ignore every other signal", %{cli: cli, svr: svr} do
      :ok = request(cli, svr, [], 2)

      # Another name, the connection's own unique name, a broadcast lookalike
      # from a peer, and NameOwnerChanged, which also carries the name first.
      :ok = TestServer.push(svr, name_signal("NameAcquired", "org.example.Other"))
      :ok = TestServer.push(svr, name_signal("NameAcquired", @unique_name))

      :ok =
        TestServer.push(
          svr,
          Message.new!(:signal,
            sender: ":1.99",
            path: @driver_path,
            interface: @driver,
            member: "NameLost",
            signature: "s",
            body: [@name]
          )
        )

      :ok =
        TestServer.push(
          svr,
          Message.new!(:signal,
            sender: @driver,
            path: @driver_path,
            interface: @driver,
            member: "NameOwnerChanged",
            signature: "sss",
            body: [@name, "", @unique_name]
          )
        )

      refute_receive {:rebus_name, _cli, _event, _name}, 200
    end

    test "leave signal handlers receiving the signals themselves", %{cli: cli, svr: svr} do
      {:ok, ref} = Rebus.add_signal_handler(cli)
      :ok = request(cli, svr, [], 1)

      :ok = TestServer.push(svr, name_signal("NameAcquired", @name))

      assert_receive {:rebus_name, ^cli, :acquired, @name}, 1_000
      assert_receive {^ref, %Message{header_fields: %{member: "NameAcquired"}, body: [@name]}}
    end

    test "stop for a first request the bus answered with exists", %{cli: cli, svr: svr} do
      :ok = request(cli, svr, [:do_not_queue], 3)

      assert wait_until(fn -> :sys.get_state(cli).handlers == %{} end)
    end

    test "continue for an earlier request when a later one is answered with exists", %{
      cli: cli,
      svr: svr
    } do
      :ok = request(cli, svr, [], 2)
      :ok = request(cli, svr, [:do_not_queue], 3)

      :ok = TestServer.push(svr, name_signal("NameAcquired", @name))
      assert_receive {:rebus_name, ^cli, :acquired, @name}, 1_000
    end

    test "stop once the name is released", %{cli: cli, svr: svr} do
      :ok = request(cli, svr, [], 1)

      task = Task.async(fn -> Bus.release_name(cli, @name) end)
      release = await_driver_call(svr, "ReleaseName")

      # The driver announces the loss before it answers the release.
      :ok = TestServer.push(svr, name_signal("NameLost", @name))
      :ok = reply(svr, release, 1)
      assert {:ok, :released} = Task.await(task)

      assert_receive {:rebus_name, ^cli, :lost, @name}, 1_000
      assert wait_until(fn -> :sys.get_state(cli).handlers == %{} end)

      :ok = TestServer.push(svr, name_signal("NameAcquired", @name))
      refute_receive {:rebus_name, _cli, _event, _name}, 200
    end

    test "continue when the release fails", %{cli: cli, svr: svr} do
      :ok = request(cli, svr, [], 1)

      assert {:error, :timeout} = Bus.release_name(cli, @name, timeout: 200)
      assert %Message{} = await_driver_call(svr, "ReleaseName")

      :ok = TestServer.push(svr, name_signal("NameLost", @name))
      assert_receive {:rebus_name, ^cli, :lost, @name}, 1_000
    end

    test "stop when the owner exits", %{cli: cli, svr: svr} do
      owner = spawn(fn -> Process.sleep(:infinity) end)
      :ok = request(cli, svr, [owner: owner], 1)
      assert [%{pid: ^owner}] = Map.values(:sys.get_state(cli).handlers)

      Process.exit(owner, :kill)
      assert wait_until(fn -> :sys.get_state(cli).handlers == %{} end)
    end
  end

  describe "a peer-to-peer connection" do
    setup [:server_setup]

    test "has no names to request or release", %{svr: svr} do
      :ok = TestServer.set_auto_hello(svr, false)
      {:ok, addr} = TestServer.get_listen_addr(svr)
      {:ok, cli} = Rebus.connect(addr, bus: false)

      assert {:error, :not_a_bus} = Bus.request_name(cli, @name)
      assert {:error, :not_a_bus} = Bus.release_name(cli, @name)

      refute_receive {^svr, %Message{type: :method_call}}, 200
      assert :sys.get_state(cli).handlers == %{}
      assert :ok = Rebus.close(cli)
    end
  end

  defp server_setup(_) do
    {:ok, svr} = start_supervised({TestServer, tap: self()})
    %{svr: svr}
  end

  defp client_setup(%{svr: svr}) do
    {:ok, addr} = TestServer.get_listen_addr(svr)
    {:ok, cli} = Rebus.connect(addr)
    assert_receive {^svr, %Message{header_fields: %{member: "Hello"}}}, 1_000
    on_exit(fn -> Rebus.close(cli) end)
    %{cli: cli}
  end

  # Requests the name for the test process, which owns it unless `opts`
  # names another owner, and answers the request with `code`. Atoms in
  # `opts` are the request's flags.
  defp request(cli, svr, opts, code) do
    {flags, opts} = Enum.split_with(opts, &is_atom/1)
    opts = Keyword.put_new(opts, :owner, self())

    task = Task.async(fn -> Bus.request_name(cli, @name, flags, opts) end)
    :ok = reply(svr, await_driver_call(svr, "RequestName"), code)
    {:ok, _result} = Task.await(task)
    :ok
  end

  defp await_driver_call(svr, member) do
    assert_receive {^svr, %Message{type: :method_call, header_fields: %{member: ^member}} = call},
                   1_000

    call
  end

  defp reply(svr, %Message{serial: serial}, code) do
    TestServer.push(
      svr,
      Message.new!(:method_return, reply_serial: serial, signature: "u", body: [code])
    )
  end

  defp name_signal(member, name) do
    Message.new!(:signal,
      sender: @driver,
      path: @driver_path,
      interface: @driver,
      member: member,
      destination: @unique_name,
      signature: "s",
      body: [name]
    )
  end

  defp wait_until(predicate, attempts \\ 100)

  defp wait_until(predicate, 0), do: predicate.()

  defp wait_until(predicate, attempts) do
    if predicate.() do
      true
    else
      Process.sleep(10)
      wait_until(predicate, attempts - 1)
    end
  end
end
