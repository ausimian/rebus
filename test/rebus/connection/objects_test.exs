defmodule Rebus.Connection.ObjectsTest do
  use ExUnit.Case, async: true

  alias Rebus.Connection.Objects
  alias Rebus.Connection.Objects.Export
  alias Rebus.Interface

  @calculator Interface.new!("org.example.Calculator", methods: [{"Add", in: ["i", "i"]}])
  @display Interface.new!("org.example.Display", methods: [{"Show", in: ["s"]}])

  describe "add/2" do
    test "records an export and monitors its handler" do
      handler = spawn_handler()
      {:ok, objects} = Objects.add(Objects.new(), export("/org/example/Calc", handler: handler))

      assert {:object, %{"org.example.Calculator" => %Export{} = export}} =
               Objects.resolve(objects, "/org/example/Calc")

      assert export.handler == handler
      assert export.interfaces == %{"org.example.Calculator" => @calculator}
      assert export.call_timeout == 60_000
      assert {:monitors, [{:process, ^handler}]} = Process.info(self(), :monitors)
    end

    test "lets exports with different interfaces share a path" do
      {:ok, objects} = Objects.add(Objects.new(), export("/obj"))
      {:ok, objects} = Objects.add(objects, export("/obj", interfaces: [@display]))

      assert {:object, interfaces} = Objects.resolve(objects, "/obj")
      assert Map.keys(interfaces) == ["org.example.Calculator", "org.example.Display"]
    end

    test "refuses an interface already exported at the path" do
      {:ok, objects} = Objects.add(Objects.new(), export("/obj"))

      assert {:error, {:already_exported, "/obj", "org.example.Calculator"}} =
               Objects.add(objects, export("/obj", interfaces: [@display, @calculator]))
    end

    test "keeps exact and subtree exports of the same path apart" do
      {:ok, objects} = Objects.add(Objects.new(), export("/obj"))

      assert {:ok, _objects} = Objects.add(objects, export("/obj", subtree?: true))
    end
  end

  describe "resolve/2" do
    test "finds nothing in an empty table" do
      assert Objects.resolve(Objects.new(), "/") == :unknown
      assert Objects.resolve(Objects.new(), "/org/example") == :unknown
    end

    test "resolves an exact export only at its own path" do
      {:ok, objects} = Objects.add(Objects.new(), export("/org/example/Calc"))

      assert {:object, _interfaces} = Objects.resolve(objects, "/org/example/Calc")
      assert Objects.resolve(objects, "/org/example/Calc/child") == :unknown
      assert Objects.resolve(objects, "/org/example/Calculator") == :unknown
    end

    test "reports every proper prefix of an exported path as an intermediate node" do
      {:ok, objects} = Objects.add(Objects.new(), export("/org/example/Calc"))

      assert Objects.resolve(objects, "/") == :intermediate
      assert Objects.resolve(objects, "/org") == :intermediate
      assert Objects.resolve(objects, "/org/example") == :intermediate
      # A string prefix that is not a path prefix is no ancestor.
      assert Objects.resolve(objects, "/org/exam") == :unknown
    end

    test "resolves a subtree export at its own path and every path below it" do
      {:ok, objects} = Objects.add(Objects.new(), export("/org/example", subtree?: true))

      for path <- ["/org/example", "/org/example/a", "/org/example/a/b"] do
        assert {:object, %{"org.example.Calculator" => %Export{path: "/org/example"}}} =
                 Objects.resolve(objects, path)
      end

      assert Objects.resolve(objects, "/org") == :intermediate
      assert Objects.resolve(objects, "/org/examples") == :unknown
    end

    test "resolves a subtree export of the root for every path" do
      {:ok, objects} = Objects.add(Objects.new(), export("/", subtree?: true))

      assert {:object, _interfaces} = Objects.resolve(objects, "/")
      assert {:object, _interfaces} = Objects.resolve(objects, "/any/path")
    end

    # The path is peer-supplied. Rescanning it once per ancestor is quadratic
    # in its length and would blow this test's timeout; one pass per exported
    # subtree takes milliseconds.
    @tag timeout: 5_000
    test "resolves a path with very many components in linear time" do
      deep = String.duplicate("/a", 200_000)
      {:ok, objects} = Objects.add(Objects.new(), export("/a/a", subtree?: true))
      {:ok, objects} = Objects.add(objects, export("/b", subtree?: true))

      assert {:object, %{"org.example.Calculator" => %Export{path: "/a/a"}}} =
               Objects.resolve(objects, deep)

      assert Objects.resolve(objects, "/c" <> deep) == :unknown
    end

    test "prefers the longest covering subtree" do
      {:ok, objects} = Objects.add(Objects.new(), export("/a", subtree?: true))

      {:ok, objects} =
        Objects.add(objects, export("/a/b", subtree?: true, interfaces: [@display]))

      assert {:object, %{"org.example.Display" => _export}} = Objects.resolve(objects, "/a/b/c")
      assert {:object, %{"org.example.Calculator" => _export}} = Objects.resolve(objects, "/a/c")
    end

    test "prefers an exact export over a subtree that covers its path" do
      {:ok, objects} = Objects.add(Objects.new(), export("/a", subtree?: true))
      {:ok, objects} = Objects.add(objects, export("/a/b", interfaces: [@display]))

      assert {:object, interfaces} = Objects.resolve(objects, "/a/b")
      assert Map.keys(interfaces) == ["org.example.Display"]

      assert {:object, %{"org.example.Calculator" => _export}} =
               Objects.resolve(objects, "/a/b/c")
    end
  end

  describe "remove/2" do
    test "removes the export and releases its monitor" do
      ref = make_ref()
      {:ok, objects} = Objects.add(Objects.new(), export("/obj", ref: ref))

      assert {%Export{ref: ^ref}, objects} = Objects.remove(objects, ref)
      assert Objects.resolve(objects, "/obj") == :unknown
      assert objects == Objects.new()
      assert {:monitors, []} = Process.info(self(), :monitors)
    end

    test "leaves the other exports at the path in place" do
      ref = make_ref()
      {:ok, objects} = Objects.add(Objects.new(), export("/obj", ref: ref))
      {:ok, objects} = Objects.add(objects, export("/obj", interfaces: [@display]))

      {_export, objects} = Objects.remove(objects, ref)

      assert {:object, interfaces} = Objects.resolve(objects, "/obj")
      assert Map.keys(interfaces) == ["org.example.Display"]
    end

    test "frees the interface for a later export" do
      ref = make_ref()
      {:ok, objects} = Objects.add(Objects.new(), export("/obj", ref: ref))
      {_export, objects} = Objects.remove(objects, ref)

      assert {:ok, _objects} = Objects.add(objects, export("/obj"))
    end

    test "finds nothing for an unknown reference" do
      {:ok, objects} = Objects.add(Objects.new(), export("/obj"))

      assert {nil, ^objects} = Objects.remove(objects, make_ref())
    end
  end

  describe "remove_by_monitor/2" do
    test "removes the export whose handler went down" do
      handler = spawn_handler()
      ref = make_ref()
      {:ok, objects} = Objects.add(Objects.new(), export("/obj", ref: ref, handler: handler))
      {:ok, objects} = Objects.add(objects, export("/other", interfaces: [@display]))

      Process.exit(handler, :kill)
      assert_receive {:DOWN, monitor_ref, :process, ^handler, :killed}, 1_000

      assert {%Export{ref: ^ref}, objects} = Objects.remove_by_monitor(objects, monitor_ref)
      assert Objects.resolve(objects, "/obj") == :unknown
      assert {:object, _interfaces} = Objects.resolve(objects, "/other")
    end

    test "finds nothing for a monitor it does not hold" do
      {:ok, objects} = Objects.add(Objects.new(), export("/obj"))

      assert {nil, ^objects} = Objects.remove_by_monitor(objects, make_ref())
    end
  end

  defp export(path, opts \\ []) do
    %{
      ref: Keyword.get_lazy(opts, :ref, &make_ref/0),
      path: path,
      subtree?: Keyword.get(opts, :subtree?, false),
      interfaces: Keyword.get(opts, :interfaces, [@calculator]),
      handler: Keyword.get(opts, :handler, self()),
      call_timeout: 60_000
    }
  end

  defp spawn_handler do
    pid = spawn(fn -> Process.sleep(:infinity) end)
    on_exit(fn -> Process.exit(pid, :kill) end)
    pid
  end
end
