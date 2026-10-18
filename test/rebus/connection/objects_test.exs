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

  describe "introspect/2" do
    test "refuses a path with nothing exported at it or below it" do
      assert Objects.introspect(Objects.new(), "/") == :error

      {:ok, objects} = Objects.add(Objects.new(), export("/org/example/Calc"))
      assert Objects.introspect(objects, "/org/example/Calc/below") == :error
      assert Objects.introspect(objects, "/org/ex") == :error
    end

    test "lists exported and subtree paths as children, once each" do
      {:ok, objects} = Objects.add(Objects.new(), export("/org/a"))
      {:ok, objects} = Objects.add(objects, export("/org/a", interfaces: [@display]))
      {:ok, objects} = Objects.add(objects, export("/org/a/x/y"))
      {:ok, objects} = Objects.add(objects, export("/org/b", subtree?: true))
      {:ok, objects} = Objects.add(objects, export("/orgx"))

      assert {:ok, xml, objects} = Objects.introspect(objects, "/org")
      assert children(xml) == ["a", "b"]
      assert {:ok, xml, _objects} = Objects.introspect(objects, "/org/a")
      assert children(xml) == ["x"]
      assert {:ok, xml, _objects} = Objects.introspect(objects, "/")
      assert children(xml) == ["org", "orgx"]
    end

    test "lists a root export's children" do
      {:ok, objects} = Objects.add(Objects.new(), export("/"))
      {:ok, objects} = Objects.add(objects, export("/a/b"))

      assert {:ok, xml, _objects} = Objects.introspect(objects, "/")
      assert xml =~ ~s(<interface name="org.example.Calculator">)
      assert children(xml) == ["a"]
    end

    test "answers from the cache until the table changes" do
      {:ok, objects} = Objects.add(Objects.new(), export("/obj"))
      {:ok, xml, objects} = Objects.introspect(objects, "/obj")

      # A sentinel in place of the cached document shows which one is served.
      objects = %{objects | introspection: %{{:path, "/obj"} => "cached"}}
      assert {:ok, "cached", ^objects} = Objects.introspect(objects, "/obj")

      {:ok, added} = Objects.add(objects, export("/other", interfaces: [@display]))
      assert added.introspection == %{}
      assert {:ok, ^xml, _objects} = Objects.introspect(added, "/obj")
    end

    test "forgets cached documents on unexport and when a handler goes down" do
      handler = spawn_handler()
      ref = make_ref()
      {:ok, objects} = Objects.add(Objects.new(), export("/obj", ref: ref))
      {:ok, objects} = Objects.add(objects, export("/other", handler: handler))
      {:ok, _xml, objects} = Objects.introspect(objects, "/")
      assert map_size(objects.introspection) == 1

      assert {%Export{}, removed} = Objects.remove(objects, ref)
      assert removed.introspection == %{}

      Process.exit(handler, :kill)
      assert_receive {:DOWN, monitor_ref, :process, ^handler, :killed}, 1_000
      assert {%Export{}, down} = Objects.remove_by_monitor(objects, monitor_ref)
      assert down.introspection == %{}
    end

    test "shares one cached document between every path a subtree serves" do
      {:ok, objects} = Objects.add(Objects.new(), export("/tree", subtree?: true))
      {:ok, objects} = Objects.add(objects, export("/tree/a/b"))

      objects =
        Enum.reduce(["/tree", "/tree/a", "/tree/c/d"], objects, fn path, objects ->
          assert {:ok, xml, objects} = Objects.introspect(objects, path)
          assert xml =~ ~s(<interface name="org.example.Calculator">)
          assert children(xml) == []
          objects
        end)

      assert Map.keys(objects.introspection) == [{:subtree, "/tree"}]
    end

    test "keys a cached document by a copy of the path it was asked for" do
      # The VM copies a sub-binary of 64 bytes or fewer by itself, so only a
      # longer path can still point into the buffer it was read from.
      node = "/" <> String.duplicate("a", 100)
      {:ok, objects} = Objects.add(Objects.new(), export(node <> "/Calc"))
      buffer = node <> :binary.copy("x", 4096)
      path = binary_part(buffer, 0, byte_size(node))
      assert :binary.referenced_byte_size(path) > byte_size(path)

      {:ok, _xml, objects} = Objects.introspect(objects, path)

      assert [{:path, key}] = Map.keys(objects.introspection)
      assert key == path
      assert :binary.referenced_byte_size(key) == byte_size(key)
    end
  end

  # Child node names in an introspection document.
  defp children(xml) do
    ~r/^  <node name="([^"]+)"\/>$/m
    |> Regex.scan(xml, capture: :all_but_first)
    |> List.flatten()
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
