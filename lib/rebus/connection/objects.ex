defmodule Rebus.Connection.Objects do
  @moduledoc false

  # The table of objects a connection exports, and the resolution of an
  # inbound call's path against it.
  #
  # An export binds a set of interfaces at one path to one handler process.
  # Several exports may share a path as long as no interface appears in more
  # than one of them, so different handlers can serve different interfaces of
  # the same object. A subtree export also serves every path below its own,
  # and is kept apart from exact exports: an exact export always wins for its
  # own path, and between nested subtrees the longest prefix wins.
  #
  # Like `Rebus.Connection.InboundCalls` this is not a pure structure: each
  # export owns the monitor on its handler, taken when it is added and
  # released when it is removed, so every way out releases it exactly once.
  # Only local code creates exports, so nothing a peer sends can grow the
  # table, and the linear scans below are bounded by what the application
  # exported.
  #
  # The table also answers `Introspect`, and caches each document it renders
  # until the next export or unexport. A cached document is keyed by the node
  # it describes, never by the path a peer asked for: every path a subtree
  # covers below its own shares the subtree's document, so the cache is
  # bounded by the table as well.

  use TypedStruct

  alias Rebus.Interface
  alias Rebus.Introspection
  alias Rebus.Introspection.Node

  defmodule Export do
    @moduledoc false

    use TypedStruct

    # One export: the interfaces it serves by name, the handler its calls go
    # to and the monitor on that handler, and the expiry its calls get.

    typedstruct enforce: true do
      field :ref, reference()
      field :path, binary()
      field :subtree?, boolean()
      field :interfaces, %{binary() => Interface.t()}
      field :handler, pid()
      field :monitor_ref, reference()
      field :call_timeout, pos_integer() | :infinity
    end
  end

  @typedoc "The fields a caller supplies for a new export."
  @type export :: %{
          required(:ref) => reference(),
          required(:path) => binary(),
          required(:subtree?) => boolean(),
          required(:interfaces) => [Interface.t()],
          required(:handler) => pid(),
          required(:call_timeout) => pos_integer() | :infinity
        }

  @typedoc """
  What a path resolves to: the interfaces of the object there, each with the
  export that serves it; an intermediate node, which is a proper prefix of an
  exported path with nothing exported on it; or nothing at all.
  """
  @type resolution :: {:object, %{binary() => Export.t()}} | :intermediate | :unknown

  typedstruct enforce: true do
    field :exports, %{reference() => Export.t()}, default: %{}
    # Export references by path, for exact and subtree exports separately.
    field :exact, %{binary() => [reference()]}, default: %{}
    field :subtrees, %{binary() => [reference()]}, default: %{}
    field :monitor_index, %{reference() => reference()}, default: %{}
    # Rendered introspection documents, emptied by every change to the table.
    field :introspection, %{{:path | :subtree, binary()} => binary()}, default: %{}
  end

  @spec new() :: t()
  def new, do: %__MODULE__{}

  # Adds an export and monitors its handler, unless one of its interfaces is
  # already exported at the same path by an export of the same kind.
  @spec add(t(), export()) ::
          {:ok, t()} | {:error, {:already_exported, binary(), binary()}}
  def add(%__MODULE__{} = objects, %{ref: ref, path: path, subtree?: subtree?} = export)
      when is_reference(ref) and is_binary(path) and is_boolean(subtree?) do
    case conflict(objects, path, subtree?, export.interfaces) do
      nil -> {:ok, insert(objects, export)}
      name -> {:error, {:already_exported, path, name}}
    end
  end

  # Unexport: the export leaves the table and its monitor is released, along
  # with any `:DOWN` it already queued.
  @spec remove(t(), reference()) :: {Export.t() | nil, t()}
  def remove(%__MODULE__{} = objects, ref) do
    case Map.fetch(objects.exports, ref) do
      {:ok, %Export{} = export} ->
        Process.demonitor(export.monitor_ref, [:flush])
        {export, delete(objects, export)}

      :error ->
        {nil, objects}
    end
  end

  # The handler of an export went down. Its monitor has already fired, so
  # there is nothing to release.
  @spec remove_by_monitor(t(), reference()) :: {Export.t() | nil, t()}
  def remove_by_monitor(%__MODULE__{} = objects, monitor_ref) do
    case Map.fetch(objects.monitor_index, monitor_ref) do
      {:ok, ref} ->
        export = Map.fetch!(objects.exports, ref)
        {export, delete(objects, export)}

      :error ->
        {nil, objects}
    end
  end

  @spec resolve(t(), binary()) :: resolution()
  def resolve(%__MODULE__{} = objects, path) when is_binary(path) do
    case locate(objects, path) do
      {:exact, refs} -> {:object, object(objects, refs)}
      {:subtree, prefix} -> {:object, object(objects, Map.fetch!(objects.subtrees, prefix))}
      other -> other
    end
  end

  # The introspection document for a path, from the cache when the node it
  # describes has not changed since it was rendered. An exported object lists
  # its interfaces and the standard ones, an intermediate node only
  # `Introspectable`, and both list their children. A path a subtree serves
  # lists none, because nothing can enumerate what a subtree's handler serves.
  @spec introspect(t(), binary()) :: {:ok, binary(), t()} | :error
  def introspect(%__MODULE__{} = objects, path) when is_binary(path) do
    case locate(objects, path) do
      :unknown ->
        :error

      location ->
        key = cache_key(location, path)

        case Map.fetch(objects.introspection, key) do
          {:ok, xml} ->
            {:ok, xml, objects}

          :error ->
            xml =
              objects |> node(location, path) |> Introspection.to_xml() |> IO.iodata_to_binary()

            {:ok, xml, %{objects | introspection: Map.put(objects.introspection, key, xml)}}
        end
    end
  end

  # Every path a subtree serves shares one document. Any other path that
  # resolves is an exported or intermediate path, which the table bounds. A
  # peer-supplied path is copied before it becomes a key, so a cached entry
  # cannot pin the receive buffer the path was parsed from.
  defp cache_key({:subtree, prefix}, _path), do: {:subtree, prefix}
  defp cache_key(_location, path), do: {:path, :binary.copy(path)}

  defp node(%__MODULE__{} = objects, {:exact, refs}, path),
    do: object_node(objects, refs, children(objects, path))

  defp node(%__MODULE__{} = objects, {:subtree, prefix}, _path),
    do: object_node(objects, Map.fetch!(objects.subtrees, prefix), [])

  defp node(%__MODULE__{} = objects, :intermediate, path),
    do: %Node{interfaces: [Interface.introspectable()], children: children(objects, path)}

  # Application interfaces by name, then the standard ones the connection
  # answers for every exported object.
  defp object_node(%__MODULE__{} = objects, refs, children) do
    exported =
      for {name, %Export{} = export} <- Enum.sort(object(objects, refs)),
          do: export.interfaces[name]

    standard = [Interface.peer(), Interface.introspectable(), Interface.properties()]
    %Node{interfaces: exported ++ standard, children: children}
  end

  # The next path element of every exported path strictly below `path`.
  defp children(%__MODULE__{} = objects, path) do
    prefix = descendant_prefix(path)
    size = byte_size(prefix)

    for exported <- Map.keys(objects.exact) ++ Map.keys(objects.subtrees),
        exported != path,
        <<^prefix::binary-size(^size), rest::binary>> <- [exported],
        uniq: true,
        do: rest |> :binary.split("/") |> hd()
  end

  # What a path names in the table.
  defp locate(%__MODULE__{} = objects, path) do
    case Map.fetch(objects.exact, path) do
      {:ok, refs} -> {:exact, refs}
      :error -> locate_subtree(objects, path)
    end
  end

  # The path is peer-supplied and its length is bounded only by the message
  # size, so the search runs over the exported subtrees, which local code
  # bounds, checking each against the path once, rather than over the path's
  # ancestors. Of the subtrees covering the path, the longest wins.
  defp locate_subtree(%__MODULE__{} = objects, path) do
    objects.subtrees
    |> Map.keys()
    |> Enum.filter(&covers?(&1, path))
    |> Enum.max_by(&byte_size/1, fn -> nil end)
    |> case do
      nil -> if intermediate?(objects, path), do: :intermediate, else: :unknown
      prefix -> {:subtree, prefix}
    end
  end

  # Every interface of the object, keyed by name, with the export serving it.
  # `add/2` keeps interface names unique per path and kind, so no entry here
  # shadows another.
  defp object(%__MODULE__{exports: exports}, refs) do
    for ref <- refs,
        %Export{} = export = Map.fetch!(exports, ref),
        name <- Map.keys(export.interfaces),
        into: %{},
        do: {name, export}
  end

  # Whether a subtree exported at `prefix` serves `path`: the path itself or
  # any path below it, and every path for the root.
  defp covers?("/", _path), do: true

  defp covers?(prefix, path) do
    size = byte_size(prefix)

    case path do
      ^prefix -> true
      <<^prefix::binary-size(^size), ?/, _rest::binary>> -> true
      _other -> false
    end
  end

  defp intermediate?(%__MODULE__{} = objects, path) do
    prefix = descendant_prefix(path)

    Enum.any?(
      Map.keys(objects.exact) ++ Map.keys(objects.subtrees),
      &(&1 != path and String.starts_with?(&1, prefix))
    )
  end

  # What every path strictly below `path` starts with.
  defp descendant_prefix("/"), do: "/"
  defp descendant_prefix(path), do: path <> "/"

  defp conflict(%__MODULE__{} = objects, path, subtree?, interfaces) do
    taken =
      objects
      |> paths(subtree?)
      |> Map.get(path, [])
      |> Enum.flat_map(&Map.keys(Map.fetch!(objects.exports, &1).interfaces))

    Enum.find_value(interfaces, fn %Interface{name: name} -> if name in taken, do: name end)
  end

  defp insert(%__MODULE__{} = objects, export) do
    monitor_ref = Process.monitor(export.handler)

    entry = %Export{
      ref: export.ref,
      path: export.path,
      subtree?: export.subtree?,
      interfaces: Map.new(export.interfaces, &{&1.name, &1}),
      handler: export.handler,
      monitor_ref: monitor_ref,
      call_timeout: export.call_timeout
    }

    objects
    |> update_paths(
      export.subtree?,
      &Map.update(&1, export.path, [export.ref], fn refs -> [export.ref | refs] end)
    )
    |> Map.update!(:exports, &Map.put(&1, export.ref, entry))
    |> Map.update!(:monitor_index, &Map.put(&1, monitor_ref, export.ref))
    |> Map.put(:introspection, %{})
  end

  defp delete(%__MODULE__{} = objects, %Export{} = export) do
    objects
    |> update_paths(export.subtree?, &delete_path(&1, export.path, export.ref))
    |> Map.update!(:exports, &Map.delete(&1, export.ref))
    |> Map.update!(:monitor_index, &Map.delete(&1, export.monitor_ref))
    |> Map.put(:introspection, %{})
  end

  defp paths(%__MODULE__{subtrees: subtrees}, true), do: subtrees
  defp paths(%__MODULE__{exact: exact}, false), do: exact

  defp update_paths(%__MODULE__{} = objects, true, fun),
    do: %{objects | subtrees: fun.(objects.subtrees)}

  defp update_paths(%__MODULE__{} = objects, false, fun),
    do: %{objects | exact: fun.(objects.exact)}

  defp delete_path(paths, path, ref) do
    case List.delete(Map.fetch!(paths, path), ref) do
      [] -> Map.delete(paths, path)
      refs -> Map.put(paths, path, refs)
    end
  end
end
