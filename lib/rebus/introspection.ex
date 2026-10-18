defmodule Rebus.Introspection do
  @moduledoc """
  Generates D-Bus introspection XML, the document that
  `org.freedesktop.DBus.Introspectable.Introspect` returns.

  `to_xml/1` renders a `Rebus.Introspection.Node` in the format the
  [D-Bus specification](https://dbus.freedesktop.org/doc/dbus-specification.html#introspection-format)
  describes. Rebus only generates introspection XML; it does not parse it.

  ## Example

      calculator =
        Rebus.Interface.new!("org.example.Calculator",
          methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}]
        )

      %Rebus.Introspection.Node{interfaces: [calculator], children: ["History"]}
      |> Rebus.Introspection.to_xml()
      |> IO.iodata_to_binary()

  returns

  ```xml
  <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
   "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
  <node>
    <interface name="org.example.Calculator">
      <method name="Add">
        <arg name="a" type="i" direction="in"/>
        <arg name="b" type="i" direction="in"/>
        <arg name="sum" type="i" direction="out"/>
      </method>
    </interface>
    <node name="History"/>
  </node>
  ```
  """

  alias Rebus.Interface
  alias Rebus.Interface.Arg
  alias Rebus.Interface.Method
  alias Rebus.Interface.Property
  alias Rebus.Interface.Signal
  alias Rebus.Introspection.Node

  @doctype """
  <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
   "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
  """

  @doc """
  Renders a node as introspection XML.

  Interfaces appear in the order given. Within an interface, annotations come
  first, then methods, signals and properties, each sorted by name. Child
  nodes come last, sorted and without duplicates. Every attribute value is
  escaped.
  """
  @spec to_xml(Node.t()) :: iodata()
  def to_xml(%Node{interfaces: interfaces, children: children}) do
    [
      @doctype,
      "<node>\n",
      Enum.map(interfaces, &interface/1),
      children
      |> Enum.sort()
      |> Enum.dedup()
      |> Enum.map(&element(1, "node", [{"name", &1}], [])),
      "</node>\n"
    ]
  end

  defp interface(%Interface{} = interface) do
    element(1, "interface", [{"name", interface.name}], [
      annotations(interface.annotations, 2),
      interface.methods |> sorted() |> Enum.map(&method/1),
      interface.signals |> sorted() |> Enum.map(&signal/1),
      interface.properties |> sorted() |> Enum.map(&property/1)
    ])
  end

  defp method(%Method{} = method) do
    element(2, "method", [{"name", method.name}], [
      annotations(method.annotations, 3),
      Enum.map(method.in_args, &arg(&1, [{"direction", "in"}])),
      Enum.map(method.out_args, &arg(&1, [{"direction", "out"}]))
    ])
  end

  defp signal(%Signal{} = signal) do
    element(2, "signal", [{"name", signal.name}], [
      annotations(signal.annotations, 3),
      Enum.map(signal.args, &arg(&1, []))
    ])
  end

  defp property(%Property{} = property) do
    attributes = [
      {"name", property.name},
      {"type", property.signature},
      {"access", Atom.to_string(property.access)}
    ]

    element(2, "property", attributes, annotations(property.annotations, 3))
  end

  defp arg(%Arg{name: name, signature: signature}, direction) do
    name_attribute = if name == nil, do: [], else: [{"name", name}]
    element(3, "arg", name_attribute ++ [{"type", signature} | direction], [])
  end

  defp annotations(annotations, depth) do
    Enum.map(annotations, fn {name, value} ->
      element(depth, "annotation", [{"name", name}, {"value", value}], [])
    end)
  end

  defp sorted(members), do: members |> Map.values() |> Enum.sort_by(& &1.name)

  defp element(depth, tag, attributes, content) do
    indent = String.duplicate("  ", depth)
    open = [indent, "<", tag, Enum.map(attributes, &attribute/1)]

    if IO.iodata_length(content) == 0,
      do: [open, "/>\n"],
      else: [open, ">\n", content, indent, "</", tag, ">\n"]
  end

  defp attribute({name, value}), do: [" ", name, "=\"", escape(value), "\""]

  # Tab, line feed and carriage return are written as character references
  # because attribute-value normalization would otherwise read them back as
  # spaces.
  defp escape(value), do: for(<<char <- value>>, into: "", do: escape_char(char))

  defp escape_char(?&), do: "&amp;"
  defp escape_char(?<), do: "&lt;"
  defp escape_char(?>), do: "&gt;"
  defp escape_char(?"), do: "&quot;"
  defp escape_char(?'), do: "&apos;"
  defp escape_char(?\t), do: "&#9;"
  defp escape_char(?\n), do: "&#10;"
  defp escape_char(?\r), do: "&#13;"
  defp escape_char(char), do: <<char>>
end
