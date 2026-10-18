defmodule Rebus.IntrospectionTest do
  use ExUnit.Case, async: true

  require Record

  alias Rebus.Interface
  alias Rebus.Interface.Arg
  alias Rebus.Interface.Method
  alias Rebus.Interface.Signal
  alias Rebus.Introspection
  alias Rebus.Introspection.Node

  # xmerl is used only here, to check that the output is well-formed XML; it
  # is not a runtime dependency of Rebus.
  Record.defrecordp(
    :xml_attribute,
    :xmlAttribute,
    Record.extract(:xmlAttribute, from_lib: "xmerl/include/xmerl.hrl")
  )

  @doctype """
  <!DOCTYPE node PUBLIC "-//freedesktop//DTD D-BUS Object Introspection 1.0//EN"
   "http://www.freedesktop.org/standards/dbus/1.0/introspect.dtd">
  """

  defp xml(node), do: node |> Introspection.to_xml() |> IO.iodata_to_binary()

  # xmerl decodes the document's UTF-8 bytes itself.
  defp parse!(xml) do
    {document, []} = xml |> :binary.bin_to_list() |> :xmerl_scan.string(quiet: true)
    document
  end

  # The values of every attribute matched by an XPath expression, as binaries.
  defp attribute_values(document, path) do
    path
    |> String.to_charlist()
    |> :xmerl_xpath.string(document)
    |> Enum.map(&(&1 |> xml_attribute(:value) |> :unicode.characters_to_binary()))
  end

  describe "to_xml/1" do
    test "renders a representative node" do
      calculator =
        Interface.new!("org.example.Calculator",
          annotations: [{"org.freedesktop.DBus.Deprecated", "false"}],
          methods: [
            {"Add", in: [a: "i", b: "i"], out: [sum: "i"]},
            {"Reset", []},
            {"Log", in: [line: "s"], no_reply: true}
          ],
          signals: [{"Computed", args: [sum: "i"]}, {"Cleared", []}],
          properties: [
            {"Precision", "u", :readwrite},
            {"Secret", "s", :write, emits_changed: false},
            {"Version", "s", :read, emits_changed: :const}
          ]
        )

      node = %Node{
        interfaces: [calculator, Interface.introspectable(), Interface.peer()],
        children: ["History"]
      }

      assert xml(node) ==
               @doctype <>
                 """
                 <node>
                   <interface name="org.example.Calculator">
                     <annotation name="org.freedesktop.DBus.Deprecated" value="false"/>
                     <method name="Add">
                       <arg name="a" type="i" direction="in"/>
                       <arg name="b" type="i" direction="in"/>
                       <arg name="sum" type="i" direction="out"/>
                     </method>
                     <method name="Log">
                       <annotation name="org.freedesktop.DBus.Method.NoReply" value="true"/>
                       <arg name="line" type="s" direction="in"/>
                     </method>
                     <method name="Reset"/>
                     <signal name="Cleared"/>
                     <signal name="Computed">
                       <arg name="sum" type="i"/>
                     </signal>
                     <property name="Precision" type="u" access="readwrite"/>
                     <property name="Secret" type="s" access="write">
                       <annotation name="org.freedesktop.DBus.Property.EmitsChangedSignal" value="false"/>
                     </property>
                     <property name="Version" type="s" access="read">
                       <annotation name="org.freedesktop.DBus.Property.EmitsChangedSignal" value="const"/>
                     </property>
                   </interface>
                   <interface name="org.freedesktop.DBus.Introspectable">
                     <method name="Introspect">
                       <arg name="xml_data" type="s" direction="out"/>
                     </method>
                   </interface>
                   <interface name="org.freedesktop.DBus.Peer">
                     <method name="GetMachineId">
                       <arg name="machine_uuid" type="s" direction="out"/>
                     </method>
                     <method name="Ping"/>
                   </interface>
                   <node name="History"/>
                 </node>
                 """

      assert {:xmlElement, :node, _, _, _, _, _, _, _, _, _, _} = parse!(xml(node))
    end

    test "renders an intermediate node" do
      node = %Node{interfaces: [Interface.introspectable()], children: ["example", "freedesktop"]}

      assert xml(node) ==
               @doctype <>
                 """
                 <node>
                   <interface name="org.freedesktop.DBus.Introspectable">
                     <method name="Introspect">
                       <arg name="xml_data" type="s" direction="out"/>
                     </method>
                   </interface>
                   <node name="example"/>
                   <node name="freedesktop"/>
                 </node>
                 """

      parse!(xml(node))
    end

    test "renders an empty node" do
      assert xml(%Node{}) == @doctype <> "<node>\n</node>\n"
      parse!(xml(%Node{}))
    end

    test "sorts child nodes and drops duplicates" do
      node = %Node{children: ["b", "a", "C", "b", "a_1", "a"]}

      assert xml(node) ==
               @doctype <>
                 """
                 <node>
                   <node name="C"/>
                   <node name="a"/>
                   <node name="a_1"/>
                   <node name="b"/>
                 </node>
                 """

      assert attribute_values(parse!(xml(node)), "/node/node/@name") == ["C", "a", "a_1", "b"]
    end

    test "keeps the given order of interfaces" do
      node = %Node{interfaces: [Interface.peer(), Interface.introspectable()]}

      assert attribute_values(parse!(xml(node)), "/node/interface/@name") == [
               "org.freedesktop.DBus.Peer",
               "org.freedesktop.DBus.Introspectable"
             ]
    end

    test "sorts members by name however many there are" do
      # Maps of more than 32 keys do not iterate in key order.
      names = for i <- 1..40, do: "M#{String.pad_leading(Integer.to_string(i), 2, "0")}"

      interface =
        Interface.new!("org.example.Many", methods: Enum.map(Enum.shuffle(names), &{&1, []}))

      document = parse!(xml(%Node{interfaces: [interface]}))

      assert attribute_values(document, "/node/interface/method/@name") == names
    end

    test "omits the name of an unnamed argument" do
      interface =
        Interface.new!("org.example.Thing",
          methods: [{"M", in: ["s"], out: ["v"]}],
          signals: [{"S", args: ["u"]}]
        )

      output = xml(%Node{interfaces: [interface]})
      assert output =~ ~s(<arg type="s" direction="in"/>)
      assert output =~ ~s(<arg type="v" direction="out"/>)
      assert output =~ ~s(<arg type="u"/>)
    end

    test "escapes special characters in annotation values" do
      value = ~s(a & b < c > d "e" 'f')

      interface =
        Interface.new!("org.example.Thing",
          annotations: [{"org.example.Note", value}],
          methods: [{"M", annotations: [{"org.example.Note", value}]}],
          signals: [{"S", annotations: [{"org.example.Note", value}]}],
          properties: [{"P", "s", :read, annotations: [{"org.example.Note", value}]}]
        )

      output = xml(%Node{interfaces: [interface]})
      escaped = "a &amp; b &lt; c &gt; d &quot;e&quot; &apos;f&apos;"

      assert length(String.split(output, ~s(value="#{escaped}"))) == 5
      assert attribute_values(parse!(output), "//annotation/@value") == List.duplicate(value, 4)
    end

    test "escapes special characters in argument names" do
      # `Rebus.Interface.new/2` only accepts argument names in the member-name
      # grammar, but a declaration built directly, as parsed XML may be, can
      # carry any name.
      name = ~s(a&b<c>d"e'f)

      interface = %Interface{
        name: "org.example.Thing",
        methods: %{
          "M" => %Method{
            name: "M",
            in_args: [%Arg{name: name, signature: "s"}],
            out_args: [%Arg{name: name, signature: "s"}],
            in_signature: "s",
            out_signature: "s"
          }
        },
        signals: %{
          "S" => %Signal{name: "S", args: [%Arg{name: name, signature: "s"}], signature: "s"}
        }
      }

      output = xml(%Node{interfaces: [interface]})
      escaped = "a&amp;b&lt;c&gt;d&quot;e&apos;f"

      assert output =~ ~s(<arg name="#{escaped}" type="s" direction="in"/>)
      assert output =~ ~s(<arg name="#{escaped}" type="s" direction="out"/>)
      assert output =~ ~s(<arg name="#{escaped}" type="s"/>)
      assert attribute_values(parse!(output), "//arg/@name") == List.duplicate(name, 3)
    end

    test "preserves tabs, line feeds and carriage returns in attribute values" do
      value = "one\ttwo\nthree\rfour"
      interface = Interface.new!("org.example.Thing", annotations: [{"org.example.Note", value}])
      output = xml(%Node{interfaces: [interface]})

      assert output =~ ~s(value="one&#9;two&#10;three&#13;four")
      assert attribute_values(parse!(output), "//annotation/@value") == [value]
    end

    test "carries non-ASCII annotation values unchanged" do
      value = "ünïcødé ✓"
      interface = Interface.new!("org.example.Thing", annotations: [{"org.example.Note", value}])
      output = xml(%Node{interfaces: [interface]})

      assert output =~ ~s(value="#{value}")
      assert attribute_values(parse!(output), "//annotation/@value") == [value]
    end

    test "escapes child node names" do
      output = xml(%Node{children: ["a&b"]})
      assert output =~ ~s(<node name="a&amp;b"/>)
      assert attribute_values(parse!(output), "/node/node/@name") == ["a&b"]
    end

    test "renders every standard interface as well-formed XML" do
      node = %Node{
        interfaces: [
          Interface.peer(),
          Interface.introspectable(),
          Interface.properties(),
          Interface.object_manager()
        ]
      }

      document = parse!(xml(node))

      assert attribute_values(
               document,
               "/node/interface[@name='org.freedesktop.DBus.ObjectManager']/method/arg/@type"
             ) ==
               ["a{oa{sa{sv}}}"]

      assert attribute_values(document, "//signal[@name='PropertiesChanged']/arg/@type") ==
               ["s", "a{sv}", "as"]
    end
  end
end
