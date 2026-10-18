defmodule Rebus.InterfaceTest do
  use ExUnit.Case, async: true

  alias Rebus.Interface
  alias Rebus.Interface.Arg
  alias Rebus.Interface.Method
  alias Rebus.Interface.Property
  alias Rebus.Interface.Signal
  alias Rebus.Message

  doctest Rebus.Interface

  @no_reply "org.freedesktop.DBus.Method.NoReply"
  @emits_changed "org.freedesktop.DBus.Property.EmitsChangedSignal"

  defp calculator do
    Interface.new!("org.example.Calculator",
      methods: [
        {"Add", in: [a: "i", b: "i"], out: [sum: "i"]},
        {"Log", in: [line: "s"], no_reply: true}
      ],
      signals: [{"Computed", args: [sum: "i"]}],
      properties: [
        {"Precision", "u", :readwrite},
        {"Label", "s", :readwrite, emits_changed: :invalidates},
        {"Secret", "s", :write, emits_changed: false},
        {"Version", "s", :read, emits_changed: :const}
      ]
    )
  end

  describe "new/2 builds" do
    test "an interface with no members" do
      assert {:ok,
              %Interface{
                name: "org.example.Empty",
                methods: %{},
                signals: %{},
                properties: %{},
                annotations: []
              }} = Interface.new("org.example.Empty")
    end

    test "methods with named arguments and concatenated signatures" do
      {:ok, interface} =
        Interface.new("org.example.Calculator",
          methods: [{"Divide", in: [a: "x", b: "x"], out: [quotient: "x", remainder: "x"]}]
        )

      assert %{
               "Divide" => %Method{
                 name: "Divide",
                 in_args: [%Arg{name: "a", signature: "x"}, %Arg{name: "b", signature: "x"}],
                 out_args: [
                   %Arg{name: "quotient", signature: "x"},
                   %Arg{name: "remainder", signature: "x"}
                 ],
                 in_signature: "xx",
                 out_signature: "xx",
                 no_reply?: false,
                 annotations: []
               }
             } = interface.methods
    end

    test "a method with no arguments" do
      {:ok, interface} = Interface.new("org.example.Thing", methods: [{"Reset", []}])

      assert %Method{in_args: [], out_args: [], in_signature: "", out_signature: ""} =
               interface.methods["Reset"]
    end

    test "arguments named by binaries, nil, or not at all" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          methods: [{"Mixed", in: [{"first", "s"}, {nil, "a{sv}"}, "(ii)"], out: ["v"]}]
        )

      assert %Method{
               in_args: [
                 %Arg{name: "first", signature: "s"},
                 %Arg{name: nil, signature: "a{sv}"},
                 %Arg{name: nil, signature: "(ii)"}
               ],
               out_args: [%Arg{name: nil, signature: "v"}],
               in_signature: "sa{sv}(ii)",
               out_signature: "v"
             } = interface.methods["Mixed"]
    end

    test "signals with concatenated signatures" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          signals: [{"Moved", args: [x: "i", y: "i"]}, {"Ticked", []}]
        )

      assert %Signal{
               name: "Moved",
               args: [%Arg{name: "x", signature: "i"}, %Arg{name: "y", signature: "i"}],
               signature: "ii",
               annotations: []
             } = interface.signals["Moved"]

      assert %Signal{args: [], signature: ""} = interface.signals["Ticked"]
    end

    test "properties with each access mode" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          properties: [{"R", "s", :read}, {"W", "u", :write}, {"RW", "a{sv}", :readwrite}]
        )

      assert %Property{name: "R", signature: "s", access: :read, emits_changed: true} =
               interface.properties["R"]

      assert %Property{access: :write} = interface.properties["W"]
      assert %Property{access: :readwrite, signature: "a{sv}"} = interface.properties["RW"]
    end

    test "annotations at every level, in the order given" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          annotations: [{"org.example.B", "2"}, {"org.example.A", "1"}],
          methods: [{"M", annotations: [{"org.freedesktop.DBus.Deprecated", "true"}]}],
          signals: [{"S", annotations: [{"org.example.Note", "x"}]}],
          properties: [{"P", "s", :read, annotations: [{"org.example.Note", "y"}]}]
        )

      assert interface.annotations == [{"org.example.B", "2"}, {"org.example.A", "1"}]
      assert interface.methods["M"].annotations == [{"org.freedesktop.DBus.Deprecated", "true"}]
      assert interface.signals["S"].annotations == [{"org.example.Note", "x"}]
      assert interface.properties["P"].annotations == [{"org.example.Note", "y"}]
    end

    test "a method and a signal that share a name" do
      assert {:ok, interface} =
               Interface.new("org.example.Thing",
                 methods: [{"Changed", []}],
                 signals: [{"Changed", []}]
               )

      assert Map.has_key?(interface.methods, "Changed")
      assert Map.has_key?(interface.signals, "Changed")
    end

    test "a signature of exactly 255 bytes" do
      args = List.duplicate("i", 255)
      assert {:ok, interface} = Interface.new("org.example.Thing", methods: [{"M", in: args}])
      assert byte_size(interface.methods["M"].in_signature) == 255
    end

    test "an out argument of type h on a method that expects a reply" do
      assert {:ok, interface} =
               Interface.new("org.example.Thing", methods: [{"Open", out: ["h"]}])

      assert interface.methods["Open"].out_signature == "h"
    end

    test "an in argument of type h on a no-reply method" do
      assert {:ok, _interface} =
               Interface.new("org.example.Thing", methods: [{"Send", in: ["h"], no_reply: true}])
    end
  end

  describe "new/2 rejects options" do
    test "that are unknown" do
      assert {:error, {:invalid_option, [method: []]}} =
               Interface.new("org.example.Thing", method: [])
    end

    test "that repeat" do
      assert {:error, {:invalid_option, _}} =
               Interface.new("org.example.Thing", methods: [], methods: [])
    end

    test "that are not a keyword list" do
      assert {:error, {:invalid_option, %{}}} = Interface.new("org.example.Thing", %{})

      assert {:error, {:invalid_option, [:methods]}} =
               Interface.new("org.example.Thing", [:methods])
    end

    test "whose member lists are not lists" do
      assert {:error, {:invalid_option, %{}}} = Interface.new("org.example.Thing", methods: %{})
      assert {:error, {:invalid_option, nil}} = Interface.new("org.example.Thing", signals: nil)

      assert {:error, {:invalid_option, "P"}} =
               Interface.new("org.example.Thing", properties: "P")
    end
  end

  describe "new/2 validates names" do
    for {description, name} <- [
          {"a single element", "Calculator"},
          {"an empty element", "org..Calculator"},
          {"a leading digit", "org.1example"},
          {"a hyphen", "org.example-x.Calculator"},
          {"a non-binary", :"org.example.Calculator"},
          {"more than 255 bytes", "a." <> String.duplicate("b", 254)}
        ] do
      test "rejecting an interface name with #{description}" do
        assert {:error, {:invalid_interface_name, unquote(name)}} =
                 Interface.new(unquote(name))
      end
    end

    for {description, name} <- [
          {"a period", "Do.It"},
          {"a leading digit", "1Do"},
          {"a hyphen", "Do-It"},
          {"no characters", ""},
          {"a non-binary", :DoIt}
        ] do
      test "rejecting a method name with #{description}" do
        assert {:error, {:invalid_member_name, unquote(name)}} =
                 Interface.new("org.example.Thing", methods: [{unquote(name), []}])
      end

      test "rejecting a signal name with #{description}" do
        assert {:error, {:invalid_member_name, unquote(name)}} =
                 Interface.new("org.example.Thing", signals: [{unquote(name), []}])
      end

      test "rejecting a property name with #{description}" do
        assert {:error, {:invalid_member_name, unquote(name)}} =
                 Interface.new("org.example.Thing", properties: [{unquote(name), "s", :read}])
      end
    end

    test "rejecting argument names that are not member names" do
      for name <- ["has space", "a.b", "1st", "", "x&y"] do
        assert {:error, {:invalid_arg_name, ^name}} =
                 Interface.new("org.example.Thing", methods: [{"M", in: [{name, "s"}]}])

        assert {:error, {:invalid_arg_name, ^name}} =
                 Interface.new("org.example.Thing", signals: [{"S", args: [{name, "s"}]}])
      end
    end
  end

  describe "new/2 validates signatures" do
    for signature <- ["", "ii", "a", "(", "{sv}", "z", "a{vs}"] do
      test "rejecting the argument signature #{inspect(signature)}" do
        assert {:error, {:invalid_signature, unquote(signature)}} =
                 Interface.new("org.example.Thing", methods: [{"M", in: [x: unquote(signature)]}])

        assert {:error, {:invalid_signature, unquote(signature)}} =
                 Interface.new("org.example.Thing", methods: [{"M", out: [unquote(signature)]}])

        assert {:error, {:invalid_signature, unquote(signature)}} =
                 Interface.new("org.example.Thing", signals: [{"S", args: [unquote(signature)]}])
      end

      test "rejecting the property signature #{inspect(signature)}" do
        assert {:error, {:invalid_signature, unquote(signature)}} =
                 Interface.new("org.example.Thing",
                   properties: [{"P", unquote(signature), :read}]
                 )
      end
    end

    test "rejecting a signature over the nesting limit" do
      deep = String.duplicate("a", 33) <> "i"

      assert {:error, {:invalid_signature, ^deep}} =
               Interface.new("org.example.Thing", methods: [{"M", in: [deep]}])

      assert {:error, {:invalid_signature, ^deep}} =
               Interface.new("org.example.Thing", properties: [{"P", deep, :read}])
    end

    test "rejecting arguments whose concatenation exceeds 255 bytes" do
      args = List.duplicate("i", 256)
      signature = String.duplicate("i", 256)

      assert {:error, {:invalid_signature, ^signature}} =
               Interface.new("org.example.Thing", methods: [{"M", in: args}])

      assert {:error, {:invalid_signature, ^signature}} =
               Interface.new("org.example.Thing", methods: [{"M", out: args}])

      assert {:error, {:invalid_signature, ^signature}} =
               Interface.new("org.example.Thing", signals: [{"S", args: args}])
    end

    test "rejecting a non-binary signature" do
      assert {:error, {:invalid_signature, :s}} =
               Interface.new("org.example.Thing", properties: [{"P", :s, :read}])

      assert {:error, {:invalid_arg, {:x, :s}}} =
               Interface.new("org.example.Thing", methods: [{"M", in: [x: :s]}])
    end
  end

  describe "new/2 rejects malformed entries" do
    test "for methods" do
      assert {:error, {:invalid_method, "M"}} =
               Interface.new("org.example.Thing", methods: ["M"])

      assert {:error, {:invalid_method, "M"}} =
               Interface.new("org.example.Thing", methods: [{"M", in: [], in: []}])

      assert {:error, {:invalid_method, "M"}} =
               Interface.new("org.example.Thing", methods: [{"M", args: []}])

      assert {:error, {:invalid_method, {"M", :x}}} =
               Interface.new("org.example.Thing", methods: [{"M", :x}])
    end

    test "for signals" do
      assert {:error, {:invalid_signal, "S"}} =
               Interface.new("org.example.Thing", signals: [{"S", in: []}])

      assert {:error, {:invalid_signal, {"S"}}} =
               Interface.new("org.example.Thing", signals: [{"S"}])
    end

    test "for properties" do
      assert {:error, {:invalid_property, {"P", "s"}}} =
               Interface.new("org.example.Thing", properties: [{"P", "s"}])

      assert {:error, {:invalid_property, "P"}} =
               Interface.new("org.example.Thing", properties: [{"P", "s", :read, access: :read}])

      assert {:error, {:invalid_property, {"P", "s", :read, :x}}} =
               Interface.new("org.example.Thing", properties: [{"P", "s", :read, :x}])
    end

    test "for arguments" do
      assert {:error, {:invalid_arg, %{a: "i"}}} =
               Interface.new("org.example.Thing", methods: [{"M", in: %{a: "i"}}])

      assert {:error, {:invalid_arg, {"a", "i", "x"}}} =
               Interface.new("org.example.Thing", methods: [{"M", in: [{"a", "i", "x"}]}])

      assert {:error, {:invalid_arg, 1}} =
               Interface.new("org.example.Thing", signals: [{"S", args: [1]}])
    end
  end

  describe "new/2 rejects duplicates" do
    test "among methods" do
      assert {:error, {:duplicate_member, "M"}} =
               Interface.new("org.example.Thing", methods: [{"M", []}, {"M", in: ["s"]}])
    end

    test "among signals" do
      assert {:error, {:duplicate_member, "S"}} =
               Interface.new("org.example.Thing", signals: [{"S", []}, {"S", []}])
    end

    test "among properties" do
      assert {:error, {:duplicate_property, "P"}} =
               Interface.new("org.example.Thing",
                 properties: [{"P", "s", :read}, {"P", "u", :write}]
               )
    end

    test "among annotations at any level" do
      twice = [{"org.example.A", "1"}, {"org.example.A", "2"}]

      assert {:error, {:duplicate_annotation, "org.example.A"}} =
               Interface.new("org.example.Thing", annotations: twice)

      assert {:error, {:duplicate_annotation, "org.example.A"}} =
               Interface.new("org.example.Thing", methods: [{"M", annotations: twice}])

      assert {:error, {:duplicate_annotation, "org.example.A"}} =
               Interface.new("org.example.Thing", signals: [{"S", annotations: twice}])

      assert {:error, {:duplicate_annotation, "org.example.A"}} =
               Interface.new("org.example.Thing",
                 properties: [{"P", "s", :read, annotations: twice}]
               )
    end
  end

  describe "new/2 validates annotations" do
    for {description, annotation} <- [
          {"a name that is not an interface name", {"Deprecated", "true"}},
          {"a non-binary name", {:deprecated, "true"}},
          {"a non-binary value", {"org.example.A", true}},
          {"a NUL in the value", {"org.example.A", "a\0b"}},
          {"invalid UTF-8 in the value", {"org.example.A", <<0xFF>>}},
          {"a control character in the value", {"org.example.A", "bell\a"}},
          {"a vertical tab in the value", {"org.example.A", "a\vb"}},
          {"U+FFFE in the value", {"org.example.A", "a\u{FFFE}"}},
          {"U+FFFF in the value", {"org.example.A", "a\u{FFFF}"}},
          {"a bare name", "org.example.A"}
        ] do
      test "rejecting #{description}" do
        annotation = unquote(Macro.escape(annotation))

        assert {:error, {:invalid_annotation, ^annotation}} =
                 Interface.new("org.example.Thing", annotations: [annotation])

        assert {:error, {:invalid_annotation, ^annotation}} =
                 Interface.new("org.example.Thing", methods: [{"M", annotations: [annotation]}])
      end
    end

    test "accepting values that XML can carry once escaped" do
      value = "tab\tnewline\nreturn\r & <tag> \"quoted\" 'single' ünïcødé"

      assert {:ok, interface} =
               Interface.new("org.example.Thing", annotations: [{"org.example.A", value}])

      assert interface.annotations == [{"org.example.A", value}]
    end

    test "rejecting annotations that are not a list" do
      assert {:error, {:invalid_annotation, %{}}} =
               Interface.new("org.example.Thing", annotations: %{})
    end
  end

  describe "new/2 access" do
    test "rejects an unknown access mode" do
      for access <- [:readonly, "read", nil] do
        assert {:error, {:invalid_access, ^access}} =
                 Interface.new("org.example.Thing", properties: [{"P", "s", access}])
      end
    end
  end

  describe "new/2 no-reply methods" do
    test "add the NoReply annotation" do
      {:ok, interface} = Interface.new("org.example.Thing", methods: [{"M", no_reply: true}])

      assert %Method{no_reply?: true, annotations: [{@no_reply, "true"}]} =
               interface.methods["M"]
    end

    test "keep other annotations ahead of the added one" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          methods: [{"M", no_reply: true, annotations: [{"org.example.A", "1"}]}]
        )

      assert interface.methods["M"].annotations == [{"org.example.A", "1"}, {@no_reply, "true"}]
    end

    test "follow a NoReply annotation given without the option" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          methods: [
            {"Yes", annotations: [{@no_reply, "true"}]},
            {"No", annotations: [{@no_reply, "false"}]}
          ]
        )

      assert %Method{no_reply?: true, annotations: [{@no_reply, "true"}]} =
               interface.methods["Yes"]

      assert %Method{no_reply?: false, annotations: [{@no_reply, "false"}]} =
               interface.methods["No"]
    end

    test "accept an option that agrees with the annotation" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          methods: [{"M", no_reply: true, annotations: [{@no_reply, "true"}]}]
        )

      assert %Method{no_reply?: true, annotations: [{@no_reply, "true"}]} =
               interface.methods["M"]
    end

    test "leave a method without the option or annotation expecting a reply" do
      {:ok, interface} =
        Interface.new("org.example.Thing", methods: [{"M", []}, {"N", no_reply: false}])

      assert %Method{no_reply?: false, annotations: []} = interface.methods["M"]
      assert %Method{no_reply?: false, annotations: []} = interface.methods["N"]
    end

    test "reject an option that contradicts the annotation" do
      assert {:error, {:conflicting_annotation, {"M", @no_reply}}} =
               Interface.new("org.example.Thing",
                 methods: [{"M", no_reply: false, annotations: [{@no_reply, "true"}]}]
               )

      assert {:error, {:conflicting_annotation, {"M", @no_reply}}} =
               Interface.new("org.example.Thing",
                 methods: [{"M", no_reply: true, annotations: [{@no_reply, "false"}]}]
               )
    end

    test "reject a non-boolean option" do
      assert {:error, {:invalid_method, "M"}} =
               Interface.new("org.example.Thing", methods: [{"M", no_reply: :yes}])

      assert {:error, {:invalid_method, "M"}} =
               Interface.new("org.example.Thing", methods: [{"M", no_reply: nil}])
    end

    test "reject an annotation value other than true or false" do
      assert {:error, {:invalid_annotation, {@no_reply, "yes"}}} =
               Interface.new("org.example.Thing",
                 methods: [{"M", annotations: [{@no_reply, "yes"}]}]
               )
    end

    test "reject a Unix file descriptor among the out arguments" do
      for out <- [["h"], ["s", "ah"], ["(sh)"], ["a{sh}"]] do
        assert {:error, {:unix_fd_in_no_reply, "M"}} =
                 Interface.new("org.example.Thing", methods: [{"M", out: out, no_reply: true}])

        assert {:error, {:unix_fd_in_no_reply, "M"}} =
                 Interface.new("org.example.Thing",
                   methods: [{"M", out: out, annotations: [{@no_reply, "true"}]}]
                 )
      end
    end
  end

  describe "new/2 emits_changed" do
    test "defaults to true without an annotation" do
      {:ok, interface} = Interface.new("org.example.Thing", properties: [{"P", "s", :read}])
      assert %Property{emits_changed: true, annotations: []} = interface.properties["P"]
    end

    test "adds the annotation when the option departs from the default" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          properties: [
            {"T", "s", :read, emits_changed: true},
            {"F", "s", :read, emits_changed: false},
            {"I", "s", :read, emits_changed: :invalidates},
            {"C", "s", :read, emits_changed: :const}
          ]
        )

      assert %Property{emits_changed: true, annotations: []} = interface.properties["T"]

      assert %Property{emits_changed: false, annotations: [{@emits_changed, "false"}]} =
               interface.properties["F"]

      assert %Property{
               emits_changed: :invalidates,
               annotations: [{@emits_changed, "invalidates"}]
             } =
               interface.properties["I"]

      assert %Property{emits_changed: :const, annotations: [{@emits_changed, "const"}]} =
               interface.properties["C"]
    end

    test "follows a property annotation given without the option" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          properties: [
            {"T", "s", :read, annotations: [{@emits_changed, "true"}]},
            {"F", "s", :read, annotations: [{@emits_changed, "false"}]},
            {"I", "s", :read, annotations: [{@emits_changed, "invalidates"}]},
            {"C", "s", :read, annotations: [{@emits_changed, "const"}]}
          ]
        )

      assert interface.properties["T"].emits_changed == true
      assert interface.properties["F"].emits_changed == false
      assert interface.properties["I"].emits_changed == :invalidates
      assert interface.properties["C"].emits_changed == :const
    end

    test "accepts an option that agrees with the annotation" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          properties: [
            {"C", "s", :read, emits_changed: :const, annotations: [{@emits_changed, "const"}]}
          ]
        )

      assert %Property{emits_changed: :const, annotations: [{@emits_changed, "const"}]} =
               interface.properties["C"]
    end

    test "rejects an option that contradicts the annotation" do
      assert {:error, {:conflicting_annotation, {"P", @emits_changed}}} =
               Interface.new("org.example.Thing",
                 properties: [
                   {"P", "s", :read,
                    emits_changed: true, annotations: [{@emits_changed, "const"}]}
                 ]
               )
    end

    test "takes its default from an interface annotation" do
      {:ok, interface} =
        Interface.new("org.example.Thing",
          annotations: [{@emits_changed, "const"}],
          properties: [
            {"Inherited", "s", :read},
            {"Same", "s", :read, emits_changed: :const},
            {"Overridden", "s", :readwrite, emits_changed: true},
            {"Annotated", "s", :readwrite, annotations: [{@emits_changed, "invalidates"}]}
          ]
        )

      assert %Property{emits_changed: :const, annotations: []} = interface.properties["Inherited"]
      assert %Property{emits_changed: :const, annotations: []} = interface.properties["Same"]

      assert %Property{emits_changed: true, annotations: [{@emits_changed, "true"}]} =
               interface.properties["Overridden"]

      assert interface.properties["Annotated"].emits_changed == :invalidates
    end

    test "rejects an unknown option value" do
      for value <- [:yes, "const", nil] do
        assert {:error, {:invalid_emits_changed, ^value}} =
                 Interface.new("org.example.Thing",
                   properties: [{"P", "s", :read, emits_changed: value}]
                 )
      end
    end

    test "rejects an unknown annotation value on a property or the interface" do
      assert {:error, {:invalid_annotation, {@emits_changed, "sometimes"}}} =
               Interface.new("org.example.Thing",
                 properties: [{"P", "s", :read, annotations: [{@emits_changed, "sometimes"}]}]
               )

      assert {:error, {:invalid_annotation, {@emits_changed, "sometimes"}}} =
               Interface.new("org.example.Thing", annotations: [{@emits_changed, "sometimes"}])
    end
  end

  describe "new!/2" do
    test "returns the interface" do
      assert %Interface{name: "org.example.Thing"} = Interface.new!("org.example.Thing")
    end

    test "raises ArgumentError with the reason" do
      assert_raise ArgumentError,
                   ~s(invalid D-Bus interface: {:invalid_interface_name, "x"}),
                   fn ->
                     Interface.new!("x")
                   end
    end
  end

  describe "standard interfaces" do
    test "Peer" do
      peer = Interface.peer()
      assert peer.name == "org.freedesktop.DBus.Peer"
      assert signatures(peer.methods) == %{"Ping" => {"", ""}, "GetMachineId" => {"", "s"}}
      assert peer.signals == %{}
      assert peer.properties == %{}
    end

    test "Introspectable" do
      introspectable = Interface.introspectable()
      assert introspectable.name == "org.freedesktop.DBus.Introspectable"
      assert signatures(introspectable.methods) == %{"Introspect" => {"", "s"}}
    end

    test "Properties" do
      properties = Interface.properties()
      assert properties.name == "org.freedesktop.DBus.Properties"

      assert signatures(properties.methods) == %{
               "Get" => {"ss", "v"},
               "Set" => {"ssv", ""},
               "GetAll" => {"s", "a{sv}"}
             }

      assert properties.signals["PropertiesChanged"].signature == "sa{sv}as"
    end

    test "ObjectManager" do
      object_manager = Interface.object_manager()
      assert object_manager.name == "org.freedesktop.DBus.ObjectManager"
      assert signatures(object_manager.methods) == %{"GetManagedObjects" => {"", "a{oa{sa{sv}}}"}}
      assert object_manager.signals["InterfacesAdded"].signature == "oa{sa{sv}}"
      assert object_manager.signals["InterfacesRemoved"].signature == "oas"
    end

    defp signatures(methods),
      do:
        Map.new(methods, fn {name, method} ->
          {name, {method.in_signature, method.out_signature}}
        end)
  end

  describe "signal/5" do
    test "builds a signal typed by the declaration" do
      assert {:ok, %Message{type: :signal} = message} =
               Interface.signal(calculator(), "/org/example/Calc", "Computed", [42])

      assert message.header_fields.path == "/org/example/Calc"
      assert message.header_fields.interface == "org.example.Calculator"
      assert message.header_fields.member == "Computed"
      assert Message.signature(message) == "i"
      assert message.body == [42]
    end

    test "passes :destination and :flags through" do
      assert {:ok, message} =
               Interface.signal(calculator(), "/c", "Computed", [1],
                 destination: ":1.7",
                 flags: [:no_auto_start]
               )

      assert message.header_fields.destination == ":1.7"
      assert message.flags == [:no_auto_start]
    end

    test "rejects a signal the interface does not declare" do
      assert {:error, :unknown_signal} = Interface.signal(calculator(), "/c", "Add", [1])
    end

    test "rejects a body that does not fit the declaration" do
      assert {:error, :invalid_body} = Interface.signal(calculator(), "/c", "Computed", ["one"])
      assert {:error, :invalid_body} = Interface.signal(calculator(), "/c", "Computed", [])
    end

    test "rejects other options" do
      assert {:error, {:invalid_option, [signature: "s"]}} =
               Interface.signal(calculator(), "/c", "Computed", [1], signature: "s")
    end

    test "reports an invalid path from the message" do
      assert {:error, {:invalid_header_field, :path}} =
               Interface.signal(calculator(), "relative", "Computed", [1])
    end
  end

  describe "properties_changed/5" do
    test "builds PropertiesChanged with typed values" do
      assert {:ok, message} =
               Interface.properties_changed(calculator(), "/c", %{"Precision" => 3}, ["Label"])

      assert message.header_fields.interface == "org.freedesktop.DBus.Properties"
      assert message.header_fields.member == "PropertiesChanged"
      assert message.header_fields.path == "/c"
      assert Message.signature(message) == "sa{sv}as"
      assert message.body == ["org.example.Calculator", [{"Precision", {"u", 3}}], ["Label"]]
    end

    test "rejects a property that emits true among the invalidated" do
      assert {:error, {:invalid_property_change, "Precision"}} =
               Interface.properties_changed(calculator(), "/c", %{}, ["Precision"])
    end

    test "rejects a property that only invalidates among the changed" do
      assert {:error, {:invalid_property_change, "Label"}} =
               Interface.properties_changed(calculator(), "/c", %{"Label" => "x"}, [])
    end

    test "rejects properties that emit nothing" do
      for name <- ["Secret", "Version"] do
        assert {:error, {:invalid_property_change, ^name}} =
                 Interface.properties_changed(calculator(), "/c", %{name => "x"}, [])

        assert {:error, {:invalid_property_change, ^name}} =
                 Interface.properties_changed(calculator(), "/c", %{}, [name])
      end
    end

    test "passes :destination, :flags and :fds through" do
      interface = Interface.new!("org.example.Thing", properties: [{"Handle", "h", :read}])

      assert {:ok, message} =
               Interface.properties_changed(interface, "/t", %{"Handle" => 0}, [],
                 destination: ":1.7",
                 flags: [:no_auto_start],
                 fds: [0]
               )

      assert message.header_fields.destination == ":1.7"
      assert message.flags == [:no_auto_start]
      assert message.unix_fds == [0]
      assert message.body == ["org.example.Thing", [{"Handle", {"h", 0}}], []]
    end

    test "rejects other options" do
      assert {:error, {:invalid_option, [path: "/other"]}} =
               Interface.properties_changed(calculator(), "/c", %{}, [], path: "/other")
    end

    test "rejects properties the interface does not declare" do
      assert {:error, {:unknown_property, "Nope"}} =
               Interface.properties_changed(calculator(), "/c", %{"Nope" => 1}, [])

      assert {:error, {:unknown_property, "Nope"}} =
               Interface.properties_changed(calculator(), "/c", %{}, ["Nope"])
    end

    test "reports a value that does not fit its declaration" do
      assert {:error, :invalid_body} =
               Interface.properties_changed(calculator(), "/c", %{"Precision" => "three"}, [])
    end
  end

  describe "wrap_properties/2" do
    test "types each value by its declaration, sorted by name" do
      assert {:ok, [{"Label", {"s", "x"}}, {"Precision", {"u", 3}}, {"Version", {"s", "1"}}]} =
               Interface.wrap_properties(calculator(), %{
                 "Version" => "1",
                 "Precision" => 3,
                 "Label" => "x"
               })
    end

    test "wraps an empty map" do
      assert {:ok, []} = Interface.wrap_properties(calculator(), %{})
    end

    test "rejects a property the interface does not declare" do
      assert {:error, {:unknown_property, "Nope"}} =
               Interface.wrap_properties(calculator(), %{"Nope" => 1, "Precision" => 3})
    end

    test "produces a body that encodes as a{sv}" do
      {:ok, wrapped} =
        Interface.wrap_properties(calculator(), %{"Precision" => 3, "Label" => "x"})

      assert {:ok, _message} =
               Message.new(:method_return, reply_serial: 1, signature: "a{sv}", body: [wrapped])
    end
  end

  describe "wrap_properties!/2" do
    test "returns the wrapped values" do
      assert [{"Precision", {"u", 3}}] =
               Interface.wrap_properties!(calculator(), %{"Precision" => 3})
    end

    test "raises ArgumentError for an undeclared property" do
      assert_raise ArgumentError,
                   ~s(cannot wrap D-Bus properties: {:unknown_property, "Nope"}),
                   fn ->
                     Interface.wrap_properties!(calculator(), %{"Nope" => 1})
                   end
    end
  end
end
