defmodule Rebus.Interface.Standard do
  @moduledoc false

  # The standard interfaces from the D-Bus specification, "Standard
  # Interfaces", built once at compile time. Argument names follow the
  # specification's.

  alias Rebus.Interface

  @peer Interface.new!("org.freedesktop.DBus.Peer",
          methods: [
            {"Ping", []},
            {"GetMachineId", out: [machine_uuid: "s"]}
          ]
        )

  @introspectable Interface.new!("org.freedesktop.DBus.Introspectable",
                    methods: [{"Introspect", out: [xml_data: "s"]}]
                  )

  @properties Interface.new!("org.freedesktop.DBus.Properties",
                methods: [
                  {"Get", in: [interface_name: "s", property_name: "s"], out: [value: "v"]},
                  {"Set", in: [interface_name: "s", property_name: "s", value: "v"]},
                  {"GetAll", in: [interface_name: "s"], out: [props: "a{sv}"]}
                ],
                signals: [
                  {"PropertiesChanged",
                   args: [
                     interface_name: "s",
                     changed_properties: "a{sv}",
                     invalidated_properties: "as"
                   ]}
                ]
              )

  @object_manager Interface.new!("org.freedesktop.DBus.ObjectManager",
                    methods: [
                      {"GetManagedObjects",
                       out: [objpath_interfaces_and_properties: "a{oa{sa{sv}}}"]}
                    ],
                    signals: [
                      {"InterfacesAdded",
                       args: [object_path: "o", interfaces_and_properties: "a{sa{sv}}"]},
                      {"InterfacesRemoved", args: [object_path: "o", interfaces: "as"]}
                    ]
                  )

  @spec peer() :: Interface.t()
  def peer, do: @peer

  @spec introspectable() :: Interface.t()
  def introspectable, do: @introspectable

  @spec properties() :: Interface.t()
  def properties, do: @properties

  @spec object_manager() :: Interface.t()
  def object_manager, do: @object_manager
end
