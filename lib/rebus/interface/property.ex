defmodule Rebus.Interface.Property do
  @moduledoc """
  A property declared by a `Rebus.Interface`.

  `signature` is a single complete D-Bus type. `emits_changed` says how
  `org.freedesktop.DBus.Properties.PropertiesChanged` reports a change, as the
  `org.freedesktop.DBus.Property.EmitsChangedSignal` annotation does:

  - `true` - the signal carries the new value
  - `:invalidates` - the signal names the property without its value
  - `false` - the property changes without a signal
  - `:const` - the property never changes while the object exists
  """

  use TypedStruct

  alias Rebus.Interface

  @typedoc "How `PropertiesChanged` reports a change to a property."
  @type emits_changed :: true | false | :invalidates | :const

  typedstruct enforce: true do
    @typedoc "A declared property."
    field :name, binary()
    field :signature, binary()
    field :access, Interface.access()
    field :emits_changed, emits_changed(), default: true
    field :annotations, [Interface.annotation()], default: []
  end
end
