defmodule Rebus.Interface.Arg do
  @moduledoc """
  One argument of a `Rebus.Interface.Method` or `Rebus.Interface.Signal`.

  `signature` is a single complete D-Bus type. `name` is optional, as it is in
  introspection XML.
  """

  use TypedStruct

  typedstruct enforce: true do
    @typedoc "A method or signal argument."
    field :name, binary() | nil
    field :signature, binary()
  end
end
