defmodule Rebus.Introspection.Node do
  @moduledoc """
  One object path's introspection data: the interfaces it implements and the
  names of its child nodes.

  `children` holds single path elements, such as `"Calc"` for
  `/org/example/Calc` under `/org/example`, not full paths.
  `Rebus.Introspection.to_xml/1` sorts them.
  """

  use TypedStruct

  typedstruct do
    @typedoc "An introspection node."
    field :interfaces, [Rebus.Interface.t()], default: []
    field :children, [binary()], default: []
  end
end
