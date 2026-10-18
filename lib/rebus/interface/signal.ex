defmodule Rebus.Interface.Signal do
  @moduledoc """
  A signal declared by a `Rebus.Interface`.

  `signature` is the argument signatures concatenated: the body signature of
  every emission of the signal.
  """

  use TypedStruct

  alias Rebus.Interface
  alias Rebus.Interface.Arg

  typedstruct enforce: true do
    @typedoc "A declared signal."
    field :name, binary()
    field :args, [Arg.t()], default: []
    field :signature, binary(), default: ""
    field :annotations, [Interface.annotation()], default: []
  end
end
