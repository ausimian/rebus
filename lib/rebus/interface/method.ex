defmodule Rebus.Interface.Method do
  @moduledoc """
  A method declared by a `Rebus.Interface`.

  `in_signature` and `out_signature` are the argument signatures concatenated,
  so a method call's body signature can be compared with `in_signature`
  directly. `no_reply?` reflects the `org.freedesktop.DBus.Method.NoReply`
  annotation, which `annotations` always carries when `no_reply?` is `true`.
  """

  use TypedStruct

  alias Rebus.Interface
  alias Rebus.Interface.Arg

  typedstruct enforce: true do
    @typedoc "A declared method."
    field :name, binary()
    field :in_args, [Arg.t()], default: []
    field :out_args, [Arg.t()], default: []
    field :in_signature, binary(), default: ""
    field :out_signature, binary(), default: ""
    field :no_reply?, boolean(), default: false
    field :annotations, [Interface.annotation()], default: []
  end
end
