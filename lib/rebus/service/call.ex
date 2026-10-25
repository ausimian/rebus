defmodule Rebus.Service.Call do
  @moduledoc """
  One inbound method call, handed to the handler of an exported object.

  A handler receives `{export_ref, %Rebus.Service.Call{}}`, where
  `export_ref` is the reference `Rebus.Service.export/4` returned, and answers
  it with `Rebus.Service.reply/3`, `Rebus.Service.reply_error/4` or
  `Rebus.Service.reply_message/3`. The struct is what identifies the call to
  those functions, so any process holding it can answer.

  By the time a call arrives, the connection has checked that its interface
  and member are declared on the object and that its arguments have the
  declared signature, so `body` holds values of the declared types.

  ## Fields

  - `conn` - the connection the call arrived on.
  - `export` - the reference of the export the call was routed to.
  - `ref` - identifies this call to the reply functions.
  - `serial` - the caller's serial for the call.
  - `sender` - the caller's unique name, set by the message bus, or `nil` on
    a connection created with `bus: false`.
  - `path` - the object path called, which under a subtree export can be any
    path at or below the exported one.
  - `interface` - the interface the call was routed to. A caller may omit the
    interface when only one exported interface declares the member; this is
    then the interface that does. A property access has
    `"org.freedesktop.DBus.Properties"` here, and names the interface it
    targets as the first element of `body`.
  - `member` - the method name.
  - `signature` - the signature of `body`, which equals the method's declared
    in-signature.
  - `out_signature` - the declared out-signature, which `Rebus.Service.reply/3`
    encodes the reply body with: for a property access, `"v"` for `Get`, `""`
    for `Set` and `"a{sv}"` for `GetAll`.
  - `body` - the arguments.
  - `flags` - the message flags.
  - `no_reply?` - whether the caller asked for no reply. Answering such a
    call still completes it, but sends nothing.
  - `unix_fds` - `Rebus.UnixFD` handles for the descriptors sent with the
    call, which its `h` arguments index. The connection owns them and the
    handler borrows them until the call completes, when the connection
    closes them, unless `Rebus.Service.take_fds/2` has taken them first.
    The handler cannot close them itself, but a reply may send them back.
  - `deadline` - the `System.monotonic_time(:millisecond)` at which the
    connection completes the call itself: it answers the caller with
    `org.freedesktop.DBus.Error.NoReply`, or, for a call that expects no
    reply, closes the descriptors it carried. `nil` when the call never
    expires, or expects no reply and carried no descriptors.
  - `message` - the whole inbound message, for anything not lifted above.
    Its `unix_fds` are the same borrowed handles.
  """

  use TypedStruct

  typedstruct enforce: true do
    @typedoc "An inbound method call awaiting its answer."
    field :conn, pid()
    field :export, reference()
    field :ref, reference()
    field :serial, pos_integer()
    field :sender, binary() | nil
    field :path, binary()
    field :interface, binary()
    field :member, binary()
    field :signature, binary()
    field :out_signature, binary()
    field :body, [term()]
    field :flags, [Rebus.Message.flag()]
    field :no_reply?, boolean()
    field :unix_fds, [Rebus.UnixFD.t()]
    field :deadline, integer() | nil
    field :message, Rebus.Message.t()
  end
end
