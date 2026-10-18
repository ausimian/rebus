defmodule Rebus.Interface do
  @moduledoc """
  A validated declaration of a D-Bus interface: its methods, signals,
  properties and annotations.

  One declaration describes an interface everywhere Rebus needs it: checking
  the arguments of inbound calls, typing replies, signals and property values,
  and generating introspection XML with `Rebus.Introspection`.

  ## Example

      calculator =
        Rebus.Interface.new!("org.example.Calculator",
          methods: [
            {"Add", in: [a: "i", b: "i"], out: [sum: "i"]},
            {"Log", in: [line: "s"], no_reply: true}
          ],
          signals: [{"Computed", args: [sum: "i"]}],
          properties: [
            {"Precision", "u", :readwrite},
            {"Version", "s", :read, emits_changed: :const}
          ]
        )

  ## Declarations

  `new/2` takes the interface name and these options, each a list:

  - `:methods` - `{name, opts}` entries. `opts` takes `:in` and `:out`
    argument lists, `:no_reply` (a boolean, default `false`) and
    `:annotations`.
  - `:signals` - `{name, opts}` entries. `opts` takes `:args` and
    `:annotations`.
  - `:properties` - `{name, signature, access}` or
    `{name, signature, access, opts}` entries, where `access` is `:read`,
    `:write` or `:readwrite`. `opts` takes `:emits_changed` and
    `:annotations`.
  - `:annotations` - `{name, value}` pairs for the interface itself.

  An argument list holds `{name, signature}` pairs, where the name is an atom
  or a binary, so `[a: "i", b: "i"]` declares two named `int32` arguments. A
  bare signature declares an unnamed argument. Every argument and property
  signature must be a single complete type.

  `no_reply: true` adds the `org.freedesktop.DBus.Method.NoReply` annotation,
  and `:emits_changed` adds `org.freedesktop.DBus.Property.EmitsChangedSignal`
  when it differs from the default, so introspection reports both. Either
  annotation may be given directly instead. An `EmitsChangedSignal`
  annotation on the interface sets the default for its properties, as the
  D-Bus specification describes; otherwise the default is `true`.
  """

  use TypedStruct

  alias Rebus.Interface.Arg
  alias Rebus.Interface.Method
  alias Rebus.Interface.Property
  alias Rebus.Interface.Signal
  alias Rebus.Interface.Standard
  alias Rebus.Message
  alias Rebus.Signature
  alias Rebus.WireValue

  @typedoc "Whether a property can be read, written, or both."
  @type access :: :read | :write | :readwrite

  @typedoc "An introspection annotation: a name and a string value."
  @type annotation :: {name :: binary(), value :: binary()}

  typedstruct enforce: true do
    @typedoc "A declared D-Bus interface."
    field :name, binary()
    field :methods, %{binary() => Method.t()}, default: %{}
    field :signals, %{binary() => Signal.t()}, default: %{}
    field :properties, %{binary() => Property.t()}, default: %{}
    field :annotations, [annotation()], default: []
  end

  @typedoc """
  A reason why `new/2` rejected a declaration.

  Each reason carries the offending value, or the name of the member it
  belongs to.
  """
  @type validation_error ::
          {:invalid_option, term()}
          | {:invalid_interface_name, term()}
          | {:invalid_member_name, term()}
          | {:invalid_method, term()}
          | {:invalid_signal, term()}
          | {:invalid_property, term()}
          | {:invalid_arg, term()}
          | {:invalid_arg_name, term()}
          | {:invalid_signature, term()}
          | {:invalid_access, term()}
          | {:invalid_emits_changed, term()}
          | {:invalid_annotation, term()}
          | {:conflicting_annotation, {member :: binary(), annotation :: binary()}}
          | {:duplicate_member, binary()}
          | {:duplicate_property, binary()}
          | {:duplicate_annotation, binary()}
          | {:unix_fd_in_no_reply, binary()}

  @typedoc "A reason why `wrap_properties/2` could not wrap a value."
  @type property_error :: {:unknown_property, term()}

  @no_reply "org.freedesktop.DBus.Method.NoReply"
  @emits_changed "org.freedesktop.DBus.Property.EmitsChangedSignal"
  @emits_changed_values [true, false, :invalidates, :const]

  @options [:methods, :signals, :properties, :annotations]
  @message_options [:destination, :flags, :fds]

  # XML 1.0 cannot carry these characters even as character references, so an
  # annotation value holding one could not be introspected.
  @xml_forbidden ~r/[\x00-\x08\x0B\x0C\x0E-\x1F\x{FFFE}\x{FFFF}]/u

  @doc """
  Builds an interface declaration, validating every name, signature and
  annotation.

  Names follow the D-Bus grammar: the interface name is checked as an
  interface name, and method, signal, property and argument names as member
  names. Annotation names follow the interface-name grammar, and annotation
  values must be strings that XML can carry. Duplicate methods, signals,
  properties or annotations are rejected, as is a no-reply method with a Unix
  file descriptor (`h`) among its out arguments.

  ## Examples

      iex> {:ok, interface} =
      ...>   Rebus.Interface.new("org.example.Calculator",
      ...>     methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}]
      ...>   )
      iex> interface.methods["Add"].in_signature
      "ii"

      iex> Rebus.Interface.new("Calculator")
      {:error, {:invalid_interface_name, "Calculator"}}
  """
  @spec new(binary(), keyword()) :: {:ok, t()} | {:error, validation_error()}
  def new(name, opts \\ [])

  def new(name, opts) when is_list(opts) do
    with :ok <- validate_keys(opts, @options, {:invalid_option, opts}),
         :ok <- check(WireValue.valid_interface_name?(name), {:invalid_interface_name, name}),
         {:ok, annotations} <- build_annotations(Keyword.get(opts, :annotations, [])),
         {:ok, annotated} <- annotated_emits_changed(annotations),
         default_emits = if(annotated == nil, do: true, else: annotated),
         {:ok, methods} <-
           build_members(Keyword.get(opts, :methods, []), &build_method/1, :duplicate_member),
         {:ok, signals} <-
           build_members(Keyword.get(opts, :signals, []), &build_signal/1, :duplicate_member),
         {:ok, properties} <-
           build_members(
             Keyword.get(opts, :properties, []),
             &build_property(&1, default_emits),
             :duplicate_property
           ) do
      {:ok,
       %__MODULE__{
         name: name,
         methods: methods,
         signals: signals,
         properties: properties,
         annotations: annotations
       }}
    end
  end

  def new(_name, opts), do: {:error, {:invalid_option, opts}}

  @doc """
  Builds an interface declaration, raising `ArgumentError` when it is invalid.
  """
  @spec new!(binary(), keyword()) :: t()
  def new!(name, opts \\ []) do
    case new(name, opts) do
      {:ok, interface} -> interface
      {:error, reason} -> raise ArgumentError, "invalid D-Bus interface: #{inspect(reason)}"
    end
  end

  @doc "The standard `org.freedesktop.DBus.Peer` interface."
  @spec peer() :: t()
  def peer, do: Standard.peer()

  @doc "The standard `org.freedesktop.DBus.Introspectable` interface."
  @spec introspectable() :: t()
  def introspectable, do: Standard.introspectable()

  @doc "The standard `org.freedesktop.DBus.Properties` interface."
  @spec properties() :: t()
  def properties, do: Standard.properties()

  @doc "The standard `org.freedesktop.DBus.ObjectManager` interface."
  @spec object_manager() :: t()
  def object_manager, do: Standard.object_manager()

  @doc """
  Builds a signal message for a signal declared by `interface`, typed by its
  declaration.

  `opts` takes `:destination`, `:flags` and `:fds`, as `Rebus.Message.new/2`
  does. Returns `{:error, :unknown_signal}` when the interface does not
  declare `member`, and `Rebus.Message.new/2`'s errors otherwise; a body that
  does not fit the declared signature is `{:error, :invalid_body}`.

  ## Example

      iex> interface =
      ...>   Rebus.Interface.new!("org.example.Calculator",
      ...>     signals: [{"Computed", args: [sum: "i"]}]
      ...>   )
      iex> {:ok, message} =
      ...>   Rebus.Interface.signal(interface, "/org/example/Calc", "Computed", [42])
      iex> Rebus.Message.signature(message)
      "i"
  """
  @spec signal(t(), binary(), binary(), [term()], keyword()) ::
          {:ok, Message.t()}
          | {:error, :unknown_signal | {:invalid_option, term()} | Message.construction_error()}
  def signal(%__MODULE__{} = interface, path, member, body, opts \\ []) do
    with {:ok, %Signal{signature: signature}} <- fetch_signal(interface, member),
         :ok <- validate_keys(opts, @message_options, {:invalid_option, opts}) do
      Message.new(
        :signal,
        [
          path: path,
          interface: interface.name,
          member: member,
          signature: signature,
          body: body
        ] ++ opts
      )
    end
  end

  @doc """
  Builds an `org.freedesktop.DBus.Properties.PropertiesChanged` signal for
  properties declared by `interface`.

  `changed` maps property names to their new values, which are typed by the
  declaration. `invalidated` names properties that changed without their
  values. Each property's `emits_changed` decides where it may appear: one
  that emits `true` only in `changed`, one that emits `:invalidates` only in
  `invalidated`, and one that emits `false` or `:const` in neither.

  `opts` takes `:destination`, `:flags` and `:fds`, as `signal/5` does, so a
  value of a property declared with a Unix file descriptor type can refer to
  a descriptor.

  Returns `{:error, {:unknown_property, name}}` for a property the interface
  does not declare, `{:error, {:invalid_property_change, name}}` for one whose
  declaration does not allow it where it appears, and `Rebus.Message.new/2`'s
  errors otherwise.
  """
  @spec properties_changed(t(), binary(), %{binary() => term()}, [binary()], keyword()) ::
          {:ok, Message.t()}
          | {:error,
             property_error()
             | {:invalid_property_change, binary()}
             | {:invalid_option, term()}
             | Message.construction_error()}
  def properties_changed(%__MODULE__{} = interface, path, changed, invalidated, opts \\ [])
      when is_map(changed) and is_list(invalidated) do
    with {:ok, changed_body} <- wrap_properties(interface, changed),
         :ok <- check_change(interface, Map.keys(changed), true),
         :ok <- check_change(interface, invalidated, :invalidates),
         :ok <- validate_keys(opts, @message_options, {:invalid_option, opts}) do
      Message.new(
        :signal,
        [
          path: path,
          interface: Standard.properties().name,
          member: "PropertiesChanged",
          signature: "sa{sv}as",
          body: [interface.name, changed_body, invalidated]
        ] ++ opts
      )
    end
  end

  @doc """
  Wraps property values as the `a{sv}` body element that `GetAll` replies and
  `PropertiesChanged` signals carry, typing each value by its declaration.

  Entries are sorted by property name. Values are not checked against their
  signatures here; the message they are placed in reports one that does not
  fit as `:invalid_body`.

  ## Example

      iex> interface =
      ...>   Rebus.Interface.new!("org.example.Calculator",
      ...>     properties: [{"Precision", "u", :readwrite}, {"Version", "s", :read}]
      ...>   )
      iex> Rebus.Interface.wrap_properties(interface, %{"Version" => "1.0", "Precision" => 3})
      {:ok, [{"Precision", {"u", 3}}, {"Version", {"s", "1.0"}}]}
  """
  @spec wrap_properties(t(), %{binary() => term()}) ::
          {:ok, [{binary(), {binary(), term()}}]} | {:error, property_error()}
  def wrap_properties(%__MODULE__{properties: properties}, values) when is_map(values) do
    values
    |> Enum.sort_by(fn {name, _value} -> name end)
    |> map_while(fn {name, value} ->
      case Map.fetch(properties, name) do
        {:ok, %Property{signature: signature}} -> {:ok, {name, {signature, value}}}
        :error -> {:error, {:unknown_property, name}}
      end
    end)
  end

  @doc """
  Wraps property values as `wrap_properties/2` does, raising `ArgumentError`
  for a property the interface does not declare.
  """
  @spec wrap_properties!(t(), %{binary() => term()}) :: [{binary(), {binary(), term()}}]
  def wrap_properties!(interface, values) do
    case wrap_properties(interface, values) do
      {:ok, wrapped} -> wrapped
      {:error, reason} -> raise ArgumentError, "cannot wrap D-Bus properties: #{inspect(reason)}"
    end
  end

  defp fetch_signal(%__MODULE__{signals: signals}, member) do
    case Map.fetch(signals, member) do
      {:ok, signal} -> {:ok, signal}
      :error -> {:error, :unknown_signal}
    end
  end

  # Checks that every named property emits `PropertiesChanged` as `emits`.
  defp check_change(%__MODULE__{properties: properties}, names, emits),
    do: Enum.find_value(names, :ok, &change_error(properties, &1, emits))

  defp change_error(properties, name, emits) do
    case Map.fetch(properties, name) do
      {:ok, %Property{emits_changed: ^emits}} ->
        nil

      {:ok, %Property{}} ->
        {:error, {:invalid_property_change, name}}

      :error ->
        {:error, {:unknown_property, name}}
    end
  end

  # Builds a name-keyed map of members, rejecting a repeated name with
  # `{duplicate, name}`.
  defp build_members(entries, build, duplicate) when is_list(entries) do
    reduce_ok(entries, %{}, fn entry, members ->
      with {:ok, name, member} <- build.(entry), do: put_member(members, name, member, duplicate)
    end)
  end

  defp build_members(entries, _build, _duplicate), do: {:error, {:invalid_option, entries}}

  defp put_member(members, name, member, duplicate) do
    if Map.has_key?(members, name),
      do: {:error, {duplicate, name}},
      else: {:ok, Map.put(members, name, member)}
  end

  defp build_method({name, opts}) when is_list(opts) do
    with :ok <- validate_keys(opts, [:in, :out, :no_reply, :annotations], {:invalid_method, name}),
         :ok <- check(WireValue.valid_member_name?(name), {:invalid_member_name, name}),
         {:ok, in_args} <- build_args(Keyword.get(opts, :in, [])),
         {:ok, out_args} <- build_args(Keyword.get(opts, :out, [])),
         {:ok, in_signature} <- concat_signature(in_args),
         {:ok, out_signature} <- concat_signature(out_args),
         {:ok, annotations} <- build_annotations(Keyword.get(opts, :annotations, [])),
         {:ok, no_reply?, annotations} <- resolve_no_reply(name, opts, annotations),
         :ok <-
           check(
             not (no_reply? and String.contains?(out_signature, "h")),
             {:unix_fd_in_no_reply, name}
           ) do
      {:ok, name,
       %Method{
         name: name,
         in_args: in_args,
         out_args: out_args,
         in_signature: in_signature,
         out_signature: out_signature,
         no_reply?: no_reply?,
         annotations: annotations
       }}
    end
  end

  defp build_method(entry), do: {:error, {:invalid_method, entry}}

  defp build_signal({name, opts}) when is_list(opts) do
    with :ok <- validate_keys(opts, [:args, :annotations], {:invalid_signal, name}),
         :ok <- check(WireValue.valid_member_name?(name), {:invalid_member_name, name}),
         {:ok, args} <- build_args(Keyword.get(opts, :args, [])),
         {:ok, signature} <- concat_signature(args),
         {:ok, annotations} <- build_annotations(Keyword.get(opts, :annotations, [])) do
      {:ok, name, %Signal{name: name, args: args, signature: signature, annotations: annotations}}
    end
  end

  defp build_signal(entry), do: {:error, {:invalid_signal, entry}}

  defp build_property({name, signature, access}, default_emits),
    do: build_property({name, signature, access, []}, default_emits)

  defp build_property({name, signature, access, opts}, default_emits) when is_list(opts) do
    with :ok <- validate_keys(opts, [:emits_changed, :annotations], {:invalid_property, name}),
         :ok <- check(WireValue.valid_member_name?(name), {:invalid_member_name, name}),
         :ok <- check(single_complete_type?(signature), {:invalid_signature, signature}),
         :ok <- check(access in [:read, :write, :readwrite], {:invalid_access, access}),
         {:ok, annotations} <- build_annotations(Keyword.get(opts, :annotations, [])),
         {:ok, emits_changed, annotations} <-
           resolve_emits_changed(name, opts, annotations, default_emits) do
      {:ok, name,
       %Property{
         name: name,
         signature: signature,
         access: access,
         emits_changed: emits_changed,
         annotations: annotations
       }}
    end
  end

  defp build_property(entry, _default_emits), do: {:error, {:invalid_property, entry}}

  defp build_args(args) when is_list(args), do: map_while(args, &build_arg/1)
  defp build_args(args), do: {:error, {:invalid_arg, args}}

  defp build_arg(signature) when is_binary(signature), do: build_arg(nil, signature)

  defp build_arg({name, signature})
       when is_binary(signature) and (is_binary(name) or name == nil),
       do: build_arg(name, signature)

  defp build_arg({name, signature}) when is_binary(signature) and is_atom(name),
    do: build_arg(Atom.to_string(name), signature)

  defp build_arg(arg), do: {:error, {:invalid_arg, arg}}

  defp build_arg(name, signature) do
    with :ok <-
           check(name == nil or WireValue.valid_member_name?(name), {:invalid_arg_name, name}),
         :ok <- check(single_complete_type?(signature), {:invalid_signature, signature}) do
      {:ok, %Arg{name: name, signature: signature}}
    end
  end

  # The concatenation of complete types is valid unless it exceeds the
  # 255-byte signature limit, which `Signature.parse/1` enforces.
  defp concat_signature(args) do
    signature = Enum.map_join(args, & &1.signature)

    case Signature.parse(signature) do
      {:ok, _types} -> {:ok, signature}
      {:error, _reason} -> {:error, {:invalid_signature, signature}}
    end
  end

  defp single_complete_type?(signature) when is_binary(signature),
    do: match?({:ok, [_type]}, Signature.parse(signature))

  defp single_complete_type?(_signature), do: false

  defp build_annotations(annotations) when is_list(annotations) do
    with {:ok, acc} <- reduce_ok(annotations, [], &add_annotation/2) do
      {:ok, Enum.reverse(acc)}
    end
  end

  defp build_annotations(annotations), do: {:error, {:invalid_annotation, annotations}}

  defp add_annotation({name, value} = annotation, acc)
       when is_binary(name) and is_binary(value) do
    cond do
      not WireValue.valid_interface_name?(name) or not valid_annotation_value?(value) ->
        {:error, {:invalid_annotation, annotation}}

      List.keymember?(acc, name, 0) ->
        {:error, {:duplicate_annotation, name}}

      true ->
        {:ok, [annotation | acc]}
    end
  end

  defp add_annotation(annotation, _acc), do: {:error, {:invalid_annotation, annotation}}

  defp valid_annotation_value?(value),
    do: WireValue.valid_string?(value) and not Regex.match?(@xml_forbidden, value)

  # Reconciles the `:no_reply` option with a NoReply annotation, adding the
  # annotation when only the option says the method is no-reply.
  defp resolve_no_reply(name, opts, annotations) do
    with {:ok, annotated} <- annotated_no_reply(annotations) do
      case {Keyword.fetch(opts, :no_reply), annotated} do
        {:error, nil} -> {:ok, false, annotations}
        {:error, value} -> {:ok, value, annotations}
        {{:ok, value}, _} when not is_boolean(value) -> {:error, {:invalid_method, name}}
        {{:ok, true}, nil} -> {:ok, true, annotations ++ [{@no_reply, "true"}]}
        {{:ok, false}, nil} -> {:ok, false, annotations}
        {{:ok, value}, value} -> {:ok, value, annotations}
        {{:ok, _value}, _} -> conflicting(name, @no_reply)
      end
    end
  end

  defp annotated_no_reply(annotations) do
    case List.keyfind(annotations, @no_reply, 0) do
      nil -> {:ok, nil}
      {_, "true"} -> {:ok, true}
      {_, "false"} -> {:ok, false}
      annotation -> {:error, {:invalid_annotation, annotation}}
    end
  end

  # Reconciles the `:emits_changed` option with an EmitsChangedSignal
  # annotation, adding the annotation when the option departs from the
  # default that introspection would otherwise imply.
  defp resolve_emits_changed(name, opts, annotations, default) do
    with {:ok, annotated} <- annotated_emits_changed(annotations) do
      case {Keyword.fetch(opts, :emits_changed), annotated} do
        {:error, nil} ->
          {:ok, default, annotations}

        {:error, value} ->
          {:ok, value, annotations}

        {{:ok, ^default}, nil} ->
          {:ok, default, annotations}

        {{:ok, value}, nil} when value in @emits_changed_values ->
          {:ok, value, annotations ++ [{@emits_changed, emits_changed_text(value)}]}

        {{:ok, value}, value} when value in @emits_changed_values ->
          {:ok, value, annotations}

        {{:ok, value}, _} when value in @emits_changed_values ->
          conflicting(name, @emits_changed)

        {{:ok, value}, _} ->
          {:error, {:invalid_emits_changed, value}}
      end
    end
  end

  defp annotated_emits_changed(annotations) do
    case List.keyfind(annotations, @emits_changed, 0) do
      nil -> {:ok, nil}
      {_, "true"} -> {:ok, true}
      {_, "false"} -> {:ok, false}
      {_, "invalidates"} -> {:ok, :invalidates}
      {_, "const"} -> {:ok, :const}
      annotation -> {:error, {:invalid_annotation, annotation}}
    end
  end

  defp emits_changed_text(value), do: to_string(value)

  defp conflicting(name, annotation), do: {:error, {:conflicting_annotation, {name, annotation}}}

  # Accepts a keyword list holding only `keys`, each at most once.
  defp validate_keys(opts, keys, error) do
    if is_list(opts) and Keyword.keyword?(opts) do
      opt_keys = Keyword.keys(opts)

      check(
        Enum.all?(opt_keys, &(&1 in keys)) and opt_keys == Enum.uniq(opt_keys),
        error
      )
    else
      {:error, error}
    end
  end

  defp check(true, _error), do: :ok
  defp check(false, error), do: {:error, error}

  # Folds `fun` over `list` while it returns `{:ok, acc}`, stopping at the
  # first error.
  defp reduce_ok(list, acc, fun) do
    Enum.reduce_while(list, {:ok, acc}, fn item, {:ok, acc} ->
      case fun.(item, acc) do
        {:ok, acc} -> {:cont, {:ok, acc}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp map_while(list, fun) do
    with {:ok, acc} <- reduce_ok(list, [], &prepend_ok(fun.(&1), &2)) do
      {:ok, Enum.reverse(acc)}
    end
  end

  defp prepend_ok({:ok, value}, acc), do: {:ok, [value | acc]}
  defp prepend_ok({:error, _} = error, _acc), do: error
end
