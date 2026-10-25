defmodule Rebus.Object do
  @moduledoc """
  A behaviour for exported objects, run as a GenServer.

  `Rebus.Service` hands each call on an exported object to a handler process
  as a message, and leaves the process to answer it. `use Rebus.Object`
  packages that for the common case: `start_link/3` starts a GenServer that
  exports the object and turns each call into a callback, answering it from
  the callback's return value.

  ## Example

      defmodule Calculator do
        use Rebus.Object

        @calculator Rebus.Interface.new!("org.example.Calculator",
                      methods: [{"Add", in: [a: "i", b: "i"], out: [sum: "i"]}],
                      properties: [{"Precision", "u", :readwrite}]
                    )

        @impl true
        def interfaces, do: [@calculator]

        @impl true
        def init(precision), do: {:ok, %{precision: precision}}

        @impl true
        def handle_method(%{member: "Add"}, [a, b], state), do: {:reply, [a + b], state}

        @impl true
        def handle_get_property(_interface, "Precision", state),
          do: {:ok, state.precision, state}

        @impl true
        def handle_set_property(_interface, "Precision", precision, state) when precision <= 16,
          do: {:ok, %{state | precision: precision}}

        def handle_set_property(_interface, "Precision", _precision, state),
          do: {:error, "org.freedesktop.DBus.Error.InvalidArgs", "Precision above 16", state}
      end

      {:ok, conn} = Rebus.connect(:session)
      {:ok, _pid} = Rebus.Object.start_link(Calculator, 2, conn: conn, path: "/org/example/Calculator")

  `use Rebus.Object` also defines `child_spec/1`, which takes `{arg, opts}`
  as `start_link/3` takes them, so the object can be supervised:

      children = [{Calculator, {2, conn: conn, path: "/org/example/Calculator"}}]

  The child is `:transient`. An object that crashes is restarted and exports
  itself again. An object whose connection stops exits with
  `{:shutdown, :disconnected}`, which a supervisor treats as a normal exit,
  so it is not restarted: a restart would try to export on the same stopped
  connection and fail.

  `:temporary` is the only other sensible value, for an object that should
  never be restarted, such as one started on demand under a
  `DynamicSupervisor`. Avoid `:permanent`. It restarts an object after its
  connection stops, every restart fails, and the supervisor gives up after
  its restart limit and exits, taking its other children with it.

  `use Rebus.Object` takes the keys of `t:Supervisor.child_spec/0`, such as
  `:restart` or `:id`, as `use GenServer` does.

  ## Lifetime

  The object exports itself before its `c:init/1` runs, and its export lives
  as long as the process. When the process exits, the connection fails every
  call still waiting for its answer with `org.freedesktop.DBus.Error.Failed`,
  and when a supervisor restarts it, the new process exports the object
  again.

  Exceptions in callbacks are not rescued. The process crashes, the caller
  receives `Failed`, and the exception's detail stays off the wire.

  The object stops with `{:shutdown, :disconnected}` when its connection
  stops, since its export is gone with the connection.

  ## Methods

  Each call to a declared method reaches `c:handle_method/3` with its
  arguments, which have the declared types. The callback answers it by
  returning `{:reply, body, state}` or `{:error, name, message, state}`.
  Returning `{:noreply, state}` leaves the call open, to be answered later by
  any process holding the `Rebus.Service.Call`:

      @impl true
      def handle_method(%{member: "SlowAdd"} = call, [a, b], state) do
        Task.start(fn -> Rebus.Service.reply(call, [a + b]) end)
        {:noreply, state}
      end

  A reply that cannot be sent while the call is still open, such as a body
  that does not fit the method's out-signature, is a bug in the object, which
  stops with `{:invalid_reply, reason}`. Replying to a call that was already
  answered or has expired, or a reply whose delivery timed out, is not a bug,
  and the object carries on.

  ## Properties

  `Get` reaches `c:handle_get_property/3`, whose value is typed by the
  property's declaration. `GetAll` calls it for every readable property of
  the interface, in name order, and answers with the values together, or with
  the first error returned. `Set` reaches `c:handle_set_property/4` with the
  new value, already checked against the declared type.

  After a successful `Set`, the object emits
  `org.freedesktop.DBus.Properties.PropertiesChanged` as the property's
  `emits_changed` asks: with the new value when it is `true`, with the
  property's name only when it is `:invalidates`, and not at all when it is
  `false` or `:const`. A write-only property is never announced, since that
  would broadcast a value no one may read. A callback that returns
  `{:ok, state, emit: false}` suppresses the signal, for example when it
  stores a value other than the one it was given, and can emit its own with
  `Rebus.Service.properties_changed/6`.

  The callbacks cannot carry Unix file descriptors, so an object whose
  properties have a type containing `h` serves them through
  `Rebus.Service` instead.

  An object that declares readable properties without defining
  `c:handle_get_property/3`, or writable ones without
  `c:handle_set_property/4`, answers those calls with
  `org.freedesktop.DBus.Error.NotSupported`.

  ## Keeping your own loop

  A GenServer that exports itself with `Rebus.Service.export/4` can still map
  its calls to these callbacks by passing them to `dispatch/4` from its own
  `handle_info/2`:

      @impl GenServer
      def handle_info({_ref, %Rebus.Service.Call{}} = call, state),
        do: Rebus.Object.dispatch(call, __MODULE__, state)

  Such a module defines `c:handle_method/3` and the property callbacks it
  needs as plain functions. It cannot declare `@behaviour Rebus.Object`
  beside `GenServer`, since both behaviours have `init/1`, `handle_info/2`
  and `terminate/2` callbacks.
  """

  alias Rebus.Interface
  alias Rebus.Interface.Property
  alias Rebus.Service
  alias Rebus.Service.Call

  @properties "org.freedesktop.DBus.Properties"
  @not_supported "org.freedesktop.DBus.Error.NotSupported"

  # Reply errors after which the call is complete or the connection gone, so
  # the object has nothing left to do for it.
  @settled_reply_errors [:unknown_call, :timeout, :disconnected]

  @export_options [:subtree, :call_timeout, :timeout]
  @gen_server_options [:name, :debug, :spawn_opt, :hibernate_after]
  @start_options [:conn, :path, :interfaces] ++ @export_options ++ @gen_server_options

  @typedoc "The object's own state, as its callbacks return it."
  @type state :: term()

  @typedoc "What `c:handle_method/3` returns."
  @type method_result ::
          {:reply, body :: [term()], state()}
          | {:reply, body :: [term()], state(), [fds: [Rebus.Message.unix_fd()]]}
          | {:error, name :: binary(), message :: binary(), state()}
          | {:noreply, state()}
          | {:stop, reason :: term(), state()}

  @doc "The interfaces the object exports, unless `start_link/3` is given `:interfaces`."
  @callback interfaces() :: [Interface.t()]

  @doc """
  Initialises the object's state from the `arg` given to `start_link/3`.

  The object is already exported when this runs, so calls that arrive in
  the meantime wait in the mailbox. Returning `{:stop, reason}` or `:ignore`
  withdraws the export.
  """
  @callback init(arg :: term()) :: {:ok, state()} | {:stop, reason :: term()} | :ignore

  @doc """
  Handles a call to a declared method, whose arguments are `args`.

  - `{:reply, body, state}` answers with `body`, encoded with the method's
    out-signature. A fourth element, `fds: fds`, sends descriptors with the
    reply, as `Rebus.Service.reply/3` does.
  - `{:error, name, message, state}` answers with the D-Bus error `name`.
  - `{:noreply, state}` leaves the call open, for `Rebus.Service.reply/3` or
    `Rebus.Service.reply_error/4` to answer later, from any process.
  - `{:stop, reason, state}` stops the object, which fails the call.
  """
  @callback handle_method(call :: Call.t(), args :: [term()], state()) :: method_result()

  @doc """
  Reads the property `property` of `interface`, for `Get` and `GetAll`.

  Only readable properties of exported interfaces reach this callback.
  """
  @callback handle_get_property(interface :: binary(), property :: binary(), state()) ::
              {:ok, value :: term(), state()}
              | {:error, name :: binary(), message :: binary(), state()}

  @doc """
  Writes `value` to the property `property` of `interface`, for `Set`.

  Only writable properties of exported interfaces reach this callback, and
  `value` has the declared type. `{:ok, state}` answers the call and emits
  `PropertiesChanged` as the property declares; `{:ok, state, emit: false}`
  answers without emitting it.
  """
  @callback handle_set_property(
              interface :: binary(),
              property :: binary(),
              value :: term(),
              state()
            ) ::
              {:ok, state()}
              | {:ok, state(), [emit: boolean()]}
              | {:error, name :: binary(), message :: binary(), state()}

  @doc "Handles any other message, as `c:GenServer.handle_info/2` does."
  @callback handle_info(msg :: term(), state()) ::
              {:noreply, state()} | {:stop, reason :: term(), state()}

  @doc "Called when the object stops, as `c:GenServer.terminate/2` is."
  @callback terminate(reason :: term(), state()) :: term()

  @optional_callbacks handle_get_property: 3,
                      handle_set_property: 4,
                      handle_info: 2,
                      terminate: 2

  @doc false
  defmacro __using__(opts) do
    quote location: :keep, bind_quoted: [opts: opts] do
      @behaviour Rebus.Object

      @rebus_object_child_spec opts

      @doc """
      Returns a specification to start this object under a supervisor, from
      the `arg` and options `Rebus.Object.start_link/3` takes.
      """
      def child_spec({arg, start_opts}) do
        Supervisor.child_spec(
          %{
            id: __MODULE__,
            start: {Rebus.Object, :start_link, [__MODULE__, arg, start_opts]},
            restart: :transient
          },
          @rebus_object_child_spec
        )
      end

      defoverridable child_spec: 1
    end
  end

  @doc """
  Starts an object of `module`, exports it, and runs `module.init(arg)`.

  ## Options

  - `:conn` - the connection to export on. Required.
  - `:path` - the object path to export at. Required.
  - `:interfaces` - the interfaces to export. Defaults to
    `module.interfaces()`.
  - `:subtree`, `:call_timeout` and `:timeout` - as for
    `Rebus.Service.export/4`. `:timeout` bounds the export, not the
    GenServer's `init/1`.
  - `:name` - a name for the process in the VM, as for
    `GenServer.start_link/3`: an atom, `{:global, term}` or
    `{:via, module, term}`. It is not a bus name; clients find the object by
    its `:path`, under a well-known name the connection requests with
    `Rebus.Bus.request_name/4`.
  - `:debug`, `:spawn_opt` and `:hibernate_after` - passed to
    `GenServer.start_link/3`.

  Any other option raises `ArgumentError`.

  ## Return values

  As for `GenServer.start_link/3`, with `{:error, reason}` for any
  `Rebus.Service.export/4` error, such as
  `{:error, {:already_exported, path, interface}}`.
  """
  @spec start_link(module(), term(), keyword()) :: GenServer.on_start()
  def start_link(module, arg, opts) when is_atom(module) and is_list(opts) do
    opts = Keyword.validate!(opts, @start_options)
    conn = Keyword.fetch!(opts, :conn)
    path = Keyword.fetch!(opts, :path)

    export_opts = Keyword.take(opts, @export_options)

    GenServer.start_link(
      Rebus.Object.Server,
      {module, arg, conn, path, Keyword.get(opts, :interfaces), export_opts},
      Keyword.take(opts, @gen_server_options)
    )
  end

  @doc """
  Answers a call by passing it to `module`'s callbacks with `state`.

  This is what the process `start_link/3` starts does with each call, for a
  GenServer that keeps its own loop. Give it the `{ref, call}` message the
  connection sent, and return its result from `handle_info/2`: either
  `{:noreply, state}` or `{:stop, reason, state}`. `module` defines
  `c:handle_method/3`, and `c:handle_get_property/3` and
  `c:handle_set_property/4` as its properties need.

  ## Options

  - `:interfaces` - the exported interfaces, which property calls are
    typed by. Defaults to `module.interfaces()`, which is only called for a
    property call.
  """
  @spec dispatch({reference(), Call.t()}, module(), state(), keyword()) ::
          {:noreply, state()} | {:stop, reason :: term(), state()}
  def dispatch({ref, %Call{export: ref} = call}, module, state, opts \\ [])
      when is_atom(module) and is_list(opts) do
    case call do
      %Call{interface: @properties, member: "Get", body: [interface, name]} ->
        interface = fetch_interface!(module, opts, interface)
        get_property(call, module, interface, fetch_property!(module, interface, name), state)

      %Call{interface: @properties, member: "GetAll", body: [interface]} ->
        get_all(call, module, fetch_interface!(module, opts, interface), state)

      %Call{interface: @properties, member: "Set", body: [interface, name, {_sig, value}]} ->
        interface = fetch_interface!(module, opts, interface)
        property = fetch_property!(module, interface, name)
        set_property(call, module, interface, property, value, state)

      %Call{} ->
        call_method(call, module, state)
    end
  end

  defp call_method(call, module, state) do
    case module.handle_method(call, call.body, state) do
      {:reply, body, state} ->
        answered(Service.reply(call, body), state)

      {:reply, body, state, opts} when is_list(opts) ->
        answered(Service.reply(call, body, opts), state)

      {:error, name, message, state} ->
        answered(Service.reply_error(call, name, message), state)

      {:noreply, state} ->
        {:noreply, state}

      {:stop, reason, state} ->
        {:stop, reason, state}

      other ->
        {:stop, {:bad_return_value, other}, state}
    end
  end

  defp get_property(call, module, %Interface{} = interface, %Property{} = property, state) do
    if function_exported?(module, :handle_get_property, 3) do
      module
      |> read_property(interface.name, property, state)
      |> answer_read(call, state, &[{property.signature, &1}])
    else
      not_supported(call, state)
    end
  end

  defp get_all(call, module, %Interface{} = interface, state) do
    if function_exported?(module, :handle_get_property, 3) do
      interface.properties
      |> Map.values()
      |> Enum.filter(&(&1.access in [:read, :readwrite]))
      |> Enum.sort_by(& &1.name)
      |> Enum.reduce_while({:ok, %{}, state}, &read_into(module, interface.name, &1, &2))
      |> answer_read(call, state, &[Interface.wrap_properties!(interface, &1)])
    else
      not_supported(call, state)
    end
  end

  defp read_into(module, interface, %Property{} = property, {:ok, values, state}) do
    case read_property(module, interface, property, state) do
      {:ok, value, state} -> {:cont, {:ok, Map.put(values, property.name, value), state}}
      other -> {:halt, other}
    end
  end

  defp answer_read({:ok, value, state}, call, _state, body),
    do: answered(Service.reply(call, body.(value)), state)

  defp answer_read({:error, name, message, state}, call, _state, _body),
    do: answered(Service.reply_error(call, name, message), state)

  defp answer_read({:bad_return_value, _other} = reason, _call, state, _body),
    do: {:stop, reason, state}

  defp read_property(module, interface, %Property{name: name}, state) do
    case module.handle_get_property(interface, name, state) do
      {:ok, _value, _state} = ok -> ok
      {:error, _name, _message, _state} = error -> error
      other -> {:bad_return_value, other}
    end
  end

  defp set_property(call, module, interface, %Property{} = property, value, state) do
    if function_exported?(module, :handle_set_property, 4) do
      case module.handle_set_property(interface.name, property.name, value, state) do
        {:ok, state} ->
          set_answered(call, interface, property, value, true, state)

        {:ok, state, opts} when is_list(opts) ->
          set_answered(call, interface, property, value, Keyword.get(opts, :emit, true), state)

        {:error, name, message, state} ->
          answered(Service.reply_error(call, name, message), state)

        other ->
          {:stop, {:bad_return_value, other}, state}
      end
    else
      not_supported(call, state)
    end
  end

  defp set_answered(call, interface, property, value, emit?, state) do
    with {:noreply, state} <- answered(Service.reply(call, []), state) do
      if emit?, do: emit_change(call, interface, property, value)
      {:noreply, state}
    end
  end

  # The value is the one the caller sent, already decoded under the declared
  # signature, so it fits the signal. A failure to send the signal is the
  # connection's, and the Set itself has succeeded. A write-only property is
  # never announced, since that would broadcast a value no one may read.
  defp emit_change(_call, _interface, %Property{access: :write}, _value), do: :ok

  defp emit_change(call, interface, %Property{emits_changed: emits} = property, value) do
    case emits do
      true ->
        Service.properties_changed(call.conn, call.path, interface, %{property.name => value}, [])

      :invalidates ->
        Service.properties_changed(call.conn, call.path, interface, %{}, [property.name])

      _never ->
        :ok
    end
  end

  defp not_supported(call, state),
    do: answered(Service.reply_error(call, @not_supported, "Not supported"), state)

  defp answered(:ok, state), do: {:noreply, state}

  defp answered({:error, reason}, state) when reason in @settled_reply_errors,
    do: {:noreply, state}

  defp answered({:error, reason}, state), do: {:stop, {:invalid_reply, reason}, state}

  # The connection has already checked that the interface is exported on the
  # object and declares the property, so a miss here means the object was
  # exported with other interfaces than the ones dispatch was given.
  defp fetch_interface!(module, opts, name) do
    interfaces = Keyword.get_lazy(opts, :interfaces, &module.interfaces/0)

    case Enum.find(interfaces, &match?(%Interface{name: ^name}, &1)) do
      %Interface{} = interface ->
        interface

      nil ->
        raise ArgumentError,
              "#{inspect(module)} received a property call for interface #{inspect(name)}, " <>
                "which is not among the interfaces given to Rebus.Object.dispatch/4"
    end
  end

  defp fetch_property!(module, %Interface{properties: properties} = interface, name) do
    case Map.fetch(properties, name) do
      {:ok, property} ->
        property

      :error ->
        raise ArgumentError,
              "#{inspect(module)} received a call for property #{inspect(name)} of " <>
                "#{inspect(interface.name)}, which its interface given to Rebus.Object.dispatch/4 " <>
                "does not declare"
    end
  end
end
