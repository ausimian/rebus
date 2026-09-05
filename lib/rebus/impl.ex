defmodule Rebus.Impl do
  @moduledoc false

  # The implementation modules a connection and the address-list walk use. One
  # map, built once per `Rebus.connect/2` call and carried in the connection's
  # internal arguments.

  @default %{
    transport: Rebus.Transport.Socket,
    identity: Rebus.Identity.Posix,
    resolver: Rebus.Resolver.Inet,
    clock: Rebus.Clock.System,
    connector: Rebus.Connector.Supervised,
    hooks: Rebus.Connection.Hooks.Default
  }

  # Substituting an implementation is a test affordance, so the private
  # `:__impl__` option is read only in this project's own test build. Mix
  # compiles a dependency in `:prod` regardless of the parent application's
  # environment, so this is false wherever Rebus is used as a library and
  # `build/1` then compiles to a clause that ignores its argument entirely:
  # released code has no path that reads `:__impl__`.
  @test_seams? Mix.env() == :test

  @type t :: %{
          transport: module(),
          identity: module(),
          resolver: module(),
          clock: module(),
          connector: module(),
          hooks: module()
        }

  @spec default() :: t()
  def default, do: @default

  if @test_seams? do
    @doc """
    Reads the private `:__impl__` option out of a caller's options.
    """
    @spec from_options(keyword()) :: t()
    def from_options(opts), do: build(Keyword.get(opts, :__impl__, []))

    @doc """
    Merges implementation overrides over the defaults, ignoring unknown keys.
    """
    @spec build(term()) :: t()
    def build(overrides) when is_list(overrides) or is_map(overrides),
      do: Map.merge(@default, Map.take(Map.new(overrides), Map.keys(@default)))

    def build(_overrides), do: @default
  else
    @spec from_options(keyword()) :: t()
    def from_options(_opts), do: @default
  end
end
