defmodule Rebus.MixProject do
  use Mix.Project

  @version "0.3.0"
  @source_url "https://github.com/ausimian/rebus"

  def project do
    [
      app: :rebus,
      version: System.get_env("VERSION_OVERRIDE", @version),
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      compilers: compilers(),
      make_env: &make_env/0,
      aliases: aliases(),
      deps: deps(),
      description: description(),
      docs: docs(),
      package: package(),
      source_url: @source_url,
      elixirc_paths: elixirc_paths(Mix.env()),
      test_coverage: [
        ignore_modules: [
          Rebus.TestServer,
          Rebus.TestImpl,
          Rebus.TestImpl.Clock,
          Rebus.TestImpl.Connector,
          Rebus.TestImpl.Hooks,
          Rebus.TestImpl.Identity,
          Rebus.TestImpl.Resolver
        ]
      ],
      dialyzer: [
        plt_core_path: "_build/plts",
        plt_file:
          {:no_warn,
           "_build/plts/dialyzer-#{System.otp_release()}-#{System.version()}-#{Mix.env()}.plt"}
      ]
    ] ++ build_path()
  end

  def cli do
    [
      preferred_envs: [precommit: :test]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/lib"]
  defp elixirc_paths(_), do: ["lib"]

  # The Rebus.UnixFD NIF is built only on Linux and macOS, the platforms where
  # Rebus passes descriptors, and only when Unix fd support is enabled;
  # otherwise Rebus.UnixFD returns {:error, :unsupported}.
  defp compilers do
    if unix_fds?() and :os.type() in [{:unix, :linux}, {:unix, :darwin}],
      do: [:elixir_make] ++ Mix.compilers(),
      else: Mix.compilers()
  end

  # An application enables Unix fd support with `config :rebus, unix_fds: true`
  # in its config/config.exs, which Mix loads before it evaluates this file
  # for a dependency. Rebus.UnixFD.NIF reads the same key with compile_env/3,
  # so changing it recompiles the dependency and re-evaluates this file.
  # Rebus's own config/config.exs is loaded only after this file, so for
  # Rebus's own builds the default repeats what that file sets.
  defp unix_fds? do
    Application.get_env(:rebus, :unix_fds, own_build?() and not no_unix_fds?())
  end

  # Mix decides whether to recompile Rebus itself from its source and config
  # files, so it would not notice REBUS_UNIX_FDS changing and would reuse a
  # build made with the other setting. A build without fd support gets its
  # own directory instead.
  defp build_path do
    if own_build?() and no_unix_fds?(), do: [build_path: "_build/no_unix_fds"], else: []
  end

  # A dependency is built in :prod, so these apply only to Rebus's own builds.
  defp own_build?, do: Mix.env() in [:dev, :test]
  defp no_unix_fds?, do: System.get_env("REBUS_UNIX_FDS") == "0"

  defp make_env do
    %{"FINE_INCLUDE_DIR" => Fine.include_dir()}
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:crypto, :logger],
      mod: {Rebus.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:credo, "~> 1.7", only: [:dev, :test], runtime: false},
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:elixir_make, "~> 0.9", runtime: false},
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:fine, "~> 0.1", runtime: false},
      {:publisho, "~> 1.0", only: :dev, runtime: false},
      {:typedstruct, "~> 0.5.0", runtime: false}
    ]
  end

  defp aliases do
    [
      precommit: [
        "compile --warnings-as-errors",
        "deps.unlock --unused",
        "format",
        "credo --strict",
        "test"
      ],
      # The integration suite needs a real dbus-daemon, which macOS does not
      # have; this runs it in the container defined by docker/Dockerfile.
      "test.integration": ["cmd bin/integration-test"]
    ]
  end

  defp description do
    "An Elixir implementation of the D-Bus message protocol."
  end

  defp docs do
    [
      main: "readme",
      source_ref: @version,
      source_url: @source_url,
      extras: [
        "README.md",
        "CONTRIBUTING.md",
        "guides/services.md",
        "guides/unix_fds.md",
        "guides/authentication.md",
        "guides/match_rules.md",
        "CHANGELOG.md",
        "LICENSE.md"
      ],
      groups_for_extras: [Guides: ~r"guides/"],
      groups_for_modules: [
        Core: [
          Rebus,
          Rebus.Message,
          Rebus.MatchRule,
          Rebus.UnixFD
        ],
        Services: [
          Rebus.Service,
          Rebus.Object,
          Rebus.Bus,
          Rebus.Service.Call,
          Rebus.Interface,
          Rebus.Interface.Arg,
          Rebus.Interface.Method,
          Rebus.Interface.Signal,
          Rebus.Interface.Property,
          Rebus.Introspection,
          Rebus.Introspection.Node
        ],
        "Wire format": [
          Rebus.Encoder,
          Rebus.Decoder,
          Rebus.Signature,
          Rebus.BusAddress
        ],
        Errors: [
          Rebus.ProtocolLimitError,
          Rebus.ResourceLimitError
        ]
      ]
    ]
  end

  defp package do
    [
      name: "rebus",
      licenses: ["MIT"],
      # The NIF compiles on install when Unix fd support is enabled, so its
      # source and Makefile ship.
      files: ~w(lib c_src Makefile mix.exs .formatter.exs README.md CHANGELOG.md LICENSE.md),
      links: %{
        "GitHub" => @source_url
      },
      maintainers: ["Nick Gunn"]
    ]
  end
end
