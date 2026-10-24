defmodule Rebus.MixProject do
  use Mix.Project

  @version "0.3.0"
  @source_url "https://github.com/ausimian/rebus"

  def project do
    [
      app: :rebus,
      version: version(),
      elixir: "~> 1.18",
      start_permanent: Mix.env() == :prod,
      compilers: compilers(),
      make_env: &make_env/0,
      make_precompiler: {:nif, CCPrecompiler},
      make_precompiler_url: "#{@source_url}/releases/download/#{version()}/@{artefact_filename}",
      make_precompiler_filename: "rebus_unix_fd",
      make_precompiler_nif_versions: [versions: ["2.17"]],
      make_force_build: force_build?(),
      cc_precompiler: [compilers: precompiled_targets()],
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

  defp version, do: System.get_env("VERSION_OVERRIDE", @version)

  def cli do
    [
      preferred_envs: [precommit: :test]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/lib"]
  defp elixirc_paths(_), do: ["lib"]

  # The Rebus.UnixFD NIF is installed, precompiled or built from source, only
  # on Linux and macOS, the platforms where Rebus passes descriptors, and only
  # when Unix fd support is enabled; otherwise Rebus.UnixFD returns
  # {:error, :unsupported}.
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

  # A Hex release carries checksum.exs for the precompiled libraries that CI
  # publishes with it. Without it, as in Rebus's own builds and git or path
  # dependencies, nothing can be verified, so the library builds from source.
  # A cross-compile, such as a Nerves build, always builds from source with
  # its own toolchain rather than installing a library built for a desktop
  # distribution.
  defp force_build? do
    not File.exists?(Path.join(__DIR__, "checksum.exs")) or System.get_env("CROSSCOMPILE") != nil
  end

  # The targets with precompiled libraries. Any other target builds from
  # source. Each Linux target names the compiler prefix of the distribution
  # that builds it, so a CI job building on one target never finds a
  # compiler for another; macOS builds both of its targets on one host.
  defp precompiled_targets do
    %{
      {:unix, :linux} => %{
        "x86_64-linux-gnu" => "x86_64-linux-gnu-",
        "aarch64-linux-gnu" => "aarch64-linux-gnu-",
        "x86_64-linux-musl" => "x86_64-alpine-linux-musl-",
        "aarch64-linux-musl" => "aarch64-alpine-linux-musl-"
      },
      {:unix, :darwin} => %{
        "x86_64-apple-darwin" =>
          {"gcc", "g++", "<%= cc %> -arch x86_64", "<%= cxx %> -arch x86_64"},
        "aarch64-apple-darwin" =>
          {"gcc", "g++", "<%= cc %> -arch arm64", "<%= cxx %> -arch arm64"}
      }
    }
  end

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
      {:cc_precompiler, "~> 0.1", runtime: false},
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
      # With Unix fd support enabled, an install downloads the precompiled
      # library that checksum.exs verifies, or builds it from source on any
      # other target, so the source and Makefile ship too.
      files:
        ~w(lib c_src Makefile mix.exs checksum.exs .formatter.exs README.md CHANGELOG.md LICENSE.md),
      links: %{
        "GitHub" => @source_url
      },
      maintainers: ["Nick Gunn"]
    ]
  end
end
