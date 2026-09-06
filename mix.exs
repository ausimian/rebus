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
    ]
  end

  def cli do
    [
      preferred_envs: [precommit: :test]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/lib"]
  defp elixirc_paths(_), do: ["lib"]

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
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
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
      links: %{
        "GitHub" => @source_url
      },
      maintainers: ["Nick Gunn"]
    ]
  end
end
