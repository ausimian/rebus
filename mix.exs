defmodule Rebus.MixProject do
  use Mix.Project

  @source_url "https://github.com/ausimian/rebus"

  def project do
    [
      app: :rebus,
      version: "0.2.0",
      elixir: "~> 1.17",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      package: package(),
      source_url: @source_url,
      elixirc_paths: elixirc_paths(Mix.env()),
      test_coverage: [ignore_modules: [Rebus.TestServer]]
    ]
  end

  # Specifies which paths to compile per environment.
  defp elixirc_paths(:test), do: ["lib", "test/lib"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Rebus.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.4", only: [:dev, :test], runtime: false},
      {:ex_doc, "~> 0.31", only: :dev, runtime: false},
      {:typedstruct, "~> 0.5.0", runtime: false}
    ]
  end

  defp description do
    "An Elixir implementation of the D-Bus message protocol."
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
