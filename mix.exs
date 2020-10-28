defmodule Boom.MixProject do
  use Mix.Project

  @source_url "https://github.com/wyeworks/boom"

  def project do
    [
      app: :boom,
      version: "0.1.0",
      elixir: "~> 1.7",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [plt_add_apps: [:eex]],
      aliases: [
        quality: ["format", "credo --strict", "dialyzer"]
      ],
      docs: docs()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Boom.Application, []}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:bamboo, "~> 1.1"},
      {:bypass, "~> 1.0", only: :test},
      {:credo, "~> 1.1", only: [:dev], runtime: false},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      {:ex_doc, "~> 0.23", only: :dev},
      {:httpoison, "~> 1.5"},
      {:jason, "~> 1.2"},
      {:phoenix, "~> 1.4", only: [:test]},
      {:plug_cowboy, "~> 1.0 or ~> 2.0"}
    ]
  end

  defp docs do
    [
      main: "readme",
      source_url: @source_url,
      extras: [
        "README.md"
      ]
    ]
  end
end
