defmodule Boom.MixProject do
  use Mix.Project

  def project do
    [
      app: :boom,
      version: "0.1.0",
      elixir: "~> 1.7",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps()
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:bamboo, "~> 1.1"},
      {:bypass, "~> 1.0", only: :test},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      {:httpoison, "~> 1.5"},
      {:phoenix, "~> 1.4", only: [:test]},
      {:plug_cowboy, "~> 1.0"}
    ]
  end
end
