defmodule BoomNotifier.MixProject do
  use Mix.Project

  @source_url "https://github.com/wyeworks/boom"

  def project do
    [
      app: :boom_notifier,
      version: "0.4.0",
      elixir: "~> 1.7",
      elixirc_paths: elixirc_paths(Mix.env()),
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      dialyzer: [
        plt_add_apps: [:eex],
        plt_core_path: "priv/plts",
        plt_file: {:no_warn, "priv/plts/dialyzer.plt"}
      ],
      aliases: [
        quality: ["format", "credo --strict", "dialyzer"]
      ],
      docs: docs(),
      description: description(),
      package: package()
    ]
  end

  defp description do
    """
    This package allows your Phoenix application to send notifications
    whenever an exceptions is raised. By default it includes an email and a
    webhook notifier, but you can implement your custom ones.
    """
  end

  defp package do
    [
      files: ["lib", "mix.exs", "README.md"],
      licenses: ["MIT"],
      links: %{
        "GitHub" => @source_url,
        "Docs" => "https://hexdocs.pm/boom_notifier"
      }
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {BoomNotifier.Application, []}
    ]
  end

  defp elixirc_paths(:test), do: ["lib", "test/support"]
  defp elixirc_paths(_), do: ["lib"]

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:bamboo, "~> 2.0"},
      {:bypass, "~> 2.1", only: :test},
      {:credo, "~> 1.1", only: [:dev], runtime: false},
      {:dialyxir, "~> 1.1", only: [:dev], runtime: false},
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
