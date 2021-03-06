defmodule Dotfiles.MixProject do
  use Mix.Project

  def project do
    [
      app: :dotfiles,
      name: "Matheus's dotfiles",
      version: "2.0.0",
      elixir: "~> 1.11",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: description(),
      source_url: "https://github.com/Mdsp9070/dotfiles"
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger],
      mod: {Dotfiles.Application, []}
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:dialyxir, "~> 1.0", only: [:dev], runtime: false},
      {:credo, "~> 1.5", only: [:dev, :test], runtime: false}
    ]
  end

  defp description do
    ~s"""
    Config files to set up a Web development/Hacking workstation, focused on functional progamming!
    Also includes system configs to improve your OS usability and of course tons of cool softwares!
    """
  end
end
