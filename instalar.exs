defmodule Installer do
  @moduledoc """
  Run this Elixir script to automatically link all these configs!
  """

  def link do
    IO.puts("I'll link all these configs for you! Let's go:")
    actual_path = Path.expand(".")

    case File.stat("~/.config") do
      {:ok, _stat} ->
        IO.puts("Good, .config folder exists!")

      _ ->
        IO.puts("Well, .config folder doesn't exists... I'll create to you!")
        File.mkdir!("~/.config")
        IO.puts("Created .config foler on home dir!")
    end

    for config <- Path.wildcard("./*", match_dot: true) do
      existing_path = actual_path <> "/#{config}}"

      cond do
        config =~ ~r(^\..+) ->
          existing_path
          |> link_to_home(config)

          IO.puts("Linked #{config}!")

        config == "scripts" ->
          existing_path
          |> link_to_home(config)

          IO.puts("Linked #{config}!")

        config == "xsessions" ->
          "ln -s #{actual_path}/#{config} /usr/share/#{config}"
          |> String.split(" ")
          |> root_config()

          IO.puts("Linked #{config}!")

        config =~ ~r(exs$) || config =~ ~r(sh$) ->
          IO.puts("Skipping #{config}...")

        true ->
          existing_path
          |> link_to_dot_config(config)

          IO.puts("Linked #{config}!")
      end
    end

    IO.puts("Linked all! Congratutalions!")
    IO.puts("Please, remember to change ~/.gitconfig file")
  end

  def link_to_home(existing, config), do: File.ln_s!(existing, Path.expand("~/#{config}"))

  def link_to_dot_config(existing, config),
    do: File.ln_s!(existing, Path.expand("~/.config/#{config}"))

  def root_config(command) do
    System.cmd("sudo", [command],
      env: [{"SUDO_ASKPASS", "./askpass.sh"}],
      into: IO.stream(:stdio, :line)
    )
  end
end

Installer.link()
