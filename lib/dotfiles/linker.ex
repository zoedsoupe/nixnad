defmodule Dotfiles.Installer do
  @moduledoc """
  Link dotfiles!
  """

  import Dotfiles.{Colors, Helpers}

  def link do
    "I'll link all these configs for you! Let's go:"
    |> info()

    {config_folder, home_path, dots_path} = paths()
    actual_path = Path.expand(".")

    check_config_path(config_folder)

    for config <- Path.wildcard(dots_path <> "/*", match_dot: true) do
      config = config |> String.replace("dots/", "")

      config_path = actual_path <> "/dots/#{config}"

      cond do
        config =~ ~r(^\..+) ->
          (home_path <> "/#{config}")
          |> link_if_exist(config, config_path, &link_to_home/2)

        config == "scripts" ->
          (home_path <> "/#{config}")
          |> link_if_exist(config, config_path, &link_to_home/2)

        config == "xsessions" ->
          commands =
            "ln -s #{actual_path}/#{config} /usr/share/#{config}"
            |> String.split(" ")

          "/usr/share/#{config}"
          |> link_if_exist(config, &root_config/1, commands: commands)

        config == "udev_rules" ->
          link_udev_rules(config, config_path)

        true ->
          (config_folder <> "/#{config}")
          |> link_if_exist(config, config_path, &link_to_dot_config/2)
      end
    end

    IO.puts("")

    "Linked all! Congratutalions!"
    |> success()

    "Please, remember to change ~/.gitconfig file"
    |> warn()
  end

  defp check_config_path(config_path) do
    case File.stat(config_path) do
      {:ok, _stat} ->
        "Good, .config folder exists!\n"
        |> info()

      _ ->
        "Well, .config folder doesn't exists... I'll create to you!"
        |> warn()

        File.mkdir!(config_path)

        "Created .config foler on home dir!\n"
        |> success()
    end
  end

  defp link_udev_rules(config, config_path) do
    slash_position = 11

    Path.wildcard("./#{config}/*")
    |> Enum.map(fn rule_path -> rule_path |> String.split_at(slash_position) |> elem(1) end)
    |> Enum.each(fn rule ->
      commands =
        "ln -s #{config_path}/#{rule} /etc/udev/rules.d/#{rule}"
        |> String.split(" ")

      "/etc/udev/rules.d/#{rule}"
      |> link_if_exist(config, &root_config/1, commands: commands)
    end)
  end

  defp paths do
    paths = Application.get_env(:dotfiles, :paths)

    {paths[:config_path], paths[:home_path], paths[:dots_path]}
  end
end
