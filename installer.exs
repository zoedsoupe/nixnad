# _ __ ___   __| |___ _ __
# | '_ ` _ \ / _` / __| '_ \
# | | | | | | (_| \__ \ |_) |
# |_| |_| |_|\__,_|___/ .__/
#                    |_|

defmodule Helpers do
  @config_path Path.expand("~/.config")
  @home_dir Path.expand("~")

  def skip(config), do: "Skipping #{config}..." |> Colors.info()

  def link_to_home(existing, config), do: File.ln_s!(existing, @home_dir <> "/#{config}")

  def link_to_dot_config(existing, config),
    do: File.ln_s!(existing, @config_path <> "/#{config}")

  def root_config(commands) do
    "Please, insert your sudo pass:"
    |> Colors.warn()

    System.cmd("sudo", ["-A"] ++ commands, into: password_get())
  end

  def link_if_exist(test_path, config, config_path, link_fun) when is_binary(config_path) do
    if File.exists?(test_path) do
      skip(config)
    else
      link_fun.(config_path, config)

      "Linked #{config}!\n"
      |> Colors.success()
    end
  end

  def link_if_exist(test_path, config, link_fun, commands: commands) do
    if File.exists?(test_path) do
      skip(config)
    else
      link_fun.(commands)

      "Linked #{config}!\n"
      |> Colors.success()
    end
  end

  defp password_get() do
    pid = spawn_link(fn -> loop() end)
    ref = make_ref()
    value = IO.read(:stdio, :line)

    send(pid, {:done, self(), ref})
    receive do: ({:done, ^pid, ^ref} -> :ok)

    value
  end

  defp loop() do
    receive do
      {:done, parent, ref} ->
        send(parent, {:done, self(), ref})
        IO.write(:standard_error, "\e[2K\r")
    after
      1 ->
        IO.write(:standard_error, "\e[2K\r ")
        loop()
    end
  end
end

defmodule Colors do
  import IO.ANSI, only: [format: 2]

  def success(task) do
    [:green, :bright, "DONE: ", :green, task]
    |> format(true)
    |> IO.puts()
  end

  def error(reason) do
    [:red, :bright, "ERROR: ", :red, reason]
    |> format(true)
    |> IO.puts()
  end

  def warn(info) do
    [:yellow, :bright, "\nWARNING: ", :yellow, info]
    |> format(true)
    |> IO.puts()
  end

  def info(desc) do
    [:blue, :bright, "INFO: ", :blue, desc]
    |> format(true)
    |> IO.puts()
  end
end

defmodule Installer do
  @moduledoc """
  Run this Elixir script to automatically link all these configs!
  """

  import Colors
  import Helpers

  @config_path Path.expand("~/.config")
  @home_dir Path.expand("~")

  def link do
    "I'll link all these configs for you! Let's go:"
    |> info()

    actual_path = Path.expand(".")

    case File.stat(@config_path) do
      {:ok, _stat} ->
        "Good, .config folder exists!\n"
        |> info()

      _ ->
        "Well, .config folder doesn't exists... I'll create to you!"
        |> warn()

        File.mkdir!(@config_path)

        "Created .config foler on home dir!\n"
        |> success()
    end

    for config <- Path.wildcard("./dots/*", match_dot: true) do
      config = config |> String.replace("dots/", "")

      config_path = actual_path <> "/dots/#{config}"

      cond do
        config =~ ~r(^\..+) ->
          (@home_dir <> "/#{config}")
          |> link_if_exist(config, config_path, &link_to_home/2)

        config == "scripts" ->
          (@home_dir <> "/#{config}")
          |> link_if_exist(config, config_path, &link_to_home/2)

        config == "xsessions" ->
          commands =
            "ln -s #{actual_path}/#{config} /usr/share/#{config}"
            |> String.split(" ")

          "/usr/share/#{config}"
          |> link_if_exist(config, &root_config/1, commands: commands)

        config == "udev_rules" ->
          slah_position = 11

          Path.wildcard("./#{config}/*")
          |> Enum.map(fn rule_path -> rule_path |> String.split_at(slah_position) |> elem(1) end)
          |> Enum.each(fn rule ->
            commands =
              "ln -s #{config_path}/#{rule} /etc/udev/rules.d/#{rule}"
              |> String.split(" ")

            "/etc/udev/rules.d/#{rule}"
            |> link_if_exist(config, &root_config/1, commands: commands)
          end)

        true ->
          (@config_path <> "/#{config}")
          |> link_if_exist(config, config_path, &link_to_dot_config/2)
      end
    end

    IO.puts("")

    "Linked all! Congratutalions!"
    |> success()

    "Please, remember to change ~/.gitconfig file"
    |> warn()
  end
end

Installer.link()
