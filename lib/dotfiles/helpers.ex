defmodule Dotfiles.Helpers do
  @moduledoc """
  Some helpers functions
  """

  @config_folder Application.compile_env(:dotfiles, :paths)[:config_path]
  @home_path Application.compile_env(:dotfiles, :paths)[:home_path]

  import Dotfiles.Colors

  def skip(config), do: "Skipping #{config}..." |> info()

  def link_to_home(existing, config), do: File.ln_s!(existing, @home_path <> "/#{config}")

  def link_to_dot_config(existing, config),
    do: File.ln_s!(existing, @config_folder <> "/#{config}")

  def root_config(commands) do
    "Please, insert your sudo pass:"
    |> warn()

    System.cmd("sudo", ["-A"] ++ commands, into: password_get())
  end

  def link_if_exist(test_path, config, config_path, link_fun) when is_binary(config_path) do
    if File.exists?(test_path) do
      skip(config)
    else
      link_fun.(config_path, config)

      "Linked #{config}!\n"
      |> success()
    end
  end

  def link_if_exist(test_path, config, link_fun, commands: commands) do
    if File.exists?(test_path) do
      skip(config)
    else
      link_fun.(commands)

      "Linked #{config}!\n"
      |> success()
    end
  end

  defp password_get do
    pid = spawn_link(fn -> loop() end)
    ref = make_ref()
    value = IO.read(:stdio, :line)

    send(pid, {:done, self(), ref})
    receive do: ({:done, ^pid, ^ref} -> :ok)

    value
  end

  defp loop do
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
