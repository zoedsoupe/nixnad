defmodule Dotfiles.Messages do
  @moduledoc """
  Define some cute colors for messages!
  """

  import IO.ANSI, only: [format: 2]

  def success(task) do
    msg =
      [:green, :bright, "DONE: ", :green, task]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def error(reason) do
    msg =
      [:red, :bright, "ERROR: ", :red, reason]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def warn(info) do
    msg =
      [:yellow, :bright, "\nWARNING: ", :yellow, info]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def info(desc) do
    msg =
      [:blue, :bright, "INFO: ", :blue, desc]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end
end
