defmodule Dotfiles.Colors do
  @moduledoc """
  Define some cute colors for messages!
  """

  import IO.ANSI, only: [format: 2, green: 0, yellow: 0, reset: 0]

  @info "ℹ"
  @success "✅"
  @failure "❌"
  @warning "⚠"

  def success(task) do
    msg =
      [:green, :bright, "#{@success} DONE: ", :green, task]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def error(reason) do
    msg =
      [:red, :bright, "#{@failure} ERROR: ", :red, reason]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def warn(info) do
    msg =
      [:yellow, :bright, "#{@warning} WARNING: ", :yellow, info]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def info(desc) do
    msg =
      [:blue, :bright, "#{@info} INFO: ", :blue, desc]
      |> format(true)

    msg
    |> IO.puts()

    {:ok, msg}
  end

  def green(text) do
    green() <> text <> reset()
  end

  def yellow(text) do
    yellow() <> text <> reset()
  end
end
