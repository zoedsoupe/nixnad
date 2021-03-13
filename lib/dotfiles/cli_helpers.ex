defmodule Dotfiles.CLIHelpers do
  @moduledoc """
  Helpers to better organize CLI
  """

  import Dotfiles.Colors

  alias IO.ANSI

  def unknown_opts(invalid) do
    invalid =
      invalid
      |> Enum.map(fn {op, _} -> op end)

    "I really don't know what to do with these options:\n#{inspect(invalid)}"
    |> warn()
  end

  def no_opts do
    "Well, You didn't say me nothing so, what I need to do?" |> error()
  end

  def default_error(_), do: default_error()

  def default_error do
    ""
    |> error()
  end

  def get_version do
    {:ok, vsn} = :application.get_key(:dotfiles, :vsn)

    ~s(#{green("dotfiles")} v#{vsn}\n)
    |> IO.puts()

    {:ok, vsn}
  end

  def build_help(_) do
    yellow("USAGE:") |> IO.puts()

    IO.puts("    dotfiles [options]\n")

    yellow("OPTIONS") |> IO.puts()
    green("    -h, --help") |> IO.puts()
    IO.puts("            Shows this help section")

    {:ok, "helped (:"}
  end

  defp green(text) do
    ANSI.green() <> text <> ANSI.reset()
  end

  defp yellow(text) do
    ANSI.yellow() <> text <> ANSI.reset()
  end
end
