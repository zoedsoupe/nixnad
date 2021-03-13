defmodule Dotfiles.CLIHelpers do
  @moduledoc """
  Helpers to better organize CLI
  """

  import Dotfiles.Colors

  def unknown_opts(invalid) do
    invalid = keyword_keys(invalid)

    "I really don't know what to do with these options:\n#{inspect(invalid)}"
    |> warn()
  end

  def no_opts do
    "Well, You didn't say me nothing so, what I need to do?" |> error()
  end

  def ignoring_opts(invalid) do
    invalid = keyword_keys(invalid)

    "Ignoring these options as I don't recognize them:\n#{inspect(invalid)}"
    |> warn()
  end

  def default_error(_), do: default_error()

  def default_error do
    "Something got wrong..."
    |> error()

    System.halt(1)
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

  defp keyword_keys(kw) do
    kw
    |> Enum.map(fn {op, _} -> op end)
  end
end
