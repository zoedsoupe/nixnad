defmodule Dotfiles.CLI do
  @moduledoc """
  CLI entrypoint
  """

  import Dotfiles.CLIHelpers

  alias Dotfiles.Installer

  def main(args) do
    args
    |> parse_args
    |> case do
      {:ok, parsed} ->
        parsed |> commands()

      _ ->
        default_error()
    end
  end

  defp commands(help: help), do: get_version() |> build_help()
  defp commands(link: link), do: Installer.link()

  defp parse_args(args) do
    opts = [
      strict: [help: :boolean],
      aliases: [h: :help]
    ]

    case OptionParser.parse(args, opts) do
      {[], _, []} ->
        no_opts()

        System.halt(1)

      {_, _, invalid} when invalid != [] ->
        invalid |> unknown_opts()

        System.halt(1)

      {parsed, _, invalid} ->
        unless Enum.empty?(invalid), do: unknown_opts(invalid)

        {:ok, parsed}
    end
  end
end
