defmodule Dotfiles.CLI do
  @moduledoc """
  CLI entrypoint
  """

  import Dotfiles.CLIHelpers

  alias Dotfiles.Linker

  def main(args) do
    {:ok, parsed} =
      args
      |> parse_args

    parsed |> commands()
  end

  defp commands(help: true), do: get_version() |> build_help()
  defp commands(task: "link"), do: Linker.link()
  defp commands(task: "install"), do: nil

  defp parse_args(args) do
    opts = [
      strict: [help: :boolean, link: :boolean, task: :string],
      aliases: [h: :help, l: :link, t: :task]
    ]

    case OptionParser.parse(args, opts) do
      {[], [], []} ->
        no_opts()

        System.halt(1)

      {_, _, invalid} when invalid != [] ->
        invalid |> unknown_opts()

        System.halt(1)

      {parsed, _, invalid} when parsed != [] ->
        unless Enum.empty?(invalid), do: ignoring_opts(invalid)

        {:ok, parsed}
    end
  end
end
