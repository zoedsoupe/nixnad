{ config, lib, pkgs, ... }:

{
  programs.starship = {
    enable = true;
    enableFishIntegration = true;
    settings = {
      scan_timeout = 5;
      directory = {
        format = "[$path]($style)[$read_only]($read_only_style) ";
      };
      git_branch = {
        format = "[git:](white)\\([$branch]($style)\\)";
      };
      git_status = {
        deleted = "";
        ahead = "";
        behind = "";
        format = "[$all_status$ahead_behind]($style) ";
      };
      elixir = {
        format = "[elixir:](white)\\([$version]($style)\\) ";
      };
      nodejs = {
        format = "[node:](white)\\([$version]($style)\\) ";
      };
      rust = {
        format = "[rust:](white)\\([$version]($style)\\) ";
      };
      haskell = {
        format = "[haskell:](white)\\([$version]($style)\\) ";
      };
      elm = {
        format = "[elm:](white)\\([$version]($style)\\) ";
      };
      character = {
        success_symbol = "\n[ﬦ](bold green)";
        error_symbol = "\n[](bold red)";
        vicmd_symbol = "\n[](bold green)";
      };
      format = lib.concatStrings [
        "$git_branch"
        "$git_status"
        "$elixir"
        "$elm"
        "$nodejs"
        "$haskell"
        "$rust"
        "$character>"
      ];
    };
  };
}
