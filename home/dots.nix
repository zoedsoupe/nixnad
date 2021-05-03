{ config, pkgs, ... }:

{
  home.file = {
    ".iex.exs".text = "IEx.configure(inspect: [limit: :infinity, pretty: true])";

    ".patat.yaml".text = ''
      wrap: true
      incrementalLists: true
      images:
        backend: auto
    '';

    ".doom.d".source = ./doom.d;
  };

  xdg.configFile.xmobar = {
    source = ./xmobar;
    recursive = true;
  };
}
