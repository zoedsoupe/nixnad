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

    ".ghci".text = ''
      :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
    '';
  };

  xdg.configFile.xmobar = {
    source = ./xmobar;
    recursive = true;
  };
}
