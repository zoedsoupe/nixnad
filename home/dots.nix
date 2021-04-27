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

    ".xsession".text = ''
      #!/bin/sh

      set -e

      # set second monitor as primary 
      if xrandr | grep -q 'HDMI-1 connected'; then
        xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --left-of eDP-1
      elif xrandr | grep -q 'DP-1 connected'; then
        xrandr --output eDP-1 --auto --output DP-1 --primary --auto --left-of eDP-1
      fi

      feh --bg-fill --randomize ~/pics/wallpapers &
    '';
  };

  xdg.configFile.xmobar = {
    source = ./xmobar;
    recursive = true;
  };
}
