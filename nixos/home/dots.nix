{ config, pkgs, ... }:

{
  home-manager.users.matthew = {
    home.file = {
      ".doom.d".source = ./.doom.d;
      ".iex.exs".text = "IEx.configure(inspect: [limit: :infinity, pretty: true])";
      ".patat.yaml".text = ''
        wrap: true
        incrementalLists: true
        images:
          backend: auto
      '';
      ".xinitrc".text = ''
        #!/bin/sh

        set -e

        # BEGIN SYSTEM CONFIG
        if [[ -d /stc/X11/xinit/xinitrc.d ]]; then
          for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
            [ -x "$f" && sh "$f" ]
          done
        fi
        # END SYSTEM CONFIG

        # set second monitor as primary 
        if xrandr | grep -q 'HDMI-1 connected'; then
          xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --left-of eDP-1
        elif xrandr | grep -q 'DP-1 connected'; then
          xrandr --output eDP-1 --auto --output DP-1 --primary --auto --left-of eDP-1
        fi

        feh --bg-fill --randomize ~/pics/wallpapers &

        case "$WM" in
          xmonad) exec xmonad ;;
          *) echo "No WM specified" && false ;;
        esac
      '';
    };

    xdg.configFile.xmobar = {
      source = ./xmobar;
      recursive = true;
    };
  };
}
