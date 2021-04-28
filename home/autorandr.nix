{ pkgs, ... }:

{
  programs.autorandr = {
    enable = true;
    hooks.postswitch = {
      "change-bg" = ''
      #!/bin/sh
      feh --bg-fill --randomize ~/pics/wallpapers &
      '';
    };
    profiles = {
      "boosting" = {
        config = {
          "eDP-1" = {
            enable = true;
            mode = "1366x768";
            position = "1280x128";
            rotate = "normal";
          };
          "DP-1" = {
            enable = true;
            primary = true;
            mode = "1280x1024";
            position = "0x0";
            rotate = "normal";
          };
        };
      };
    };
  };
}
