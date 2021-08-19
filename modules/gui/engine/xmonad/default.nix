{ pkgs, ... }:

with import ../../../global-config.nix;

{
  services = {
    xserver = {
      windowManager.xmonad = {
        enable = true;
        config = ./config.hs;
        enableContribAndExtras = true;
      };

      displayManager = {
        autoLogin = {
          enable = true;
          user = "${username}";
        };

        defaultSession = "none+xmonad";
      };
    };

    xrdp.defaultWindowManager = "xmonad";
  };
}
