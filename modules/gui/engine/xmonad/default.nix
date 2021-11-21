{ global, pkgs, ... }:

{
  services = {
    xserver = {
      windowManager.xmonad = {
        enable = true;
        config = ./config.hs;
        enableContribAndExtras = true;
      };

      displayManager.gdm = {
        enable = true;
        wayland = false;
      };
    };

    xrdp.defaultWindowManager = "xmonad";
  };
}
