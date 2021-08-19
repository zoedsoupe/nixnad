{ pkgs, ... }:

{
  services = {
    gnome.gnome-keyring.enable = true;

    xserver = {
      desktopManager.gnome.enable = true;

      displayManager.gdm = {
        enable = true;
        wayland = false;
      };
    };
  };
}
