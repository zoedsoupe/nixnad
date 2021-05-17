{ pkgs, ...}:

{
  services = {
    gnome3.gnome-keyring.enable = true;

    xserver = {
      desktopManager.gnome3.enable = true;

      displayManager.gdm = {
        enable = true;
        wayland = true;
      };
    };
  };
}
