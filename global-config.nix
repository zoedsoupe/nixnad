rec {
  username = "zoedsoupe";
  email = "zoey.spessanha@zeetech.io";
  selected-desktop-environment = "gnome";
  rootPath = "/home/${username}/.dotfiles";
  rooPathNix = rootPath;
  flake = import ./lib/flake { };
}
