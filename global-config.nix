rec {
  username = "zoedsoupe";
  email = "zoey.spessanha@zeetech.io";
  selected-desktop-environment = "stumpwm";
  rootPath = "/home/${username}/.dotfiles";
  rooPathNix = rootPath;
  flake = import ./lib/flake { };
}
