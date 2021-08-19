rec {
  username = "zoedsoupe";
  email = "zoey.spessanha@zeetech.io";
  selected-desktop-environment = "gnome";
  rootPath = "/home/${username}/documents/nixnad";
  rooPathNix = rootPath;
  flake = import ./lib/flake { };
}
