rec {
  username = "zoedsoupe";
  email = "zoey.spessanha@zeetech.io";
  selected-desktop-environment = "gnome";
  rootPath = builtins.toString ./.; 
  rooPathNix = rootPath;
  flake = import ./lib/flake { };
}
