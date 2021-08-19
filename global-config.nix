rec {
  username = "matdsoupe";
  email = "matheus_pessanha2001@outlook.com";
  selected-desktop-environment = "stumpwm";
  rootPath = "/home/${username}/.dotfiles";
  rooPathNix = rootPath;
  flake = import ./lib/flake { };
}
