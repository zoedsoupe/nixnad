{ global, ... }:

let
  profiles = {
    "gnome" = ./engine/gnome;
    "xmonad" = ./engine/xmonad;
  };
  inherit (global) selected-desktop-environment;
in {
  imports = [
    (profiles."${selected-desktop-environment}")
  ];
}
