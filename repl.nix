with builtins;

rec {
  flake = getFlake (toString ./.);
  pkgs = import <nixpkgs> { };
  lib = pkgs.lib;
}
