flake: self: super:
let
  inherit (flake) inputs;
  inherit (flake.outputs.extra-args) global;
  inherit (global) rootPath system;
  inherit (super) lib callPackage writeShellScript;
  inherit (lib) recursiveUpdate;
  inherit (builtins) toString length head tail;
  inherit (flake.inputs) nixpkgs-latest nixpkgs-master nixpkgs neomat;
in
let
  cp = f: (callPackage f) {};
  reduce-join = items:
    if (length items) > 0 then
      (recursiveUpdate (head items) (reduce-join (tail items)))
    else
    {};
in
reduce-join [
  super
  rec {
    lib = {
      inherit reduce-join;
      maintainers = import "${nixpkgs-latest}/maintainers/maintainer-list.nix";
    };
    latest = import nixpkgs-latest { };
    beekeeper-studio = cp "${nixpkgs-master}/pkgs/development/tools/database/beekeeper-studio/default.nix";
    discord = cp "${nixpkgs-master}/pkgs/applications/networking/instant-messengers/discord/default.nix";
    onlyoffice-bin = cp "${nixpkgs-master}/pkgs/applications/office/onlyoffice-bin/default.nix";
    neovim = import neomat.overlay."${system}".neovimMT { };
  }
]
