with import ./global-config.nix;

self: super:
let
  recursive-update = super.lib.recursiveUpdate;
  cp = p: (super.callPackage p) { };
  reduce-join = items:
    if (builtins.length items) > 0 then
      (recursive-update (builtins.head items) (reduce-join (builtins.tail items)))
    else
      { };
in
reduce-join [
  super
  rec {
    lib = {
      inherit reduce-join;
      maintainers = import "${flakes.inputs.nixpkgs-latest}/maintainers/maintainer-list.nix";
    };
    latest = import flakes.inputs.nixpkgs-latest { };
    discord = cp "${flake.inputs.nixpkgs-master}/pkgs/applications/networking/instant-messengers/discord/default.nix";
    onlyoffice-bin = cp "${flake.inputs.nixpkgs-master}/pkgs/applications/office/onlyoffice-bin/default.nix";
  }
]
