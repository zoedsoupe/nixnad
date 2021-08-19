with import ./global-config.nix;

self: super:
let
  discord-url = https://discord.com/api/download?platform=linux&format=tar.gz;
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
    discord = super.discord.overrideAttrs (
      _: {
        src = builtins.fetchTarball discord-url;
      }
    );
  }
]
