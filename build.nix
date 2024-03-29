let
  inherit (builtins) getFlake;
  flake = getFlake "${toString ./.}";
  inherit (flake.outputs) pkgs;
in builtins.attrValues {
  inherit (pkgs) discord neovim;
  inherit (flake.outputs.nixosConfigurations)
    acer-nix
    bootstrap
  ;
  inherit (flake.outputs.homeConfigurations) main;
}
