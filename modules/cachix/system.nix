{ pkgs, config, lib, ... }:

with lib;

let
  path-if-exists = import ../../lib/path-list-if-exist.nix;
in
{
  imports = path-if-exists /etc/nixos/cachix.nix;
  options.cachix.enable = mkEnableOption "enable cachix";
  config = mkIf config.cachix.enable {
    environment.systemPackages = with pkgs; [
      cachix
    ];
  };
}
