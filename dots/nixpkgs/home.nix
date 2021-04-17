{ config, pkgs, ... }:

{
  imports = [
    ./vim.nix
    ./git.nix
    ./dunst.nix
    ./starship.nix
    ./autojump.nix
    ./alacritty.nix
  ];
  
  programs.home-manager.enable = true;

  home.username = "matthew";
  home.homeDirectory = "/home/matthew";
  
  home.stateVersion = "21.03";
}
