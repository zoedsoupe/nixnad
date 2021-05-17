{ config, pkgs, ... }:

{
  imports = [
    ./vim.nix
    ./git.nix
    ./dots.nix
    ./doom.nix
    ./starship.nix
    ./autojump.nix
    ./alacritty.nix
  ];

  nixpkgs.config.allowUnfree = true;

  programs = {
    home-manager.enable = true;
    command-not-found.enable = true;
  };

  home.username = "matthew";
  home.homeDirectory = "/home/matthew";

  home.packages = with pkgs; [
    # chat
    tdesktop discord slack-dark

    # theme
    lxappearance
  
    # tools
    docker-compose insomnia
    qbittorrent obs-studio
    screenkey gitAndTools.gh

    # audio
    spotify
  
    # images
    peek flameshot imagemagick

    # others
    any-nix-shell
    bitwarden-cli
  ];

  home.stateVersion = "21.03";
}
