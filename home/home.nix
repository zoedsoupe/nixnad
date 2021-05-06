{ config, pkgs, ... }:

{
  imports = [
    ./vim.nix
    ./git.nix
    ./dots.nix
    ./dunst.nix
    ./udiskie.nix
    ./starship.nix
    ./autojump.nix
    ./autorandr.nix
    ./alacritty.nix
  ];

  nixpkgs.config.allowUnfree = true;

  programs = {
    home-manager.enable = true;
    gpg.enable = true;
    command-not-found.enable = true;
    emacs.enable = true;
  };

  services.clipmenu.enable = true;
  services.emacs.enable = true;

  home.username = "matthew";
  home.homeDirectory = "/home/matthew";

  home.packages = with pkgs; [
    # bar
    haskellPackages.xmobar

    # chat
    tdesktop discord

    # browser
    google-chrome

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
    any-nix-shell arandr
    bitwarden-cli
  ];

  home.stateVersion = "21.03";
}