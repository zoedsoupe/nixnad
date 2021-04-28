{ config, pkgs, ... }:

{
  imports = [
    ./vim.nix
    ./git.nix
    ./dots.nix
    ./doom.nix
    ./dunst.nix
    ./udiskie.nix
    ./starship.nix
    ./autojump.nix
    ./alacritty.nix
  ];

  nixpkgs.config.allowUnfree = true;

  programs = {
    home-manager.enable = true;
    gpg.enable = true;
    command-not-found.enable = true;
  };

  services.clipmenu.enable = true;

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
    peek flameshot

    # others
    any-nix-shell arandr
    bitwarden-cli
  ];

  home.stateVersion = "21.03";
}
