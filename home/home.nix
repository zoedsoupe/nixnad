{ config, pkgs, ... }:

{
  imports = [
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.09.tar.gz}/nixos")
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

  useUserPackages = true;
  useGlobalPkgs = true;

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
    qutebrowser google-chrome

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
    bitwarden-cli dunst
  ];

  home.stateVersion = "21.03";
}
