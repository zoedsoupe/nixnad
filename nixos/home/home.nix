{ config, pkgs, ... }:

{
  imports = [
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.09.tar.gz}/nixos")
    ./vim.nix
    ./git.nix
    ./dots.nix
    ./tmux.nix
    ./dunst.nix
    ./udiskie.nix
    ./starship.nix
    ./autojump.nix
    ./alacritty.nix
    ./xmonad
  ];

  nixpkgs.config.allowUnfree = true;

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  home-manager.users.matthew = {
    programs = {
      home-manager.enable = true;
      gpg.enable = true;
      command-not-found.enable = true;
    };

    services.flameshot.enable = true;

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
      screenkey

      # audio
      spotify
    
      # images
      peek

      # others
      any-nix-shell arandr
      bitwarden-cli
    ];
  
    home.stateVersion = "21.03";
  };
}
