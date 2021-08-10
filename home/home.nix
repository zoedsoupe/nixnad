{ config, pkgs, ... }:

let
  discordUrl = https://discord.com/api/download?platform=linux&format=tar.gz;
in {
  imports = [
    ./neovim
    ./git.nix
    ./dots.nix
    #./doom.nix
    ./fish.nix
    ./starship.nix
    ./alacritty.nix
  ];

  nixpkgs.config.allowUnfree = true;

  # sometimes discord wants the latest version...
  nixpkgs.overlays = [
    (self: super: {
      discord = super.discord.overrideAttrs (
          _: {
            src = builtins.fetchTarball discordUrl;
          }
        );
    })
  ];

  programs = {
    home-manager.enable = true;
    command-not-found.enable = true;
    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };
    fzf = {
      enable = true;
      enableFishIntegration = true;
    };
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
    qbittorrent gitAndTools.gh
    exodus earthly awscli2
    ngrok flyctl gcolor3
    t-rec

    # audio
    spotify
  
    # images
    peek flameshot imagemagick

    # others
    any-nix-shell
    bitwarden-cli
    texlive.combined.scheme-full
  ];

  home.stateVersion = "21.03";
}
