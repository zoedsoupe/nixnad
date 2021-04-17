{ config, pkgs, ... }:

let
  dotfiles = "/home/matthew/documents/privy/dotfiles/dots";
in {
  imports = [
    ./vim.nix
    ./git.nix
    ./tmux.nix
    ./dunst.nix
    ./udiskie.nix
    ./starship.nix
    ./autojump.nix
    ./alacritty.nix
    ./polybar
    ./xmonad
  ];

  nixpkgs.config.allowUnfree = true;

  # extra programs
  programs.home-manager.enable = true;
  programs.alot.enable = true;
  programs.command-not-found.enable = true;

  # extra services
  services.blueman-applet.enable = true;

  home.username = "matthew";
  home.homeDirectory = "/home/matthew";

  home.packages = with pkgs; [
    # chat
    tdesktop discord

    # browser
    qutebrowser google-chrome

    # theme
    lxappearance
    
    # tools
    docker-compose insomnia
    qbittorrent obs-studio

    # audio
    spotify
    
    # images
    flameshot peek
  ];
  
  home.file = {
    ".iex.exs".source = "${dotfiles}/.iex.exs";
    ".doom.d".source = "${dotfiles}/.doom.d";
    ".sbclrc".source = "${dotfiles}/.sbclrc";
    ".patat.yaml".source = "${dotfiles}/.patat.yaml";
    ".xinitrc".source = "${dotfiles}/.xinitrc";
  };

  xdg.configFile.neofetch.source = "${dotfiles}/neofetch";
  
  accounts.email.accounts = {
    main = {
      address = "zoey.spessanha@zeetech.io"; 
      primary = true;
    };
    second = {
      address = "00119110328@pq.uenf.br";
    };
    business = {
      address = "mdsp@boosting.tech";
    };
  };
  
  home.stateVersion = "21.03";
}
