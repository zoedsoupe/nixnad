{ config, pkgs, ... }:

let
  dotfiles = "/home/matthew/documents/privy/dotfiles/dots";
in {
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

  home.packages = with pkgs; [
    # chat
    tdesktop discord

    # browser
    qutebrowser google-chrome

    # theme
    lxappearance
    
    # tools
    docker-compose insomnia
    qbittorrent

    # audio
    spotify
    
    # images
    flameshot peek
  ];
  
  home.file = {
    iex.source = "${dotfiles}/.iex.exs";
    sbclrc.source = "${dotfiles}/.scblrc";
    patat.source = "${dotfiles}/.patat.yaml";
    xinitrc.source = "${dotfiles}/.xinitrc";
  };

  xdg.configFile = {
    neofetch.source = "${dotfiles}/neofetch";
    scripts.source = "${dotfiles}/scripts";
  };
  
  accounts.email.accounts = {
    main = {
      address = "zoey.spessanha@zeetech.io"; 
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
