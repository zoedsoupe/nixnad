
{ config, pkgs, options,... }:

{
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  #
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  programs.command-not-found.enable = true;
  programs.dconf.enable = true;
  programs.fish.enable = true;

  environment.systemPackages = with pkgs; [
    #terminal & tools
    neofetch procs nnn git
    wget unzip exa pciutils unrar
    cmatrix curl fd lazygit glow
    ncdu starship alacritty
    gitAndTools.gh

    #theme
    betterlockscreen

    #chat
    tdesktop discord

    #editor/ide
    vim emacs

    #dev
    gcc zlib cmake bzip2
    gnumake lua gdb
    python38Full 

    #rust dev
    rustup cargo-edit lldb rust-analyzer

    #haskell dev
    ghc stack haskellPackages.xmonad
    haskellPackages.xmobar

    #node dev
    nodejs yarn deno

    #erlang/elixir dev
    erlangR23 elixir 

    #audio & video
    mpd playerctl mpc_cli mpv
    pavucontrol ffmpeg spotify

    #editor/ide
    vim emacs

    #dev 
    insomnia google-cloud-sdk

    #printer
    simple-scan

    #others
    udiskie rofi zathura

    #xorg
    libnotify xclip xorg.xrandr
    xautolock picom xorg.xauth
    pam

    #tools
    gparted qbittorrent
    dunst flameshot docker
    docker-compose
  ];

  environment.sessionVariables.TERMINAL = [ "alacritty" ];
  environment.sessionVariables.EDITOR = [ "emacs" ];
}
