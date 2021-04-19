{ config, pkgs, options,... }:

{
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.packageOverrides = pkgs: {
    nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
      inherit pkgs;
    };
  };

  programs.command-not-found.enable = true;
  programs.nm-applet.enable = true;

  environment.systemPackages = with pkgs; [
    # terminal & tools
    neofetch procs
    wget unzip exa pciutils 
    unrar psmisc bat cmatrix 
    iw curl neofetch ncdu 
    lazygit glow fd 
    nnn jq tldr

    # theme
    betterlockscreen 

    # editor/ide
    vim emacs

    # dev
    gcc zlib cmake bzip2 gnumake
    lua binutils.bintools gdb
    pkg-config google-cloud-sdk

    # haskell dev
    ghc stack haskellPackages.brittany 
    haskellPackages.hoogle

    # node dev
    nodejs nodePackages.yarn deno
    nodePackages.typescript
    nodePackages.create-react-app

    # elixir dev
    erlangR23 elixir

    # tools
    gparted 
    dmenu

    # audio & video
    mpv pavucontrol ffmpeg

    # image
    feh 

    # others
    zathura inotify-tools

    # xorg
    xorg.xrandr xclip

  ];

  environment.sessionVariables.TERMINAL = [ "alacritty" ];
  environment.sessionVariables.EDITOR = [ "emacs" ];
}
