{ config, pkgs, options,... }:

{
  nixpkgs.config.allowUnfree = true;

  programs.command-not-found.enable = true;
  programs.nm-applet.enable = true;

  environment.systemPackages = with pkgs; [
    # terminal & tools
    neofetch procs
    wget unzip exa pciutils 
    unrar psmisc bat cmatrix 
    iw curl ncdu lazygit 
    glow fd nnn jq tldr git
    alacritty acpi

    # theme
    betterlockscreen 

    # editor/ide
    vim

    # dev
    gcc zlib cmake bzip2 gnumake
    lua binutils.bintools gdb
    pkg-config google-cloud-sdk

    # haskell dev
    ghc stack haskellPackages.brittany 
    haskellPackages.hoogle

    # node dev
    nodejs-12_x nodePackages.yarn deno
    nodePackages.typescript
    nodePackages.create-react-app

    # elixir dev
    erlangR23 elixir

    # elm dev
    elmPackages.elm 
    elmPackages.elm-format
    elmPackages.elm-analyse 
    elmPackages.elm-test
    elmPackages.create-elm-app

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
  environment.sessionVariables.EDITOR = [ "vim" ];
}
