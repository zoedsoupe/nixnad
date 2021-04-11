
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
    ncdu starship

    #theme
    betterlockscreen

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

    #tools
    gparted qbittorrent
    dunst flameshot docker
    docker-compose
  ];

  environment.sessionVariables.TERMINAL = [ "kitty" ];
  environment.sessionVariables.EDITOR = [ "nvim" ];
}
