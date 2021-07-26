{ config, pkgs, options,... }:

{
  nixpkgs.config.allowUnfree = true;

  programs = {
    steam.enable = true;
    command-not-found.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };
  };

  environment.systemPackages = with pkgs; [
    # terminal & tools
    neofetch procs
    wget unzip exa pciutils 
    unrar psmisc bat cmatrix 
    iw curl ncdu lazygit 
    glow fd nnn jq tldr git
    alacritty acpi ag

    # theme
    betterlockscreen 

    # editor/ide
    vim

    # browser
    google-chrome

    # dev
    google-cloud-sdk

    # tools
    gparted 

    # audio & video
    mpv pavucontrol ffmpeg

    # image
    feh 

    # others
    zathura inotify-tools

    # xorg
    xorg.xrandr xclip

    # games
    steamcmd

    # gnome stuff
    gnome.gnome-tweaks
    gnomeExtensions.emoji-selector
    gnomeExtensions.clipboard-indicator
    numix-icon-theme-circle
    numix-cursor-theme

  ];

  environment.sessionVariables.TERMINAL = [ "alacritty" ];
  environment.sessionVariables.EDITOR = [ "vim" ];
}
