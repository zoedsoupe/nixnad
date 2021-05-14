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

    # gnome stuff
    gnome3.gnome-tweaks
    gnomeExtensions.emoji-selector
    gnomeExtensions.clipboard-indicator
    numix-icon-theme-circle
    numix-cursor-theme

  ];

  environment.sessionVariables.TERMINAL = [ "alacritty" ];
  environment.sessionVariables.EDITOR = [ "vim" ];
}
