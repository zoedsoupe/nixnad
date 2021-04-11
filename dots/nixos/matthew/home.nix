{ pkgs, ... }:
let
  dotfiles = "/home/matthew/documents/privy/dotfiles/dots";
in
{
  imports = [
    (import "${builtins.fetchTarball https://github.com/rycee/home-manager/archive/release-20.09.tar.gz}/nixos")
  ];

  home-manager.useUserPackages = true;
  home-manager.useGlobalPkgs = true;

  home-manager.users.matthew = {

    programs = {
      home-manager.enable = true;
      git = {
        enable = true;
        userName  = "Mdsp9070";
        userEmail = "matheus_pessanha2001@outlook.com";
      };
    };

    xsession.enable = true;
    xsession.windowManager.xmonad.enable = true; 

    xdg = {
      enable = true;
      userDirs.enable = true;
    };

    gtk = {
      enable = true;
      font = {
        package = pkgs.cantarell-fonts;
        name = "Cantarell 11";
      };

      theme = {
        name = "vimix-dark-laptop-beryl";
      };

      iconTheme = {
        package = pkgs.papirus-icon-theme;
        name = "Papirus-Dark";
      };
    };


    home.sessionVariables = {
      TERMINAL = "alacritty";
      EDITOR = "emacs";
    };

    home.packages = with pkgs; [
      #terminal & tools
      alacritty gitAndTools.gh

      #chat
      tdesktop discord

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
      xautolock picom
    ];

    xdg.configFile."alacritty".source = "${dotfiles}/alacritty";
    xdg.configFile."dunst".source = "${dotfiles}/dunst";
    xdg.configFile."fish".source = "${dotfiles}/fish";
    xdg.configFile."zathura".source = "${dotfiles}/zathura";
    xdg.configFile."neofetch".source = "${dotfiles}/neofetch";
    xdg.configFile."picom.conf".source = "${dotfiles}/picom.conf";
    xdg.configFile."startship.toml".source = "${dotfiles}/startship.toml";

    xdg.configFile."xmobar" = {
      source = "${dotfiles}/xmobar";
      recursive = true;
    };

    xresources.properties = {
      "Sxiv.font" = "Fira Code:size=12";
      "Sxiv.foreground" = "#222222";
      "Sxiv.background" = "#A0C28A";
    };

    home.file.".gitconfig".source = "${dotfiles}/.gitconfig";
    home.file.".iex.exs".source = "${dotfiles}/.iex.exs";
    home.file.".patat.yaml".source = "${dotfiles}/.patat.yaml";
    home.file.".sbclrc".source = "${dotfiles}/.sbclrc";
    home.file.".vimrc".source = "${dotfiles}/.vimrc";
    home.file.".xinitrc".source = "${dotfiles}/.xinitrc";
    home.file.".xmonad".source = "${dotfiles}/.xmonad";
  };

}
