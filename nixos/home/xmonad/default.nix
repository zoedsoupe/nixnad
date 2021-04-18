{ pkgs, ...}:

{
  home-manager.users.matthew = {
    xresources.properties = {
      "Xft.dpi" = 180;
      "Xft.autohint" = 0;
      "Xft.hintstyle" = "hintfull";
      "Xft.hinting" = 1;
      "Xft.antialias" = 1;
      "Xft.rgba" = "rgb";
    };

    xsession = {
      enable = true;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
        config = ./config.hs;
      };
    };
  };
}
