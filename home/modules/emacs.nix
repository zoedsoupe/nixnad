{ pkgs, config, ... }:

{
  home = {
    sessionPath = [ "${config.xdg.configHome}/emacs/bin" ];
    sessionVariables = {
      DOOMDIR = "${config.xdg.configHome}/doom-config";
      DOOMLOCALDIR = "${config.xdg.configHome}/doom-local";
    };
  };

  xdg = {
    enable = true;
    configFile = {
      "doom-config" = {
        recursive = true;
        source = ./doom.d;
      };
      "emacs" = {
        source = builtins.fetchGit "https://github.com/hlissner/doom-emacs";
        onChange = "${pkgs.writeShellScript "doom-change" ''
          export DOOMDIR="${config.home.sessionVariables.DOOMDIR}"
          export DOOMLOCALDIR="${config.home.sessionVariables.DOOMLOCALDIR}"
          if [ ! -d "$DOOMLOCALDIR" ]; then
            ${config.xdg.configHome}/emacs/bin/doom -y install
          else
            ${config.xdg.configHome}/emacs/bin/doom -y sync -u
          fi
        ''}";
      };
    };
  };

  programs.emacs.enable = true;
  services.emacs = {
    enable = true;
    client.enable = true;
    socketActivation.enable = true;
  };
}
