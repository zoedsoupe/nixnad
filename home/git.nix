{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    delta.enable = true;
    userEmail = "matheus_pessanha2001@outlook.com";
    userName = "Matheus de Souza Pessanha";
    extraConfig = {
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
    };
    ignores = [ "**/.~*" "*.swp" "*.swo" ".nix-*" ".postgres" ];
    signing = {
      gpgPath = "${pkgs.gnupg}/bin/gpg2";
      key = null;
      signByDefault = true;
    };
    aliases = {
      p = "push";
      s = "status";
      c = "commit";
      co = "checkout";
      aa = "add -p";
      st = "stash";
    };
  };
}
