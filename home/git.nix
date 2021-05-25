{ config, lib, pkgs, ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    delta.enable = true;
    userEmail = "zoey.spessanha@zeetech.io";
    userName = "Zoey de Souza Pessanha";
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
  };
}
