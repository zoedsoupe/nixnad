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
  };
}
