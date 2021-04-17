{ config, lib, pkgs, ... }:

{
  programs.gh = {
    enable = true;
    editor = "vim";
    gitProtocol = "ssh";
  };

  programs.git = {
    enable = true;
    lfs = true;
    delta.enable = true;
    userEmail = "matheus_pessanha2001@outlook.com";
    userName = "Matheus de Souza Pessanha";
    extraConfig = {
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
    };
  };
}