{ config, lib, pkgs, ... }:

{
  programs.gh = {
    enable = true;
    editor = "vim";
    gitProtocol = "ssh";
  };

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
  };
}
