{ config, lib, pkgs, ... }:

{
  home-manager.users.matthew = {
    programs = {
      gh = {
        enable = true;
        editor = "vim";
        gitProtocol = "ssh";
      };

      git = {
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
    };
  };
}
