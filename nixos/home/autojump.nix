{ config, lib, pkgs, ... }:

{
  home-manager.users.matthew.programs.autojump = {
    enable = true;
    enableFishIntegration = true;
  };
}
