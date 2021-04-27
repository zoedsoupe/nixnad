{ config, lib, pkgs, ... }:

{
  programs.autojump = {
    enable = true;
    enableFishIntegration = true;
  };
}
