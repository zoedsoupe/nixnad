{ config, lib, pkgs, ... }:

{
  programs.udiskie = {
    enable = true;
    automount = true;
    notify = true;
  };
}
