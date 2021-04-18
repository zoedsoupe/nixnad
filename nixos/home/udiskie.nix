{ config, lib, pkgs, ... }:

{
  home-manager.users.matthew = {
    services.udiskie = {
      enable = true;
      automount = true;
      notify = true;
    };
  };
}
