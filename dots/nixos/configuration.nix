{ config, pkgs, ... }:

{
  imports =
    [ 
      ./hardware-configuration.nix
      ./matthew/boot.nix
      ./matthew/home.nix
      ./matthew/services.nix
      ./matthew/programs.nix
      ./matthew/network.nix
    ];

  i18n.defaultLocale = "America/Sao_Paulo";

  console.useXkbConfig = true;

  fonts.fonts = with pkgs; [
    fira-code
    jetbrains-mono
    noto-fonts
    noto-fonts-cjk
    cantarell-fonts
    (nerdfonts.override {
      fonts = [ "FiraCode" "JetbrainsMono" ];
    })
  ];

  virtualisation.docker.enable = true;
  virtualisation.libvirtd.enable = true;

  security.apparmor.enable = true;
  security.polkit.enable = true;
  security.sudo = {
    enable = true;
    wheelNeedsPassword = false;
  };

  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
  };

  hardware.bluetooth.config = {
   General = {
     Name = "mdsp-note";
     DiscoverableTimeout = "0";
     AlwaysPairable = true;
     FastConnectable = true;
   };
   Policy = {
     Autonbale = true;
     ReconnectAttempts = 5;
     ReconnectIntervals = "1,2,4,8,16";
   };
  };

  users = {
    users.root = {
      shell = pkgs.fish;
      initialPassword = "mdsp";
    };
    users.matthew = {
      isNormalUser = true;
      shell = pkgs.fish;
      initialPassword = "nixos";
      extraGroups = [
        "video" "wheel"
        "docker" "libvirtd"
        "networkmanager"
      ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; 
  # Did you read the comment?
}

