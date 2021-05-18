{ config, pkgs, options,... }:

{
  imports =
    [
      ./boot.nix
      ./fish.nix
      ./network.nix
      ./programs.nix
      ./services.nix
      ./hardware-configuration.nix
    ];


  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";
  console.useXkbConfig = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  fonts.fonts = with pkgs; [
    cantarell-fonts
    font-awesome_4
    material-design-icons
    corefonts
    (nerdfonts.override {
      fonts = [ 
        "FiraCode"
        "Iosevka"
        "JetBrainsMono" 
 	      "Monofur"
      ];
    })
  ];

  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };
  virtualisation.libvirtd.enable = true;

  hardware.pulseaudio.enable = false;

  security.apparmor.enable = true;
  security.polkit.enable = true;
  security.rtkit.enable = true;
  security.sudo = {
    enable = true;
    configFile = ''
      Defaults lecture=always
      Defaults lecture_file=/etc/nixos/misc/groot.txt
    '';
  };

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
    '';
  };

  users = {
    users.root = {
      shell = pkgs.fish;
      initialPassword = "root";
    };
    users.matthew = {
      isNormalUser = true;
      shell = pkgs.fish;
      initialPassword = "nixos";
      extraGroups = [ 
        "wheel"  
        "libvirtd" 
        "networkmanager" 
        "docker" 
      ];
    };
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}


