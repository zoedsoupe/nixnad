{ config, pkgs, options,... }:

{
  imports =
    [
      ./system/boot.nix
      ./system/fish.nix
      ./system/network.nix
      ./system/programs.nix
      ./system/services.nix
      ./system/hardware-configuration.nix
      ./home/home.nix
    ];


  # Select internationalisation properties.
  i18n.defaultLocale = "en_CA.UTF-8";
  console.useXkbConfig = true;

  # Set your time zone.
  time.timeZone = "America/Sao_Paulo";

  fonts.fonts = with pkgs; [
    fira-code
    fira-code-symbols
    jetbrains-mono
    cantarell-fonts
    font-awesome_4
    material-design-icons
    corefonts
    (nerdfonts.override {
      fonts = [ 
        "FiraCode"
        "Iosevka"
        "JetBrainsMono" 
 	      "Hasklig"
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

  security.apparmor.enable = true;
  security.polkit.enable = true;
  security.sudo = {
    enable = true;
    configFile = ''
      Defaults lecture=always
      Defaults lecture_file=/etc/nixos/misc/groot.txt
    '';
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio = {
    enable = true;
    package = pkgs.pulseaudioFull;
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


