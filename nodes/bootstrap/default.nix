{ global, pkgs, ... }:

with global;

{
  i18n.defaultLocale = "en_CA.UTF-8";
  console.useXkbConfig = true;
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

  programs = {
    steam.enable = true;
    command-not-found.enable = true;
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "curses";
    };
  };

  virtualisation.docker = {
    enable = true;
    autoPrune = {
      enable = true;
      dates = "weekly";
    };
  };
  virtualisation.libvirtd.enable = true;

  hardware.pulseaudio.enable = false;
  sound.enable = false;

  security.apparmor.enable = true;
  security.polkit.enable = true;
  security.rtkit.enable = true;
  security.sudo = {
    enable = true;
    configFile = ''
      Defaults insults
      Defaults lecture=always
      Defaults lecture_file="${rootPath}/modules/misc/groot.txt"
    '';
  };

  nix = {
    autoOptimiseStore = true;
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 7d";
    };
    trustedUsers = [ username "@wheel" ];
    package = pkgs.nixFlakes;
    extraOptions = ''
      keep-outputs = true
      keep-derivations = true
      min-free = ${toString (1  * 1024*1024*1024)}
      max-free = ${toString (10 * 1024*1024*1024)}
      experimental-features = nix-command flakes
    '';
  };

  users = {
    users.root = {
      shell = pkgs.fish;
      initialPassword = "root";
    };
    users."${username}" = {
      isNormalUser = true;
      shell = pkgs.fish;
      initialPassword = "nixos";
      extraGroups = [
        "wheel"
        "audio"
        "libvirtd"
        "networkmanager"
        "docker"
      ];
    };
  };

  nixpkgs.config = {
    allowUnfree = true;
  };
}
