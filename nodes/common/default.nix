{ global, pkgs, lib, self, ... }:

{
  imports = [
    ../bootstrap
    ../../modules/cachix/system.nix
  ];

  cachix.enable = true;
  nix.maxJobs = lib.mkDefault 8;
  hardware.cpu.intel.updateMicrocode = true;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services.fstrim = {
    enable = true;
    interval = "weekly";
  };

  zramSwap = {
    enable = true;
    algorithm = "zstd";
    memoryPercent = 60;
  };

  hardware.bluetooth = {
    enable = true;
    settings = {
      General = {
        Name = "mdsp-note";
        DiscoverableTimeout = 0;
        AlwaysPairable = true;
        FastConnectable = true;
      };

      Policy = {
        AutoEnable = true;
        ReconnectAttempts = 5;
        ReconnectIntervals = "1,2,4,8,16";
      };
    };
  };

  environment = {
    variables = {
      EDITOR = "nvim";
      TERMINAL = "kitty";
      GC_INITIAL_HEAP_SIZE = "32M";
    };
    systemPackages = with pkgs; [
      # terminal & tools
      neofetch procs wget
      unzip exa pciutils
      unrar psmisc bat
      cmatrix iw curl
      ncdu lazygit glow
      fd nnn jq tldr
      git alacritty acpi
      ag

      # window manager
      # moc dmenu pamixer
      # screenkey clipmenu
      # wirelesstools
      # betterlockscreen

      # editor/ide
      vim neovim

      # browser
      google-chrome

      # dev
      google-cloud-sdk gcc tree-sitter

      # tools
      gparted

      # audio & video
      mpv
      pavucontrol
      ffmpeg

      # image
      feh

      # others
      zathura
      inotify-tools

      # xorg
      xorg.xrandr
      xclip

      # games
      steamcmd

      # gnome stuff
      gnome.gnome-tweaks
      gnomeExtensions.emoji-selector
      gnomeExtensions.clipboard-indicator
      numix-icon-theme-circle
      numix-cursor-theme
    ];
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    kernelParams = [
      "vt.default_red=0x4c,0xff,0xa3,0xdb,0x5c,0xb9,0x27,0xff,0x63,0xf9,0x4f,0xdb,0x69,0xb9,0x70,0xff"
      "vt.default_grn=0x4c,0x6c,0xdf,0xee,0xb6,0x81,0xc3,0xff,0x83,0x54,0xda,0xee,0xc3,0x81,0x97,0xff"
      "vt.default_blu=0x4c,0x6c,0x4c,0x77,0x4f,0xf4,0xff,0xff,0xa7,0x6d,0xee,0x77,0x5c,0xf4,0xff,0xff"
      "quiet"
      "splash"
      "udev.log_priority=3"
    ];
    consoleLogLevel = 0;
    initrd.verbose = false;
    plymouth.enable = true;
    loader.timeout = 0;
    loader = {
      grub.enable = true;
      grub.version = 2;
      grub.efiSupport = true;
      grub.device = "nodev"; # or "nodev" for efi only
      grub.splashImage = ./boot_wallpaper.jpg;
      grub.useOSProber = true;
      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };
    supportedFilesystems = [ "ntfs" "btrfs" ];
    cleanTmpDir = true;
    kernel.sysctl = {
      "vm.overcommit_memory" = "1";
      "vm.swappiness" = 100; # if you're using (z)swap and/or zram. if you aren't, you should.
    };
  };
}
