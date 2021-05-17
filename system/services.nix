{ config, pkgs, ... }:

{
  imports = [ ./gnome ];

  location = {
    provider = "manual";
    latitude = -21.7629877;
    longitude = -41.296212;
  };

  xdg.portal = {
    enable = true;
    gtkUsePortal = true;
  };

  # List services that you want to enable:
  services = {

    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Power management
    upower.enable = true;

    dbus.enable = true;

    pipewire.enable = true;

    # X compositor
    picom = {
      enable = true;
      inactiveOpacity = 0.8;
      backend = "glx";
      shadow = true;
      shadowOpacity = 0.5;
      shadowOffsets = [ (-60) (-25) ];
      shadowExclude = [ 
        "class_g = 'xmobar'"
        "class_g = 'google-chrome-stable'" 
      ];
      opacityRules = [ 
        "90:class_g = 'alacritty'" 
      ];
      settings = {
        shadow = { radius = 5; };
        blur = {
          background = true;
          backgroundFrame = true;
          backgroundFixed = true;
          kern = "3x3box";
          strength = 10;
        };
      };
    };

    # Bluetooth tool
    blueman.enable = true;

    # Enable CUPS to print documents.
    printing = {
      enable = true;
      drivers = [ pkgs.hplipWithPlugin ];
    };

    redshift = {
      enable = true;
      brightness = {
        day = "1";
        night = "1";
      };
      temperature = {
        day = 5500;
        night = 3700;
      };
    };

    thermald.enable = true;

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "ctrl:swapcaps";
      xkbVariant = "intl";
      libinput = {
        enable = true;
        tapping = true;
        disableWhileTyping = true;
        naturalScrolling = true;
      };
      desktopManager.xterm.enable = false;
      updateDbusEnvironment = true;
      serverFlagsSection = ''
          Option "BlankTime" "180"
          Option "StandbyTime" "45"
          Option "SuspendTime" "45"
          Option "OffTime" "180"
      '';
    };

    mingetty.helpLine = ''

            [0;34;40m ███    ██ ██ ██   ██  ██████  ███████ 
            [0;34;40m ████   ██ ██  ██ ██  ██    ██ ██      
            [0;34;40m ██ ██  ██ ██   ███   ██    ██ ███████ 
            [0;34;40m ██  ██ ██ ██  ██ ██  ██    ██      ██ 
            [0;34;40m ██   ████ ██ ██   ██  ██████  ███████ 
            [0;37;40m
    '';

  };
}
