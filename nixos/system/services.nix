{ config, pkgs, ... }:

{
  # List services that you want to enable:
  services = {

    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Clipboard history on dmenu.
    clipmenu.enable = true;

    # Power management
    upower.enable = true;

    # Emacs daemon
    emacs.enable = true;

    dbus.enable = true;

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

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "ctrl:swapcaps";
      xkbVariant = "intl";
      libinput = {
        enable = true;
        touchpad.disableWhileTyping = true;
      };
      desktopManager.xterm.enable = false;
      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
      displayManager.startx.enable = true;
      updateDbusEnvironment = true;
      serverFlagsSection = ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "0"
          Option "OffTime" "0"
      '';
    };

    getty.helpLine = ''

            [0;34;40m ███    ██ ██ ██   ██  ██████  ███████ 
            [0;34;40m ████   ██ ██  ██ ██  ██    ██ ██      
            [0;34;40m ██ ██  ██ ██   ███   ██    ██ ███████ 
            [0;34;40m ██  ██ ██ ██  ██ ██  ██    ██      ██ 
            [0;34;40m ██   ████ ██ ██   ██  ██████  ███████ 
            [0;37;40m
    '';

  };

  systemd.services.betterlockscreen = {
    enable = true;
    description = "Locks screen when going to sleep/suspend";
    environment = { DISPLAY = ":0"; };
    serviceConfig = {
      User = "matthew";
      Type = "simple";
      ExecStart = ''${pkgs.betterlockscreen}/bin/betterlockscreen -l'';
      TimeoutSec = "infinity";
      alias = [ "betterlockscreen@matthew.service" ];
    };
    before = [ "sleep.target" "suspend.target" ];
    wantedBy = [ "sleep.target" "suspend.target" ];
  };
}
