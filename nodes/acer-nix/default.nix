{ pkgs, ... }:

with import ../../global-config.nix;

let
  hostname = "acer-nix";
in
{
  imports = [
    ../common
    ./hardware-configuration.nix
    ../../modules/gui/system.nix
  ];

  networking = {
    hostName = "${username}";
    useDHCP = false;
    interfaces.enp4s0.useDHCP = true;
    interfaces.wlp0s20f3.useDHCP = true;
    networkmanager.enable = true;
  };

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

    openvpn.servers = {
      solfacilVPN = {
        autoStart = false;
        config = "config /etc/openvpn/matheus.ovpn";
      };
    };

    pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;
      media-session.config.bluez-monitor.rules = [
        {
          # Matches all cards
          matches = [{ "device.name" = "~bluez_card.*"; }];
          actions = {
            "update-props" = {
              "bluez5.reconnect-profiles" = [ "hfp_hf" "hsp_hs" "a2dp_sink" ];
              # mSBC is not expected to work on all headset + adapter combinations.
              "bluez5.msbc-support" = true;
            };
          };
        }
        {
          matches = [
            # Matches all sources
            { "node.name" = "~bluez_input.*"; }
            # Matches all outputs
            { "node.name" = "~bluez_output.*"; }
          ];
          actions = {
            "node.pause-on-idle" = false;
          };
        }
      ];
    };

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
        touchpad = {
          tapping = true;
          disableWhileTyping = true;
          naturalScrolling = true;
        };
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

    getty.helpLine = ''
      [0;34;40m ███    ██ ██ ██   ██  ██████  ███████ 
      [0;34;40m ████   ██ ██  ██ ██  ██    ██ ██      
      [0;34;40m ██ ██  ██ ██   ███   ██    ██ ███████ 
      [0;34;40m ██  ██ ██ ██  ██ ██  ██    ██      ██ 
      [0;34;40m ██   ████ ██ ██   ██  ██████  ███████ 
      [0;37;40m
    '';
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.03"; # Did you read the comment?
}
