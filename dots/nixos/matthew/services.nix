
{ config, pkgs, ... }:

{

  # List services that you want to enable:
  services = {
    # Enable the OpenSSH daemon.
    openssh.enable = true;

    # Bluetooth tool
    blueman.enable = true;

    # Enable CUPS to print documents.
    printing = {
      enable = true;
      drivers = [ pkgs.hplipWithPlugin ];
    };
    
    udev.extraRules = "
    SUBSYSTEMS=\"input\", AATRS{name}==\"Keychron K3\", RUN+=\"/home/matthew/scripts/fix_keyboard\"
    ";    

    # Enable the X11 windowing system.
    xserver = {
      enable = true;
      layout = "us";
      xkbOptions = "ctrl:swapcaps,compose:rwin";
      libinput.enable = true;
      desktopManager.xterm.enable = false;
      displayManager.startx.enable = true;
      updateDbusEnvironment = true;
      serverFlagsSection = ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "0"
          Option "OffTime" "0"
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

  # Bluetooth media buttons
  systemd.user.services.mpris-proxy = {
    Unit.Description = "Mpris proxy";
    Unit.After = [ "network.target" "sound.target" ];
    Service.ExecStart = "${pkgs.bluez}/bin/mpris-proxy";
    Install.WantedBy = [ "default.target" ];
  };
}
