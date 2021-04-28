{ pkgs, ... }:

{
  programs.autorandr = {
    enable = true;
    hooks.postswitch = {
      "change-bg" = ''
      #!/bin/sh
      feh --bg-fill --randomize ~/pics/wallpapers &
      '';
    };
    profiles = {
      "boosting" = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff000daeca1500000000221801049522137802c3c5915554942824505400000001010101010101010101010101010101da1d56e250002030442d470058c110000018000000fe004e3135364247452d4534320a20000000fe00434d4e0a202020202020202020000000fe004e3135364247452d4534320a200055";
          "DP-1" = "00ffffffffffff0010ac27a041314c3122110104a5221b78e6adb0a2574a9c25115054a54b00714f8180010101010101010101010101302a009851002a4030701300520e1100001e000000ff004a4d38393437384d314c31410a000000fc0044454c4c204531373846500a20000000fd00384b1f500e000a20202020202000c1";
        };
        config = {
          "eDP-1" = {
            enable = true;
            mode = "1366x768";
            position = "1280x128";
            rotate = "normal";
          };
          "DP-1" = {
            enable = true;
            primary = true;
            mode = "1280x1024";
            position = "0x0";
            rotate = "normal";
          };
        };
      };
    };
  };
}
