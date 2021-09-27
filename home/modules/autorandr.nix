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
      "solo" = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff000daeca1500000000221801049522137802c3c5915554942824505400000001010101010101010101010101010101da1d56e250002030442d470058c110000018000000fe004e3135364247452d4534320a20000000fe00434d4e0a202020202020202020000000fe004e3135364247452d4534320a200055";
        };
        config = {
          "eDP-1" = {
            enable = true;
            primary = true;
            mode = "1366x768";
            position = "0x0";
            rotate = "normal";
          };
         "DP-1".enable = false; 
         "HDMI-1".enable = false; 
        };
      };
      "home" = {
        fingerprint = {
          "eDP-1" = "00ffffffffffff000daeca1500000000221801049522137802c3c5915554942824505400000001010101010101010101010101010101da1d56e250002030442d470058c110000018000000fe004e3135364247452d4534320a20000000fe00434d4e0a202020202020202020000000fe004e3135364247452d4534320a200055";
          "HDMI-1" = "00ffffffffffff001e6d985b010101010119010380431c78eaca95a6554ea1260f5054a54b80714f818081c0a9c0b3000101010101017e4800e0a0381f4040403a00a11c21000018023a801871382d40582c4500a11c2100001e000000fc003235554d3538470a2020202020000000fd00384b1e5a18000a20202020202001f502031cf1499004031412051f0113230907078301000065030c002000023a801871382d40582c450056512100001e011d8018711c1620582c250056512100009e011d007251d01e206e28550056512100001e295900a0a038274030203a00a5222100001a000000ff000000000000000000000000000000000000000000000091";
        };
        config = {
          "eDP-1" = {
            enable = true;
            mode = "1366x768";
            position = "2560x156";
            rotate = "normal";
          };
          "HDMI-1" = {
            enable = true;
            primary = true;
            mode = "2560x1080";
            position = "0x0";
            rotate = "normal";
          };
        };
      };
    };
  };
}
