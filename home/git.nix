{ pkgs, ... }:

{
  programs.git = {
    enable = true;
    lfs.enable = true;
    delta.enable = true;
    userEmail = "zoey.spessanha@zeetech.io";
    userName = "Zoey de Souza Pessanha";
    extraConfig = {
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
    };
    ignores = [ "**/.~*" "*.swp" "*.swo" ".nix-*" ".postgres" ".envrc" ".direnv" ];
    signing = {
      gpgPath = "${pkgs.gnupg}/bin/gpg2";
      key = "2D4D 488F 17FB FF75 664E  C016 6DFD 6562 20A3 B849";
      signByDefault = true;
    };
    aliases = {
      p = "push";
      s = "status";
      c = "commit";
      co = "checkout";
      aa = "add -p";
      st = "stash";
    };
  };
}
