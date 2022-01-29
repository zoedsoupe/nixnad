{ global, pkgs, ... }:

let
  inherit (global) username email;
in
{
  programs.git = {
    enable = true;
    lfs.enable = true;
    delta.enable = true;
    userEmail = "${email}";
    userName = "${username}";
    extraConfig = {
      init = { defaultBranch = "main"; };
      pull = { rebase = true; };
    };
    ignores = [ 
      "**/.~*" 
      "*.swp" 
      "*.swo" 
      ".nix-*" 
      ".postgres" 
      ".envrc" 
      ".direnv"
      "shell.nix"
    ];
    signing = {
      gpgPath = "${pkgs.gnupg}/bin/gpg2";
      key = "EAA1 51DB 472B 0122 109A  CB17 1E1E 889C DBD6 A315";
      signByDefault = true;
    };
    aliases = {
      p = "push";
      s = "status";
      c = "commit";
      co = "checkout";
      aa = "add -p";
      st = "stash";
      br = "branch";
    };
  };
}
