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
      key = "2F32 CFEF E11A D73B A740  FA47 2671 964A B1E0 6A08";
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
