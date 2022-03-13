{ global, pkgs, ... }:

let
  inherit (global) username email rootPath;
in
{
  programs.git = {
    enable = true;
    lfs.enable = true;
    delta.enable = true;
    userEmail = "${email}";
    userName = "Zoey Pessanha";
    extraConfig = {
      github = { user = "${username}"; };
      grep = { linenumber = true; };
      merge = { log = true; };
      rebase = { autosquash = true; };
      fetch = { prune = true; };
      push = { default = "current"; };
      apply = { whitespace = "nowarn"; };
      help = { autocorrect = 0; };
      user = { username = "zoedsoupe"; };
      init = { defaultBranch = "main"; };
      pull = { rebase = false; };
      commit = { 
        template = "${rootPath}/modules/misc/gitmessage"; 
      };
      log = {
        follow = true;
        abbrevCommit = true;
      };
      core = {
        editor = "nvim";
        autocrlf = "input";
        whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
      };
      color = {
        grep = "auto";
        branch = "auto";
        diff = "auto";
        status = "auto";
        showbranch = "auto";
        interactive = "auto";
        ui = "auto";
      };
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
      lg = "log --graph --oneline --decorate --abbrev-commit";
    };
  };
}
