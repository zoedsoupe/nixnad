{ pkgs, ... }:

let
  aliases = {
    lg = "lazygit";
    ps = "procs";
    top = "ytop";
    ls = "exa -l";
    cheat = "tldr $argv";
    prettyjson = "python -m json.tool | bat";
    d = "rm -rf $argv";
    please = "sudo $argv";
    "..." = "cd ../../";
    nvim = "nix run github:zoedsoupe/copper#nvim.";
    vim = "nix run github:zoedsoupe/copper#nvim.";
  };

  base = ''
    ### PROMPT ###
    starship init fish | source

    any-nix-shell fish --info-right | source

    set -x GPG_TTY (tty)

    set -x STARSHIP_CONFIG ~/.config/starship.toml

    set fish_greeting # suppress fish initital greeting

    set HISTCONTROL ignoreboth # ignore commands with initial space and duplicates
  '';

  functions = ''
    ### FUNCTIONS ###
    function tre
        command tree -aC \
            -I '.git|.github|node_modules|deps|_build|.elixir_ls|.nix-hex|.nix-mix|.postgres|.direnv' \
            --dirsfirst $argv | bat
    end

    function cd
        builtin cd $argv && ls -a .
    end

    function ls
        command exa
    end

    function mkd
        command mkdir -p $argv && cd $argv
    end

    function clean_node_modules
        command find . -name "node_modules" -type d -prune -exec rm -rf '{}' +
    end
  '';
in
{
  programs.fish = {
    enable = true;
    shellInit = (base + functions);
    shellAliases = aliases;
    plugins = [
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "HEAD";
          sha256 = "05z6lnkmzbl212cbfp291p63qfzzqp73nkfizsgbmm0fbiqbi74p";
        };
      }
      {
        name = "omni";
        src = pkgs.fetchFromGitHub {
          owner = "getomni";
          repo = "fish";
          rev = "v1.0.0";
          sha256 = "1q1mvw6wh6msksxmkysjq33b0hrgfvnljdbmj72kw12ya2iksxz6";
        };
      }
      {
        name = "fzf.fish";
        src = pkgs.fetchFromGitHub {
          owner = "PatrickF1";
          repo = "fzf.fish";
          rev = "v8.1";
          sha256 = "uqYVbRdrcO6StaIim9S8xmb9P67CmXnLEywSeILn4yQ=";
        };
      }
    ];
  };
}
