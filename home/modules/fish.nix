{ pkgs, ... }:

let
  elixirAliases = {
    phx_api = "mix phx.new --no-html --no-webpack --binary-id $argv";
    mes = "mix ecto.setup";
    megm = "mix ecto.gen.migration $argv";
    mem = "mix ecto.migrate";
    mdg = "mix deps.get";
    mdc = "mix deps.compile";
    mpr = "mix phx.routes";
    mpn = "mix phx.new $argv";
    mpgh = "mix phx.gen.html $argv";
    mpgl = "mix phx.gen.live $argv";
    mpgc = "mix phx.gen.context $argv";
    mpgj = "mix phx.gen.json $argv";
    mpgs = "mix phx.gen.schema $argv";
    ies = "iex -S mix";
    mps = "mix phx.server";
  };

  otherAliases = {
    lg = "lazygit";
    ps = "procs";
    top = "ytop";
    ls = "exa -l";
    cheat = "tldr $argv";
    prettyjson = "python -m json.tool | bat";
    d = "rm -rf $argv";
    please = "sudo $argv";
  };

  base = ''
    ### PROMPT ###
    starship init fish | source

    any-nix-shell fish --info-right | source

    set -x STARSHIP_CONFIG ~/.config/starship.toml

    set fish_greeting # suppress fish initital greeting

    set HISTCONTROL ignoreboth # ignore commands with initial space and duplicates

    function __direnv_export_eval --on-event fish_prompt;
        begin;
            begin;
                "/etc/profiles/per-user/matdsoupe/bin/direnv" export fish
            end 1>| source
        end 2>| egrep -v -e "^direnv: export"
    end;
  '';

  functions = ''
    ### FUNCTIONS ###
    function tre
        command tree -aC \
            -I '.git|.github|node_modules|deps|_build|.elixir_ls|.nix-hex|.nix-mix|.postgres' \
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
    shellAliases = elixirAliases // otherAliases;
    plugins = [
      {
        name = "z";
        src = pkgs.fetchFromGitHub {
          owner = "jethrokuan";
          repo = "z";
          rev = "ccb0ac58bc09841eaa2a54bf2aa7e9fb871d0e3f";
          sha256 = "05z6lnkmzbl212cbfp291p63qfzzqp73nkfizsgbmm0fbiqbi74p";
        };
      }
      {
        name = "omni";
        src = pkgs.fetchFromGitHub {
          owner = "getomni";
          repo = "fish";
          rev = "2d089c16bf254fdf962344cedf96e0b97391fb67";
          sha256 = "1q1mvw6wh6msksxmkysjq33b0hrgfvnljdbmj72kw12ya2iksxz6";
        };
      }
    ];
  };
}
