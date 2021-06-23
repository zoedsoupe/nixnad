{ pkgs, ... }:

let
  gtk-omni = pkgs.fetchFromGitHub {
    owner = "getomni";
    repo = "gtk";
    rev = "e81b3fbebebf53369cffe1fb662abc400edb04f7";
    sha256 = "08h4x9bjd3p3h00adj9060q27w544acsnn5ifxyahqxbdy8669im";
  };
in {
  home.file = {
    ".iex.exs".text = ''
      colors_opts = [
        syntax_colors: [
          number: :light_yellow,
          atom: :light_cyan,
          string: :light_green,
          boolean: :light_blue,
          nil: [:magenta, :bright]
        ],
        ls_directory: :cyan,
        ls_device: :yellow,
        doc_code: :green,
        doc_inline_code: :magenta,
        doc_headings: [:cyan, :underline],
        doc_title: [:cyan, :bright, :underline]
      ]

      prompt = [
        # ANSI CHA, move cursor to column 1
        "\e[G",
        :light_magenta,
        # plain string
        "ﬦ",
        ">",
        :white,
        :reset
      ]
      |> IO.ANSI.format()
      |> IO.chardata_to_string()


      IEx.configure(
        inspect: [limit: :infinity, pretty: true],
        colors: colors_opts,
        default_prompt: prompt
      )
    '';

    ".ghci".text = ''
      :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
    '';

    ".themes/gtk-omni" = {
      recursive = true;
      source = gtk-omni;
    };
  };

  xdg.configFile."direnv/direnvrc".text = ''
    #! /usr/bin/env nix-shelL
    #! nix-shell -i bash -p postgresql

    layout_postgres() {
      echo "direnv: usign layout postgres"

      export PGHOST=$(direnv_layout_dir)/.postgres
      export PGDATA=$PGHOST/data
      export PGLOG=$PGHOST/postgres.log
      export PGUSER=postgres
      export PGPASSWORD=postgres

      if [[ ! -d "$PGDATA" ]]; then
        echo 'Initializing postgresql daemon...'
        initdb --auth=trust --no-locale --encoding=UTF8 -U=$PGUSER >/dev/null

        echo "unix_socket_directories = '$PGHOST'" >> "$PGDATA/postgresql.conf"

        echo "CREATE USER postgres SUPERUSER;" | postgres --single -E postgres
        echo "CREATE DATABASE $PGUSER;" | postgres --single -E postgres
      fi
    }
  '';
}
