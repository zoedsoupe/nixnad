{ config, pkgs, ... }:

let

  gtk-omni = pkgs.fetchFromGitHub {
    owner = "getomni";
    repo = "gtk";
    rev = "e81b3fbebebf53369cffe1fb662abc400edb04f7";
    sha256 = "08h4x9bjd3p3h00adj9060q27w544acsnn5ifxyahqxbdy8669im";
  };

in {
  home.file = {
    ".iex.exs".text = "IEx.configure(inspect: [limit: :infinity, pretty: true])";

    ".ghci".text = ''
      :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
    '';

    ".themes/gtk-omni" = {
      recursive = true;
      source = gtk-omni;
    };
  };
}
