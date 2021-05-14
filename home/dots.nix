{ config, pkgs, ... }:

{
  home.file = {
    ".iex.exs".text = "IEx.configure(inspect: [limit: :infinity, pretty: true])";

    ".ghci".text = ''
      :set prompt "\ESC[1;34m%s\n\ESC[0;34mλ> \ESC[m"
    '';
  };
}
