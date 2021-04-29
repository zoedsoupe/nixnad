{ pkgs, ... }:

let
  doom-emacs = pkgs.callPackage (builtins.fetchTarball {
    url = https://github.com/vlaci/nix-doom-emacs/archive/master.tar.gz;
  }) {
    doomPrivateDir = ./doom.d;
  };
in {
  home.packages = [ doom-emacs ];

  home.file.".emacs.d/init.el".text = ''
    (load "default.el")
  '';

  services.emacs = {
    enable = true;
    package = doom-emacs;
  };
}
