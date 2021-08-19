# receives: type of item looking for in modules
# returns: paths of the modules it found
let
  ls-name = import <dotfiles/lib/ls-name.nix>;
  load-given-modules = import <dotfiles/lib/load-given-modules.nix>;
  items = ls-name <dotfiles/modules>;
in
load-given-modules items
