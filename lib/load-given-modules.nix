# receives: list of modules to import, type of item looking for in modules
# returns: paths of the modules it found
items:
component:
let
  filter = import <dotfiles/lib/filter.nix>;
  tail = import <dotfiles/lib/tail.nix>;
  module-path = <dotfiles/modules>;
  get-module-name = item: tail (builtins.split "/" (builtins.toString item));
  suffixify-item = item:
    let
      suffix = "/" + item + "/" + component + ".nix";
    in
    <dotfiles/modules> + suffix;
  suffixed-items = map suffixify-item (map get-module-name items);
  filtered-items = filter builtins.pathExists suffixed-items;
in
map builtins.toPath filtered-items
