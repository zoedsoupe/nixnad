{ flakeroot ? ../../.
} @ args:
with builtins;
let
  flakeInput = import ./inputs.nix args;
in
let
  flake = import ((toString flakeroot) + "/flake.nix");
in
flake.outputs (flakeInput // { self = flakeInput; })
