#! /usr/bin/env nix-shell
#! nix-shell -i fish

nix-shell -p nixpkgs-fmt --run 'nixpkgs-fmt ./**/*.nix'
