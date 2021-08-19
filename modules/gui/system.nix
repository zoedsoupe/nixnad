let
  config = import ../../global-config.nix;
  selected-de = config.selected-desktop-environment;
in
import (./engine + "/${selected-de}")
