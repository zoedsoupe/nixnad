let
  config = import ../../globasl-config;
  selected-de = config.selected-desktop-environment;
in
import (./engine + "/${selected-de}")
