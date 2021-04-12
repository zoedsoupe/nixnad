#!/bin/sh

set -e

# BEGIN SYSTEM CONFIG
if [ test -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/?*.sh; do
    [ -x "$f" && sh "$f" ]
  done
fi

# END SYSTEM CONFIG

# set second monitor as primary 
if xrandr | grep -q 'HDMI1 connected'; then
  xrandr --output eDP1 --auto --output HDMI1 --primary --auto --left-of eDP1
elif xrandr | grep -q 'DP1 connected'; then
  xrandr --output eDP1 --auto --output DP1 --primary --auto --left-of eDP1
fi

feh --bg-fil --randomize ~/pics/wallpapers &

exec xmonad
# END SYSTEM CONFIG
