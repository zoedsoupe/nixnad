#!/bin/fish

# BEGIN SYSTEM CONFIG
if test -d /etc/X11/xinit/xinitrc.d
  for f in /etc/X11/xinit/xinitrc.d/?*.sh
    test -x "$f" && sh "$f"
  end
 set -e f
end
# END SYSTEM CONFIG

# set second monitor as primary 
if xrandr | grep -q 'HDMI1 connected'
  xrandr --output eDP1 --auto --output HDMI1 --primary --auto --left-of eDP1
else if xrandr | grep -q 'DP1 connected'
  xrandr --output eDP1 --auto --output DP1 --primary --auto --left-of eDP1
end

feh --bg-fil --randomize ~/pics/wallpapers &

exec xmonad

switch $1
  case xmonad
    exec xmonad
  case "*"
    echo "No WM or DE specified..."
end
# END SYSTEM CONFIG
