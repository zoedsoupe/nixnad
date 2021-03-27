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

# swap ctrl and caps lock and enable comopse key
setxkbmap -layout us -option ctrl:swapcaps -option compose:rwin &

bluetoothctl power on > /dev/null &
bluetoothctl scan on > /dev/null &

if ! pidof picom > /dev/null
  picom --experimental-backends &
end

if ! pidof dockerd > /dev/null
  sudo dockerd &
end

if ! pidof emacs > /dev/null
  emacs --daemon &
end

if ! procs | grep clipmenu > /dev/null
  clipmenud &
end

if ! pidof udiskie > /dev/null
  udiskie &
end

feh --bg-fil --randomize ~/pics/wallpapers &

dbus &

exec xmonad

switch $1
  case xmonad
    exec xmonad
  case "*"
    echo "No WM or DE specified..."
end
# END SYSTEM CONFIG
