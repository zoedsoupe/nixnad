#!/bin/fish

# BEGIN SYSTEM CONFIG
if test -d /etc/X11/xinit/xinitrc.d
  for f in /etc/X11/xinit/xinitrc.d/?*.sh
    test -x "$f" && sh "$f"
  end
 set -e f
end
# END SYSTEM CONFIG

# BEGIN USER CONFIG
#sudo tee /etc/resolv.conf > /dev/null << EOF
#nameserver 8.8.8.8
#nameserver 8.8.4.4#
#EOF

# set second monitor as primary 
if xrandr | grep -q 'HDMI-1 connected'
  xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --left-of eDP-1
else if xrandr | grep -q 'DP-1 connected'
    xrandr --output eDP-1 --auto --output DP-1 --primary --auto --left-of eDP-1
end

# swap ctrl and caps lock and enable comopse key
setxkbmap -layout us -option ctrl:swapcaps -option compose:rwin &

bluetoothctl power on > /dev/null &

if ! pidof picom > /dev/null
  picom --experimental-backends &
end

if ! pidof emacs > /dev/null
  emacs --daemon
end

if ! procs | grep clipmenu > /dev/null
  clipmenud &
end

if ! pidof udiskie > /dev/null
  udiskie &
end

feh --bg-fil --randomize ~/pics/wallpapers &

exec xmonad
# END SYSTEM CONFIG
