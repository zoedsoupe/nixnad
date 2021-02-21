#!/bin/sh

# BEGIN SYSTEM CONFIG
if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi
# END SYSTEM CONFIG

# BEGIN USER CONFIG
sudo tee /etc/resolv.conf > /dev/null << EOF
nameserver 8.8.8.8
nameserver 8.8.4.4
EOF

# set second monitor as primary 
if xrandr | grep -q 'HDMI-1 connected' ; then
  xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --left-of eDP-1
else if xrandr | grep -q 'DP-1 connected' ; then
    xrandr --output eDP-1 --auto --output DP-1 --primary --auto --left-of eDP-1
  fi
fi

# swap ctrl and caps lock and enable comopse key
setxkbmap -layout us -option ctrl:swapcaps -option compose:rwin

if ! pidof picom > /dev/null ; then
  picom --experimental-backends &
fi

if ! pidof emacs > /dev/null ; then
  emacs --daemon
fi

if ! ps -aux | grep clipmenu > /dev/null ; then
  clipmenud &
fi

if ! pidof udiskie > /dev/null ; then 
  udiskie &
fi

feh --bg-fil --randomize ~/pics/wallpapers &

exec xmonad
# END SYSTEM CONFIG
