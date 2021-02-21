#!/bin/sh

# BEGIN SYSTEM CONFIG
userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

twm &
xclock -geometry 50x50-1+1 &
xterm -geometry 80x50+494+51 &
xterm -geometry 80x20+494-0 &
exec xterm -geometry 80x66+0+0 -name login
# END SYSTEM CONFIG

# BEGIN USER CONFIG
sudo tee /etc/resolv.conf > /dev/null << EOF
nameserver 8.8.8.8
nameserver 8.8.4.4
EOF

# set second monitor as primary 
if [ xrandr | grep -q 'HDMI-1 connected' ] 
then
  xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --left-of eDP-1
else if [ xrandr | grep -q 'DP-1 connected' ]
  then
    xrandr --output eDP-1 --auto --output DP-1 --primary --auto --left-of eDP-1
  fi
fi

# swap ctrl and caps lock and enable comopse key
setxkbmap -layout us -option ctrl:swapcaps -option compose:rwin

clipmenud &
udiskie &
picom --experimental-backends &
/usr/bin/emacs --daemon &
feh --bg-fil --randomize ~/pics/wallpapers &

# END SYSTEM CONFIG
