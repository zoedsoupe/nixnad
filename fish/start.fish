# swap ctrl and caps lock
setxkbmap -option ctrl:swapcaps

# wallpaper
feh --bg-scale ~/Pics/wallpapers/mountains.jpg

# cannot hold and press keys
xset r off

# correct DNS handling
sudo sh -c "echo 'nameserver 8.8.8.8' > /etc/resolv.conf"

# set second monitor as primary 
xrandr | grep -q 'HDMI-1 connected' && xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --above eDP-1

# start compositor
inxi -Gxx | grep -q compositor || picom -b
