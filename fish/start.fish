# swap ctrl and caps lock and enable comopse key
setxkbmap -layout us -option ctrl:swapcaps -option compose:rwin

# correct DNS handling
sudo sh -c "echo 'nameserver 8.8.8.8' > /etc/resolv.conf"

# set second monitor as primary 
if xrandr | grep -q 'HDMI-1 connected'
    xrandr --output eDP-1 --auto --output HDMI-1 --primary --auto --left-of eDP-1
else if xrandr | grep -q 'DP-1 connected'
    xrandr --output eDP-1 --auto --output DP-1 --primary --auto --left-of eDP-1
end
