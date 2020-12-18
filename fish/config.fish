# theme
starship init fish | source

echo 'nameserver 8.8.8.8' | sudo tee /etc/resolv.conf
setxkbmap -option ctrl:swapcaps
feh --bg-fill ~/Pics/wallpapers/mountains.jpg
xset r off

# autojump
begin
    set --local AUTOJUMP_PATH $HOME/.autojump/share/autojump/autojump.fish
    if test -e $AUTOJUMP_PATH
        source $AUTOJUMP_PATH
    end
end

# vi mode
fish_vi_key_bindings

#aliases
source $HOME/.config/fish/aliases.fish 

# env vars
set PATH $HOME/bin /usr/local/bin \
  $HOME/.ebcli-virtual-env/executables \
  $HOME/.yarn/bin \
  $HOME/.config/yarn/global/node_modules/.bin \
  $HOME/.local/bin ~/scripts/ ~/.cargo/bin $PATH
set fish_greeting

# Starship theme config
set STARSHIP_CONFIG ~/Documents/privy/dotfiles/startship.toml

# Default Apps
set EDITOR nvim
set READER zathura
set VISUAL nvim
set TERMINAL kitty
set BROWSER microsoft-edge-dev
set VIDEO mpv
set IMAGE feh
set COLORTERM truecolor
set OPENER xdg-open

# fast node manager
fnm env | source

