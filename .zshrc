neofetch
# THEME
eval "$(starship init zsh)"

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# PLUGINS
source ~/.zplug/init.zsh

zplug "zsh-users/zsh-autosuggestions"
zplug "zsh-users/zsh-syntax-highlighting"

# Install plugins if there are plugins that have not been installed
if ! zplug check --verbose; then
    printf "Install? [y/N]: "
    if read -q; then
        echo; zplug install
    fi
fi

# Then, source plugins and add commands to $PATH
zplug load

# ALIASES
source $HOME/.zsh.aliases

# broot
source /home/matthew/.config/broot/launcher/bash/br

# autojump
[[ -s /home/matthew/.autojump/etc/profile.d/autojump.sh ]] && source /home/matthew/.autojump/etc/profile.d/autojump.sh

# enable vimode
bindkey -v
