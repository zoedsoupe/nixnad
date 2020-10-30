# If you come from bash you might have to change your $PATH.
export PATH=$HOME/bin:/usr/local/bin:$PATH
export PATH="/home/matthew/.ebcli-virtual-env/executables:$PATH"
export PATH="$HOME/.yarn/bin:$HOME/.config/yarn/global/node_modules/.bin:$PATH"
export PATH=/home/matthew/.local/bin:$PATH
export PATH=~/scripts/:$PATH
export PATH=~/.cargo/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/matthew/.oh-my-zsh"

# Default Apps
export EDITOR="nvim"
export READER="zathura"
export VISUAL="nvim"
export TERMINAL="kitty"
export BROWSER="google-chrome-stable"
export VIDEO="mpv"
export IMAGE="feh"
export COLORTERM="truecolor"
export OPENER="xdg-open"

# fast node manager
eval "`fnm env --multi`"
