neofetch
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
 ZSH_THEME="spaceship"

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to enable command auto-correction.
ENABLE_CORRECTION="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git zsh-autosuggestions zsh-syntax-highlighting)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='nvim'
 else
   export EDITOR='nvim'
 fi

bindkey -v

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.

alias icat="kitty +kitten icat"
alias d="kitty +kitten diff"
alias vs="code ."
alias gt="git status"
alias ga="git add ."
alias gp="git push origin master"
alias gc="git commit -m "
alias co="git checkout "
alias gb="git branch "
alias clean="yay -Rcns $(yay -Qqtd)"
alias cheat="tldr $@"
alias node_modules="find . -name "node_modules" -type d -prune -exec rm -rf '{}' +"
alias cat="ccat"
alias zsource="source ~/.zshrc"
alias one_screen="xrandr --output eDP-1 --auto --output HDMI-1 --off"
alias hdmi_on="xrandr --output eDP-1  --auto --output HDMI-1 --primary --auto --left-of eDP-1"
alias vga_on="xrandr --output eDP-1 --auto --output DP-1 --auto --above eDP-1"
alias update="yay -Syyu"
alias prettyjson="python -m json.tool | cat"
alias phx_api="mix phx.new --no-html --no-webpack --binary-id $@"
alias mes="mix ecto.setup"
alias megm="mix ecto.gen.migration $@"
alias mem="mix ecto.migrate"
alias mdg="mix deps.get"
alias mdc="mix deps.compile"
alias mpgc="mix phx.gen.context $@"
alias mpgj="mix phx.gen.json $@"
alias mpgs="mix phx.gen.schema $@"
alias ies="iex -S mix"
alias mps="mix phx.server"
alias lg="lazygit"
alias ps="procs"
alias top="ytop"
alias ls="exa -l"
alias start_postgres="docker start cef2d0d02474"

# remove o username
DEFAULT_USER=`whoami`
fpath=($fpath "/home/mdsp/.zfunctions")

  # Set Spaceship ZSH as a prompt
  autoload -U promptinit; promptinit
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")

SPACESHIP_PROMPT_ORDER=(
  user          # Username section
  #dir           # Current directory section
  #host          # Hostname section
  #vi_mode       # Vi-mode indicator
  git            # Git section (git_branch + git_status)
  node           # node version
  exec_time     # Execution time
  line_sep      # Line break
  char
  jobs          # Background jobs indicator
  exit_code     # Exit code section
  #char          # Prompt character
)

# USER
SPACESHIP_USER_SHOW=always

# PROMPT
SPACESHIP_PROMPT_ADD_NEWLINE=true

# CHAR
SPACESHIP_CHAR_SYMBOL="ﬦ "

# DIR
#SPACESHIP_DIR_PREFIX="("
#SPACESHIP_DIR_SUFFIX=") "

# GIT
SPACESHIP_GIT_PREFIX="("
SPACESHIP_GIT_SUFFIX=") "

# NODE
SPACESHIP_NODE_SHOW=always
SPACESHIP_NODE_PREFIX="("
SPACESHIP_NODE_SUFFIX=") "
SPACESHIP_NODE_SYMBOL="⬢ "
SPACESHIP_NODE_DEFAULT_VERSION="(node -v)"
SPACESHIP_NODE_COLOR="green"

# DOCKER
SPACESHIP_DOCKER_SHOW=always
SPACESHIP_DOCKER_PREFIX="("
SPACESHIP_DOCKER_SUFFIX=") "
SPACESHIP_DOCKER_SYMBOL=" "
SPACESHIP_DOCKER_COLOR="cyan"

#ELIXIR
SPACESHIP_ELIXIR_SHOW=always
SPACESHIP_ELIXIR_PREFIX="("
SPACESHIP_ELIXIR_SUFIX=") "
SPACESHIP_ELIXIR_SYMBOL=""
SPACESHIP_ELIXIR_DEFAULT_VERSION="1.10"

# VI MODE
SPACESHIP_VI_MODE_INSERT="<I>"
SPACESHIP_VI_MODE_NORMAL="<N>"
SPACESHIP_VI_MODE_VISUAL="<V>"
SPACESHIP_VI_MODE_COLOR="yellow"

### Added by Zinit's installer
if [[ ! -f $HOME/.zinit/bin/zinit.zsh ]]; then
    print -P "%F{33}▓▒░ %F{220}Installing %F{33}DHARMA%F{220} Initiative Plugin Manager (%F{33}zdharma/zinit%F{220})…%f"
    command mkdir -p "$HOME/.zinit" && command chmod g-rwX "$HOME/.zinit"
    command git clone https://github.com/zdharma/zinit "$HOME/.zinit/bin" && \
        print -P "%F{33}▓▒░ %F{34}Installation successful.%f%b" || \
        print -P "%F{160}▓▒░ The clone has failed.%f%b"
fi

source "$HOME/.zinit/bin/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zinit-zsh/z-a-patch-dl \
    zinit-zsh/z-a-as-monitor \
    zinit-zsh/z-a-bin-gem-node

### End of Zinit's installer chunk

zinit light zdharma/fast-syntax-highlighting
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions

export PATH
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")
fpath=($fpath "/home/mdsp/.zfunctions")

fpath=(~/.zsh $fpath)
### End of Zinit's installer chunk

autoload -Uz compinit

zstyle ':completion:*' menu select
fpath+=~/.zfunc

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/home/mdsp/google-cloud-sdk/path.zsh.inc' ]; then . '/home/mdsp/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/home/mdsp/google-cloud-sdk/completion.zsh.inc' ]; then . '/home/mdsp/google-cloud-sdk/completion.zsh.inc'; fi
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"

# fnm

source /home/matthew/.config/broot/launcher/bash/br

# autojump
[[ -s /home/matthew/.autojump/etc/profile.d/autojump.sh ]] && source /home/matthew/.autojump/etc/profile.d/autojump.sh
