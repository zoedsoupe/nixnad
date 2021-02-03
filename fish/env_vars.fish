### PATH ###
set PATH $HOME/bin /usr/local/bin \
    $HOME/.ebcli-virtual-env/executables \
    $HOME/.yarn/bin \
    $HOME/.emacs.d/bin \
    $HOME/.config/yarn/global/node_modules/.bin \
    $HOME/.local/bin ~/scripts/ ~/.cargo/bin \
    $HOME/.asdf/installs/rust/nightly/bin \
    $HOME/.asdf/installs/haskell/8.8.1/bin $PATH

set fish_greeting # suppress fish initital greeting

### AUTOCOMPLETE AND HIGHLIGHT COLORS ###
set fish_color_normal brcyan
set fish_color_autosuggestion '#7d7d7d'
set fish_color_command brcyan
set fish_color_error '#ff6c6b'
set fish_color_param brcyan

### DEFAULT APPS ###
set EDITOR nvim
set READER zathura
set VISUAL nvim
set TERMINAL alacritty
set BROWSER microsoft-edge-dev
set VIDEO mpv
set IMAGE feh
set COLORTERM truecolor
set OPENER xdg-open
### END DEFAULT APPS ###

set HISTCONTROL ignoreboth # ignore commands with initial space and duplicates

set KERL_BUILD_DOCS yes # add erlang docs every time it's installed via asdf
