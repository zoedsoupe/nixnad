# path
set PATH $HOME/bin /usr/local/bin \
  $HOME/.ebcli-virtual-env/executables \
  $HOME/.yarn/bin \
  $HOME/.config/yarn/global/node_modules/.bin \
  $HOME/.local/bin ~/scripts/ ~/.cargo/bin \
  $HOME/.asdf/installs/rust/nightly/bin \
  $HOME/.asdf/installs/haskell/8.6.5/bin \
  $HOME/.pyenv/versions/3.7.2/bin $PATH

# remove fish greeting
set fish_greeting

# starship theme config
set STARSHIP_CONFIG ~/Documents/privy/dotfiles/startship.toml

# default apps
set EDITOR nvim
set READER zathura
set VISUAL nvim
set TERMINAL kitty
set BROWSER microsoft-edge-dev
set VIDEO mpv
set IMAGE feh
set COLORTERM truecolor
set OPENER xdg-open

set HISTCONTROL ignoreboth

# add erlang docs every time it's installed via asdf
set KERL_BUILD_DOCS yes
