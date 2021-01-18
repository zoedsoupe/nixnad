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

set LF_ICONS "\
tw=:\
st=:\
ow=:\
dt=:\
di=:\
fi=:\
ln=:\
or=:\
ex=:\
*.clj=:\
*.css=:\
*.erl=:\
*.exs=:\
*.ex=:\
*.hs=:\
*.html=:\
*.js=:\
*.json=:\
*.lua=:\
*.md=:\
*.rb=:\
*.rs=:\
*.ts=:\
*.vim=:\
*.sh=:\
*.bash=:\
*.fish=:\
*.tar=:\
*.zip=:\
*.gz=:\
*.lz=:\
*.xz=:\
*.bz2=:\
*.bz=:\
*.rar=:\
*.7z=:\
*.jpg=:\
*.jpeg=:\
*.gif=:\
*.xbm=:\
*.xpm=:\
*.png=:\
*.svg=:\
*.pcx=:\
*.mov=:\
*.mpeg=:\
*.webm=:\
*.mp4=:\
*.qt=:\
*.flac=:\
*.mp3=:\
*.mpc=:\
*.ogg=:\
*.wav=:\
*.pdf="
