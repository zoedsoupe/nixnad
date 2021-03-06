#!/bin/fish

# Helpers variables
set fish_config_path $HOME/.config/fish/config.fish
set fish_var_file $HOME/.config/fish/env_vars.fish

function die --on-event command-failed
    exit
end

function info
    set_color blue
    echo $argv
    set_color normal
end

function failure
    set_color red
    echo $argv
    set_color normal
end

function warning
    set_color yellow
    echo $argv
    set_color normal
end

function success
    set_color green
    echo $argv
    set_color normal
end

function header
    info "$argv"
end

function is_installed
    pacman -Qi $argv >/dev/null 2>&1
end

function install
    if not is_installed $argv
        sudo aura -S $argv
    else
        success "$argv is already installed!"
    end
end

function install_aur
    if not is_installed $argv
        sudo aura -A $argv
    else
        success "$argv is already installed!"
    end
end

header "Welcome! Let's start setting up your system xD"
header "It could take more than 10 minutes, be patient"

header "Upgrading and installing base-devel group"
sudo pacman -Syyu && sudo pacman -S base-devel

header "Installing git"
install git

header "Generating a SSH Key"
echo "What email do you want to use in GIT user.email?"
echo "For example, mine will be \"matheus_pessanha2001@outlook.com\""
read git_config_user_email

ssh-keygen -t ed25519 -C $git_config_user_email
eval (ssh-agent -c)
set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK
set -Ux SSH_AGENT_PID $SSH_AGENT_PID
set -Ux SSH_AUTH_SOCK $SSH_AUTH_SOCK

echo "What's the name of your private key?"
read ssh_key

ssh-add $HOME/.ssh/$ssh_key

header "Installing github CLI"
install github-cli
gh auth login

header "Installing aura package manager"
if not is_installed aura-bin
    git clone https://aur.archlinux.org/aura-bin.git
    cd aura-bin && makepkg -si && cd .. && rm -rf aura-bin
else
    success "Aura is already installed!"
end

header "Installing flameshot"
install flameshot

header "Installing zathura"
install zathura zathura-pdf-mupdf

header "Installing mpv"
install mpv

header "Installing cURL"
install curl

header "Installing nnn"
install nnn

header "Installing microsoft-edge-dev"
install_aur microsoft-edge-dev

header "Installing ncdu"
install ncdu

header "Installing neofetch"
install neofetch

header "Installing alacritty"
install alacritty

header "Installing tool to handle clipboard via CLI"
install xclip

header "Installing dunst"
install dunst

header "Installing picom"
install picom

header "Installing beekeeper studio"
install_aur beekeeper-studio-bin

header "Installing lazygit"
install_aur lazygit-bin

header "Instaling asdf"
if not ls $HOME/.asdf/ >/dev/null 2>&1
    gh clone asdf-vm/asdf ~/.asdf
    cd ~/.asdf
    git checkout (git describe --abbrev=0 --tags)
    source ~/.asdf/asdf.fish
    mkdir -p ~/.config/fish/completions; and cp ~/.asdf/completions/asdf.fish ~/.config/fish/completions
    echo "source ~/.asdf/asdf.fish" >>$HOME/.config/fish/config.fish
else
    success " You already have asdf! Moving on..."
end

header "Installing erlang"
if not erlc -help >/dev/null 2>&1
    header "Installing some erlang build dependencies..."
    sudo aura -S unixodbc libssh glu mesa wxgtk2 libpng ncurses

    asdf plugin-add erlang
    KERL_BUILD_DOCS=yes KERL_CONFIGURE_OPTIONS="--without-javac" asdf install erlang latest
    asdf globall erlang latest
else
    success "Oh, Ericsson thanks you!"
end

header "Installing elixir"
if not elixir -v >/dev/null 2>&1
    asdf plugin-add elixir
    asdf install elixir latest
    asdf globall elixir latest
else
    success "You're a alchemist already! Good!"
end

header "Installing rust"
if not rustc -h >/dev/null 2>&1
    asdf plugin-add rust
    asdf install rust stable
    asdf globall rust stable

    echo "set PATH $HOME/.cargo/bin $PATH" >>$fish_var_file
else
    success "Wow, you're a crustacean already!"
end

header "Installing stable version of node"
asdf plugin-add nodejs
asdf install nodejs 12.20.1
asdf globall nodejs 12.20.1

header "Installing 'stable' version of haskell"
asdf plugin-add haskell
asdf install haskell 8.10.3
asdf global haskell 8.10.3

header "Updating asdf binaries..."
asdf reshim

header "Instaling xmonad"
if not xmonad --help >/dev/null 2>&1
    stack install xmonad xmonad-contrib
    sudo ln -s $HOME/.asdf/installs/haskell/8.10.3/bin/xmonad /usr/bin/xmonad
else
    success "xmonad already exists..."
end

header "Installing xmobar"
if not xmobar --help >/dev/null 2>&1
    stack install xmonad
    sudo ln -s $HOME/.asdf/installs/haskell/8.10.3/bin/xmobar /usr/bin/xmobar
else
    success "Yeah, you're already have xmobar!"
end

header "Installing google cloud sdk"
install_aur google-cloud-sdk
gcloud init

header "Installing latest version of Phoenix"
mix archive.install hex phx_new

header "Installing exa"
install exa

header "Installing glow"
install_aur glow-bin

header "Installing git-delta"
install_aur git-delta-bin

header "Installing procs"
install procs

header "Installing fd"
install fd

header "Installing startship prompt"
if not startship -V >/dev/null 2>&1
    curl -fsSL https://starship.rs/install.sh | bash
    echo "starship init fish | source" >>$fish_config_path
else
    success "starship already exists"
end

header "Installing yarn"
install yarn
echo "set PATH $HOME/.yarn/bin $PATH" >>$fish_var_file

header "Installing typescript"
yarn global add typescript ts-node ts-node-dev

header "Installing create-react-app"
yarn global add create-react-app

header "Installing firebase tools"
yarn global add firebase-tools

header "Installing eslint and prettier"
yarn global add eslint@latest prettier

header "Installing docker"
install -S docker

if not sudo systemctl start docker >/dev/null 2>&1
    warning "Not on systemd, please, enable docker service manually!"
else
    sudo systemctl enable docker
end

header "Adding you to the docker group!"
sudo groupadd docker >/dev/null
sudo usermod -aG docker $USER

warning "Log out and log in again to be able to run docker without sudo"

header "Installing insomnia"
install_aur insomnia-bin

header "Cloning omni theme for insomnia"
warning "Dont't forget to move to ~/.config/Insomnia/plugins/"
gh repo clone Rocketseat/insomnia-omni ~/insomnia-omni

header "Installing spotify"
install_aur spotify

header "Installing telegram"
install telegram-desktop

header "Installing qbittorent"
install qbittorrent

header "Installing discord"
install discord

header "I'll link some pre configured settings!"
elixir ./installer.exs

header "Let's set up git!"

echo "What name do you want to use in GIT user.name?"
echo "For example, mine will be \"Matheus de Souza Pessanha\""
read git_config_user_name

git config --global user.name $git_config_user_name
git config --global user.email $git_config_user_email

echo "Can I set Vim as your default GIT editor for you? (y/n)"
read git_core_editor_to_vim

if echo $git_core_editor_to_vim | grep -iq "^y"
    git config --global core.editor vim
else
    echo "Okay, no problem. :) Let's move on!"
end

success "All done! Enjoy now!"
