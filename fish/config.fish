# theme
starship init fish | source

# start
source $HOME/.config/fish/start.fish

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
source $HOME/.config/fish/env_vars.fish

# fast node manager
fnm env | source
# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/matthew/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/matthew/.ghcup/bin $PATH
