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


# functions
source $HOME/.config/fish/functions.fish
# env vars
source $HOME/.config/fish/env_vars.fish

# asdf
source ~/.asdf/asdf.fish
