# _ __ ___   __| |___ _ __  
#| '_ ` _ \ / _` / __| '_ \ 
#| | | | | | (_| \__ \ |_) | 
#|_| |_| |_|\__,_|___/ .__/ 
#                    |_|    

### PROMPT ###
starship init fish | source

# SHELL THEME
source $HOME/.config/fish/omni.fish

### AUTOJUMP ###
begin
    set --local AUTOJUMP_PATH $HOME/.autojump/share/autojump/autojump.fish
    if test -e $AUTOJUMP_PATH
        source $AUTOJUMP_PATH
    end
end

### VI MODE ###
#fish_vi_key_bindings

### ALIASES ###
source $HOME/.config/fish/aliases.fish 

### FUNCTIONS ###
source $HOME/.config/fish/functions.fish

### ENV VARS ###
source $HOME/.config/fish/env_vars.fish

### ASDF ###
source ~/.asdf/asdf.fish
