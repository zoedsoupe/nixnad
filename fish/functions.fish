function setenv
    set -gx $argv
end

function tre
    command tree -aC \
        -I '.git|node_modules|deps|_build|.elixir_ls' \
        --dirsfirst $argv | bat
end

function cd
    builtin cd $argv && ls -a .
end

function mkd
    command mkdir -p $argv && cd $argv
end

function emacs
    command emacsclient -c -a emacs $argv &
end
