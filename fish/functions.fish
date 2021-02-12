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

function fcode
    command grep -rnw . -e $argv \
        --exclude-dir '.git|.github|node_modules|_build|deps|.elixir_ls'
end

function pandoc
    set path (pwd)

    docker run --rm --volume "$path:/data" \
        --user (id -u):(id -g) pandoc/core $argv
end
