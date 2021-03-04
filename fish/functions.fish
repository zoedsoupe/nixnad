function export
    set params (string split -m 1 = $argv)
    set -gx $params[1] $params[2]
end

function dbus
  for var in (dbus-launch)
    export $var
  end
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

function fcode
    command grep -rnw . -e $argv --color=always \
        --exclude-dir={\.git,\.github,node_modules,_build,deps,\.elixir_ls}
end

function pandoc
    set path (pwd)

    docker run --rm --volume "$path:/data" \
        --user (id -u):(id -g) pandoc/core $argv
end
