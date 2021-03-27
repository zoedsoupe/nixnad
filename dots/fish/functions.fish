function tre
    command tree -aC \
        -I '.git|.github|node_modules|deps|_build|.elixir_ls' \
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
        --exclude-dir={\.git,\.github,node_modules,_build,deps,\.elixir_ls,\.straight}
end

function pandoc
    set path (pwd)

    docker run --rm --volume "$path:/data" \
        --user (id -u):(id -g) pandoc/core $argv
end

function clean_node_modules
    command find . -name "node_modules" -type d -prune -exec rm -rf '{}' +
end

function one_screen
    if xrandr | grep -q 'HDMI1 connected'
        xrandr --output eDP1 --auto --output HDMI1 --off
    else if xrandr | grep -q 'DP1 connected'
        xrandr --output eDP1 --auto --output DP1 --off
    end
end

function hdmi_on
    xrandr --output eDP1  --auto --output HDMI1 --primary --auto --left-of eDP1
end

function vga_on
    xrandr --output eDP1 --auto --output DP1 --primary --auto --left-of eDP1
end
