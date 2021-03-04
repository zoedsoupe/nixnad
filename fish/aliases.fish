# elixir and phoenix
alias phx_api="mix phx.new --no-html --no-webpack --binary-id $argv"
alias mes="mix ecto.setup"
alias megm="mix ecto.gen.migration $argv"
alias mem="mix ecto.migrate"
alias mdg="mix deps.get"
alias mdc="mix deps.compile"
alias mpr="mix phx.routes"
alias mpn="mix phx.new $argv"
alias mpgh="mix phx.gen.html $argv"
alias mpgc="mix phx.gen.context $argv"
alias mpgj="mix phx.gen.json $argv"
alias mpgs="mix phx.gen.schema $argv"
alias ies="iex -S mix"
alias mps="mix phx.server"

# git
alias gt="git status"
alias ga="git add ."
alias gp="git push origin (gb --show-current)"
alias gpr="git pull.rebase origin $argv"
alias gc="git commit -m $argv"
alias co="git checkout $argv"
alias gb="git branch $argv"
alias gac="ga && gc"

# docker
alias aws="docker run --rm -it -v ~/.aws:/root/.aws amazon/aws-cli"
alias psql="docker exec -it 2df73117f099 psql $argv"
alias pg_isready="docker exec -it 2df73117f099 pg_isready $argv"
alias start_postgres="docker start 2df73117f099"

# replace software
alias lg="lazygit"
alias ps="procs"
alias top="ytop"
alias ls="exa -l"
alias cheat="tldr $argv"
alias prettyjson="python -m json.tool | bat"
alias d="rm -rf $argv"

# system health
alias node_modules="find . -name "node_modules" -type d -prune -exec rm -rf '{}' +"

# screens
alias one_screen="xrandr --output eDP-1 --auto --output HDMI-1 --off"
alias hdmi_on="xrandr --output eDP-1  --auto --output HDMI-1 --primary --auto --left-of eDP-1"
alias vga_on="xrandr --output eDP-1 --auto --output DP-1 --auto --above eDP-1"

# system
alias update="sudo aura -Syyu && sudo aura -Ayyu"
alias please="sudo $argv"
alias fsource="source ~/.config/fish/config.fish"
