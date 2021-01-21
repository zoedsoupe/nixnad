
" _ __ ___   __| |___ _ __
"| '_ ` _ \ / _` / __| '_ \
"| | | | | | (_| \__ \ |_) |
"|_| |_| |_|\__,_|___/ .__/
"                    |_|

source $HOME/.config/nvim/config/plugins.vim
source $HOME/.config/nvim/config/coc.vim
source $HOME/.config/nvim/config/leader-key.vim
source $HOME/.config/nvim/config/emmet.vim
source $HOME/.config/nvim/config/bufferline.vim
source $HOME/.config/nvim/config/ident.vim
source $HOME/.config/nvim/config/minimap.vim 
source $HOME/.config/nvim/config/keymaps.vim
source $HOME/.config/nvim/config/rainbow.vim
source $HOME/.config/nvim/config/closetags.vim
source $HOME/.config/nvim/config/colorscheme.vim
source $HOME/.config/nvim/config/functions.vim
source $HOME/.config/nvim/config/settings.vim
source $HOME/.config/nvim/config/which-key.vim

lua <<EOF
require'colorizer'.setup()

require'bufferline'.setup{
  options = {
    separator_style = 'thin',
    mappings = false,
  }
}

require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
  }
}
EOF
