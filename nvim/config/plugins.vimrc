"PLUGINS

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Specify a directory for plugins
call plug#begin('~/nvim/plugged')

" wakatime
Plug 'wakatime/vim-wakatime'
" Rainbow brackets
Plug 'luochen1990/rainbow'
" Smooth scroll
Plug 'psliwka/vim-smoothie'
" Auto change html tags
Plug 'AndrewRadev/tagalong.vim'
" Better tabline
Plug 'mg979/vim-xtabline'
" airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'tonadev/vim-airline-256noir'
" Zen mode
Plug 'junegunn/goyo.vim'
" See what keys do like in emacs
Plug 'liuchengxu/vim-which-key'
" Terminal
Plug 'voldikss/vim-floaterm'
" Auto pairs for '(' '[' '{'
Plug 'jiangmiao/auto-pairs'
" Closetags
Plug 'alvan/vim-closetag'
" CodeStats plugin
Plug 'https://gitlab.com/code-stats/code-stats-vim.git'
"scratchpad
Plug 'metakirby5/codi.vim'
" Colorizer
Plug 'norcalli/nvim-colorizer.lua'
"syntax highlighting
Plug 'sheerun/vim-polyglot'
"git plugins
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-rhubarb'
Plug 'junegunn/gv.vim'
Plug 'rhysd/git-messenger.vim'
"explorer icons
" Plug 'ryanoasis/vim-devicons'
"colorschemes
"Plug 'GuiLra/vim-omni', {'as': 'omni'}
Plug 'owickstrom/vim-colors-paramount'
"auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Plug 'neoclide/coc.nvim', {'branch': 'release'}
" file manager integration
Plug 'ptzz/lf.vim'
Plug 'rbgrouleff/bclose.vim'
"minimap
Plug 'wfxr/minimap.vim'
"elixir
Plug 'slashmili/alchemist.vim'
" eslint-syntax-highlight
" pretty format
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'gmoe/vim-eslint-syntax'

call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
