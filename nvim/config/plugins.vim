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
" Zen mode
Plug 'junegunn/goyo.vim'
" See what keys do like in emacs
Plug 'liuchengxu/vim-which-key'
" Auto pairs for '(' '[' '{'
Plug 'jiangmiao/auto-pairs'
" Closetags
Plug 'alvan/vim-closetag'
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
"colorschemes
Plug 'GuiLra/vim-omni', {'as': 'omni'}
"Plug 'srcery-colors/srcery-vim'
"Plug 'owickstrom/vim-colors-paramount'
"auto completion
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'pbogut/deoplete-elm'
Plug 'Shougo/neco-vim'
"minimap
Plug 'wfxr/minimap.vim'
"elixir
Plug 'elixir-editors/vim-elixir'
Plug 'slashmili/alchemist.vim'
Plug 'mhinz/vim-mix-format'
" pretty format
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
" automatically add end
Plug 'tpope/vim-endwise'
" indent line
Plug 'Yggdroot/indentLine'
" dir vish
Plug 'justinmk/vim-dirvish'
Plug 'kristijanhusak/vim-dirvish-git'
Plug 'tpope/vim-surround'

call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
