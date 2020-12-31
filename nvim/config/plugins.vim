"PLUGINS

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  "autocmd VimEnter * PlugInstall
  autocmd VimEnter * PlugInstall | source $MYVIMRC
endif

" Specify a directory for plugins
call plug#begin('~/nvim/plugged')

" Rainbow brackets
Plug 'luochen1990/rainbow'
" better file changes
Plug 'tpope/vim-projectionist'
" Smooth scroll
Plug 'psliwka/vim-smoothie'
" Auto change html tags
Plug 'AndrewRadev/tagalong.vim'
" emmet
Plug 'mattn/emmet-vim'
" Zen mode
Plug 'junegunn/goyo.vim'
" See what keys do like in emacs
Plug 'liuchengxu/vim-which-key'
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
"auto completion
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" rust
Plug 'rust-lang/rust.vim'
" haskell
Plug 'neovimhaskell/haskell-vim'
"minimap
Plug 'wfxr/minimap.vim'
"elixir
Plug 'elixir-editors/vim-elixir'
" indent line
Plug 'Yggdroot/indentLine'
" dir vish
Plug 'justinmk/vim-dirvish'
Plug 'kristijanhusak/vim-dirvish-git'
" vim surround
Plug 'tpope/vim-surround'
" commentary
Plug 'tpope/vim-commentary'
" status-line and tabline
Plug 'liuchengxu/eleline.vim'
Plug 'pacha/vem-tabline'
" vim-easymotion
Plug 'easymotion/vim-easymotion'

call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
