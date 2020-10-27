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
" Start Screen
Plug 'mhinz/vim-startify'
" Have the file system follow you around
Plug 'airblade/vim-rooter'
" Rainbow brackets
Plug 'luochen1990/rainbow'
" Markdown Preview
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
" Smooth scroll
Plug 'psliwka/vim-smoothie'
" Find and replace
"Plug 'ChristianChiarulli/far.vim'
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
" FZF
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
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
"useful snippets for html
Plug 'mattn/emmet-vim'
"git plugins
Plug 'mhinz/vim-signify'
Plug 'tpope/vim-fugitive'
Plug 'airblade/vim-gitgutter'
Plug 'tpope/vim-rhubarb'
Plug 'junegunn/gv.vim'
Plug 'rhysd/git-messenger.vim'
"self snippets
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
"simple status line
Plug 'itchyny/lightline.vim'
"nerdtree icons
Plug 'ryanoasis/vim-devicons'
"live server
"init :Bracey
Plug 'turbio/bracey.vim' 
"colorschemes
"Plug 'dracula/vim', {'as': 'dracula'}
"Plug 'tomasiser/vim-code-dark'
"Plug 'GuiLra/vim-omni', {'as': 'omni'}
"Plug 'danilo-augusto/vim-afterglow'
Plug 'owickstrom/vim-colors-paramount'
" pretty format
"Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
"auto completion and linter
"Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" file manager integration
Plug 'ptzz/lf.vim'
Plug 'rbgrouleff/bclose.vim'
"elixir
"Plug 'slashmili/alchemist.vim'

call plug#end()

" Automatically install missing plugins on startup
autocmd VimEnter *
  \  if len(filter(values(g:plugs), '!isdirectory(v:val.dir)'))
  \|   PlugInstall --sync | q
  \| endif
