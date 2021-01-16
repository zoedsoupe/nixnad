"PLUGINS

if &compatible
  set nocompatible
endif

set runtimepath+=/home/matthew/.config/dein/repos/github.com/Shougo/dein.vim

if dein#load_state('/home/matthew/.config/dein')
  call dein#begin('/home/matthew/.config/dein')

  call dein#add('/home/matthew/.config/dein/repos/github.com/Shougo/dein.vim')

    " dein ui
    call dein#add('wsdjeg/dein-ui.vim')
      " Rainbow brackets
    call dein#add('luochen1990/rainbow') 
    " better file changes
    call dein#add('tpope/vim-projectionist')
    " Smooth scroll
    call dein#add('psliwka/vim-smoothie')
    " Auto change html tags
    call dein#add('AndrewRadev/tagalong.vim') 
    " emmet
    call dein#add('mattn/emmet-vim') 
    " Zen mode
    call dein#add('junegunn/goyo.vim') 
    " See what keys do like in emacs
    call dein#add('liuchengxu/vim-which-key') 
    " Colorizer
    call dein#add('norcalli/nvim-colorizer.lua')
    "syntax highlighting
    call dein#add('nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'})
    "git plugins
    call dein#add('mhinz/vim-signify') 
    call dein#add('tpope/vim-fugitive') 
    call dein#add('airblade/vim-gitgutter')
    call dein#add('tpope/vim-rhubarb') 
    call dein#add('junegunn/gv.vim') 
    call dein#add('rhysd/git-messenger.vim') 
    "colorschemes
    call dein#add('GuiLra/vim-omni', {'as': 'omni'}) 
    call dein#add('bluz71/vim-moonfly-colors')
    call dein#add('challenger-deep-theme/vim')
    " call dein#add('sjl/badwolf')
    " call dein#add('axvr/photon.vim') 
    "auto completion
    call dein#add('neoclide/coc.nvim', { 'merged': 0 })
    " earthly syntax
    call dein#add('earthly/earthly.vim', { 'branch': 'main'})
    " haskell
    call dein#add('neovimhaskell/haskell-vim') 
    "minimap
    call dein#add('wfxr/minimap.vim') 
    "elixir
    call dein#add('elixir-editors/vim-elixir') 
    " purescript
    call dein#add('purescript-contrib/purescript-vim')
    " indent line
    call dein#add('Yggdroot/indentLine') 
    " dir vish
    call dein#add('justinmk/vim-dirvish') 
    call dein#add('kristijanhusak/vim-dirvish-git') 
    " vim surround
    call dein#add('tpope/vim-surround') 
    " commentary
    call dein#add('tpope/vim-commentary') 
    " status-line and tabline
    call dein#add('bluz71/vim-moonfly-statusline') 
    call dein#add('akinsho/nvim-bufferline.lua')
    call dein#add('kyazdani42/nvim-web-devicons')
    " vim-easymotion
    call dein#add('easymotion/vim-easymotion') 
    " markdown preview
    call dein#add('iamcco/markdown-preview.nvim', {'on_ft': ['markdown', 'pandoc.markdown', 'rmd'],
              \ 'build': 'sh -c "cd app && yarn install"' })
    " ide like
    cal dein#add('pechorin/any-jump.vim')

  call dein#end()
  call dein#save_state()
endif

" If you want to install not installed plugins on startup.
if dein#check_install()
  call dein#install()
endif

" Remove unused plugins
call map(dein#check_clean(), "delete(v:val, 'rf')")
let g:dein#auto_recache = 1
