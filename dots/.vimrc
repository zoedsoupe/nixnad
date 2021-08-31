""" BEGIN BASIC EDITOR CONFIG
set formatoptions-=cro                  " Stop newline continution of comments
set autoread
set nocompatible
set hidden                              " Required to keep multiple buffers open multiple buffers
set nowrap                              " Display long lines as just one line
set whichwrap+=<,>,[,],h,l
set encoding=utf-8                      " The encoding displayed
set pumheight=10                        " Makes popup menu smaller
set fileencoding=utf-8                  " The encoding written to file
set cmdheight=1                         " More space for displaying messages
set ruler
set splitbelow                          " Horizontal splits will automatically be below
set splitright                          " Vertical splits will automatically be to the right
set t_Co=256                            " Support 256 colors
set conceallevel=0                      " So that I can see `` in markdown files
set tabstop=2                           " Insert 2 spaces for a tab
set shiftwidth=2                        " Change the number of space characters inserted for indentation
set smarttab                            " Makes tabbing smarter will realize you have 2 vs 4
set expandtab                           " Converts tabs to spaces
set smartindent                         " Makes indenting smart
set autoindent                          " Good auto indent
set laststatus=2                        " Always display the status line
set number relativenumber               " Activates relative and line number
set showtabline=0                       " never show tabs
set shortmess+=c                        " Don't pass messages to |ins-completion-menu|.
set updatetime=300                      " Faster completion
set timeoutlen=1000                      " By default timeoutlen is 1000 ms
set incsearch
set path+=**                            " Provides tab-completion for all file-related tasks
set wildmenu                            " Display all matching files when we tab complete
set wildignore+=**/node_modules/**,**/deps/**,**/_build/**
set so=999
filetype plugin on
set background=dark

au! BufWritePost $MYVIMRC source %      " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" force syntax highlighting for large files
autocmd BufEnter *.{ex,exs,ts,tsx} :syntax sync fromstart
autocmd BufLeave *.{ex,exs,ts,tsx} :syntax sync clear

if !exists('g:syntax_on')
  syntax enable
endif

" You can't stop me
cmap w!! w !sudo tee %

command! MakeTags !ctags -R .
""" END BASIC EDITOR CONFIG

""" BEGIN DEIN SCRIPTS
if &compatible
  set nocompatible               " Be iMproved
endif

" Required:
set runtimepath+=/home/matthew/.cache/dein/repos/github.com/Shougo/dein.vim

" Required:
if dein#load_state('/home/matthew/.cache/dein')
  call dein#begin('/home/matthew/.cache/dein')
  call dein#add('/home/matthew/.cache/dein/repos/github.com/Shougo/dein.vim')

  """ BEGIN PLUGINS
  call dein#add('luochen1990/rainbow') " Rainbow brackets

  """ BEGIN COLORSCHEMES
  call dein#add('GuiLra/vim-omni', {'as': 'omni'}) 
  call dein#add('bluz71/vim-moonfly-colors')
  call dein#add('challenger-deep-theme/vim')
  """ END COLORSCHEMES
 
  call dein#add('earthly/earthly.vim', { 'branch': 'main'})  " earthly syntax
  call dein#add('neovimhaskell/haskell-vim') " haskell
  call dein#add('elixir-editors/vim-elixir')  " elixir
  call dein#add('Yggdroot/indentLine') " indent line
  call dein#add('tpope/vim-surround') " vim surround
  call dein#add('tpope/vim-commentary') " commentary
  """ END PLUGINS

  " Required:
  call dein#end()
  call dein#save_state()
endif

" Required:
filetype plugin indent on
syntax enable

if dein#check_install()
  call dein#install()
endif
""" END DEIN SCRIPTS

""" BEGIN KEYMAPS
nnoremap <CR> :noh<CR><CR> " set highlighting off after regex 
""" END KEYMAPS

""" BEGIN FUNCTIONS
"Reloads vimrc after saving but keep cursor position
if !exists('*ReloadVimrc')
  fun! ReloadVimrc()
    let save_cursor = getcurpos()
    source $MYVIMRC
    call setpos(".", save_cursor)
  endfun
endif

autocmd! BufWritePost $MYVIMRC call ReloadVimrc()

" Enable true color
if exists('+termguicolors')
  let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
  let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
  set termguicolors
endif
""" END FUNCTIONS

colorscheme omni
