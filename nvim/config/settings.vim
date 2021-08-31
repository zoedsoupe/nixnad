set iskeyword+=-                      	" treat dash separated words as a word text object"
set iskeyword-=:,<,>,.,?,;,\",\',\/,%,$,#,@,!,^,&,*,(,),{,},~,[,],|
set formatoptions-=cro                  " Stop newline continution of comments

set autoread
set nocompatible
set hidden                              " Required to keep multiple buffers open multiple buffers
set nowrap                              " Display long lines as just one line
set whichwrap+=<,>,[,],h,l
set encoding=utf-8                      " The encoding displayed
set pumheight=10                        " Makes popup menu smaller
set fileencoding=utf-8                  " The encoding written to file
set ruler              			            " Show the cursor position all the time
set cmdheight=1                         " More space for displaying messages
set mouse=a                             " Enable your mouse
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
set number relativenumber
set cursorline                          " Enable highlighting of the current line
set showtabline=0                       " never show tabs
set shortmess+=c                        " Don't pass messages to |ins-completion-menu|.
set signcolumn=yes                      " Always show the signcolumn, otherwise it would shift the text each time
set updatetime=300                      " Faster completion
set timeoutlen=1000                      " By default timeoutlen is 1000 ms
set clipboard=unnamedplus               " Copy paste between vim and everything else
set incsearch
set path+=**                            " Provides tab-completion for all file-related tasks
set wildmenu                            " Display all matching files when we tab complete
set wildignore+=**/node_modules/**,**/deps/**,**/_build/**,.*
set so=999
filetype plugin on
set guicursor=
set background=dark

function! OnTermClose()
    if len(filter(range(1, bufnr('$')), 'buflisted(v:val)')) == 1
        :quit!
    else
        call feedkeys(" ")
    endif
endfunction

au TermClose * nested call OnTermClose()

" New stuff
" set notimeout nottimeout
" set scrolloff=1
" set sidescroll=1
" set sidescrolloff=1
" set display+=lastline
" set backspace=eol,start,indent
set nostartofline
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
" set mmp=1300
" set autochdir                           " Your working directory will always be the same as your working directory
" set foldcolumn=2                        " Folding abilities

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
