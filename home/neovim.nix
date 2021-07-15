{ pkgs, lib, ... }:

let
  plugins = pkgs.vimPlugins // pkgs.callPackage ./custom-neovim-plugins.nix {};
in 
{
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with plugins; 
    [
      vim-rescript
      haskell-vim
      vim-elixir 
      indentLine
      elm-vim 
      editorconfig-vim
      dracula-vim
      vim-nix 
      surround
      commentary
      rainbow
      delimit-mate
      bullets-vim
    ];
    extraConfig = ''
      set hidden
      set tabstop=2
      set shiftwidth=2
      set number
      set relativenumber
      set background=dark
      set formatoptions-=cro
      set autoindent
      set nofoldenable
      set autoread
      set cmdheight=1
      set whichwrap+=<,>,[,],h,l
      set shortmess+=c
      set wildignore+=**/node_modules/**,**/deps/**,**/_build/**
      set conceallevel=0 " `` on markdown and ** in org shows up
      set so=999 " cursor never leaves mid screen
      set nocompatible
      set nowrap " displays long lines into one
      set encoding=utf-8
      set pumheight=10 " popup smaller
      set fileencoding=utf-8"
      set ruler
      set splitbelow " horizontal split will be below
      set splitright " vertical split will be right
      set t_Co=256 " support 256 colors
      set showtabline=0 " never show tabs
      set updatetime=300
      set timeoutlen=1000
      set incsearch
      set wildmenu
      set smarttab
      set expandtab
      set smartindent
      set laststatus=2 " always shows status line

      filetype plugin on
      filetype plugin indent on

      colorscheme dracula

      au! BufWritePost $MYVIMRC source % " auto source when writing to init.vm alternatively you can run :source $MYVIMRC
      autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

      " force syntax highlighting for large files
      autocmd BufEnter *.{ex,exs,ts,tsx} :syntax sync fromstart
      autocmd BufLeave *.{ex,exs,ts,tsx} :syntax sync clear

      let maplocalleader="\\"

      if !exists('g:syntax_on')
        syntax enable
      endif

      " You can't stop me
      cmap w!! w !sudo tee %

      command! MakeTags !ctags -R .

      nnoremap <CR> :noh<CR><CR> " set highlighting off after regex

      let g:bullets_enabled_file_types = [
        \ 'markdown',
        \ 'text',
        \ 'gitcommit',
        \ 'scratch'
        \]

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
    '';
  };
}
