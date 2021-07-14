{ pkgs, ... }:

with pkgs.vimUtils;

let
  bullets-vim = buildVimPlugin {
    name = "bullets-vim";
    src = pkgs.fetchFromGitHub {
      owner = "dkarter";
      repo = "bullets.vim";
      rev = "1.6.0";
      sha256 = "0wz3xjmz9c4a58sh5vaxy79505hdwxa553gv6l7k9qz77skk6qbm";
    };
  };

  rescript-vim = buildVimPlugin {
    name = "resript-vim";
    src = pkgs.fetchFromGitHub {
      owner = "rescript-lang";
      repo = "vim-rescript";
      rev = "b8714edb8fe5ff2b7e32ced3bdeddd31ed08b02e";
      sha256 = "0wz3xjmz9c4a58sh5vaxy79505hdwxa553gv6l7k9qz77skk6qbm";
    };
  };
in {
  programs.neovim = {
    enable = true;
    viAlias = true;
    vimAlias = true;
    vimdiffAlias = true;
    plugins = with pkgs.vimPlugins; [
      rainbow haskell-vim
      vim-elixir surround
      commentary indentLine
      elm-vim editorconfig-vim
      bullets-vim dracula-vim
      vim-nix rescript-vim
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

      if !exists('g:syntax_on')
        syntax enable
      endif

      " You can't stop me
      cmap w!! w !sudo tee %

      command! MakeTags !ctags -R .

      nnoremap <CR> :noh<CR><CR> " set highlighting off after regex

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
