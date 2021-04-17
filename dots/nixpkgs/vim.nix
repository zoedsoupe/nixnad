{ config, lib, pkgs, ... }:

let
  vim-omni = pkgs.vimUtils.buildVimPlugin {
    name = "vim-omni";
    src = pkgs.fetchFromGitHub {
      owner = "GuiLra";
      repo = "vim-omni";
      rev = "cf57c94d6cd48d23fb02655f157f25a605988361";
      sha256 = "4f9d9a433f9a337e2bd96e0d0f969cbdbb8fc3bea6919630bd0014f1da4dcb25";
    };
  };

  earthly = pkgs.vimUtils.buildVimPlugin {
    name = "earthly.vim";
    src = pkgs.fetchFromGitHub {
      owner = "earthly";
      repo = "earthly.vim";
      rev = "292c2e76785154d68066dbe0edfab0bdb06dc200";
      sha256 = "e4d07eff273e1cbd81931c407dca3c828453f79a37cc240252957e8f006c76b0";
    };
  };
in {
    programs.vim = {
    enable = true;
    settings = {
      so = 999; # cursor never leaves mid screen
      autoread = true;
      nocompatible = true;
      hidden = true; # keep multiple buffers openned
      nowrap = true; # displays long lines into one
      encoding = "utf-8";
      pumheight = 10; # popup smaller
      fileencoding = "utf-8";
      cmdheight = 1;
      ruler = true;
      splitbelow = true; # horizontal split will be below
      splitright = true; # vertical split will be right
      t_Co = 256; # support 256 colors
      concealllevel = 0; # `` on markdown and ** in org shows up
      tabstop = 2;
      shiftwidth = 2;
      smarttab = true;
      expandtab = true;
      smartindent = true;
      autoindent = true;
      lastsattus = 2; # always shows status line
      number = true;
      relativenumber = true;
      showtabline = 0; # never show tabs
      updatetime = 300;
      timeoutlen = 1000;
      incsearch = true;
      wildmenu = true;
      background = "dark";
    };
    plugins = with pkgs.vimPlugins; [
      rainbow haskell-vim
      vim-elixir surround
      commentary indentLine
      vim-omni earthly
    ];
    extraConfig = ''
    set formatoptions-=cro
    set whichwrap+=<,>,[,],h,l
    set shortmess+=c
    set wildignore+=**/node_modules/**,**/deps/**,**/_build/**

    colorscheme omni

    filetype plugin on
    filetype plugin indent on

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
