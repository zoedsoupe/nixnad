# dotfiles

**Powered by:**

![](https://img.shields.io/badge/-Arch-informational?style=for-the-badge&logo=Arch-Linux&logoColor=white&color=1793D1)
![](https://img.shields.io/badge/-Xorg-informational?style=for-the-badge&logo=X.Org&logoColor=white&color=F28834)
![](https://img.shields.io/badge/-Fish-informational?style=for-the-badge&logoColor=white&color=5927E3)
![](https://img.shields.io/badge/-Vim-informational?style=for-the-badge&logo=vim&logoColor=white&color=019733)

## Dependencies

Obs: I recommend to use `asdf` as version manager to install all programming
languages dependencies

- [archlinuxsetup](https://github.com/Mdsp9070/archlinux-setup)
- Haskell (ghc + stack)
- [Elixir](https://elixir-lang.org/install.html)
- Neovim: See `scripts/neovim` to installing the nightly build
- [Starship prompt](https://starship.rs/)
- [fish](https://github.com/fish-shell/fish-shell)
- [picom](https://github.com/yshui/picom)

## Operating System

This setup could be reproduced on any GNU/Linux distribution.
It could however not be fully reproduced on macOs or Bsd systems.

## Programs

| Role  | Program |
| ------------- | ------------- |
| Display Server  | X11 ([Xorg](https://wiki.archlinux.org/index.php/Xorg)) |
| Window Manager  | [xmonad](https://xmonad.org/)  |
|RandR | [xorg-xrandr](https://www.archlinux.org/packages/?name=xorg-xrandr)|
|Compositor | [Picom](https://github.com/yshui/picom)|
| Bars | [xmobar](https://hackage.haskell.org/package/xmobar)|
|Terminal Emulator | [Alacritty](https://github.com/alacritty/alacritty)|
| Shell | [Fish](https://github.com/fish-shell/fish-shell)|
| Lockscreen | [ly](https://github.com/nullgemm/ly)|
| Wallpaper setter for X | [feh](https://wiki.archlinux.org/index.php/feh)|
| File manager (Cli) | [lf](https://github.com/gokcehan/lf/) |
| Document viewer | [Zathura](https://pwmt.org/projects/zathura/) |
| Notification daemon | [dunst](https://dunst-project.org/) |
| Text editor | [Neovim](https://neovim.io/) |

## Neovim plugins

| Plugin name | Role
| ----------- | ----
| [dein.vim](https://github.com/Shougo/dein.vim) | Plugin manager |
| [dein-ui.vim](https://github.com/wsdjeg/dein-ui.vim) | UI for dein |
| [rainbow](https://github.com/luochen1990/rainbow) | Rainbow matches (paren, brackets...) |
| [vim-projectionist](https://github.com/tpope/vim-projectionist) | Granular project configuration |
| [vim-smoothie](https://github.com/psliwka/vim-smoothie) | Smoothier vim moves |
| [tagalong.vim](https://github.com/AndrewRadev/tagalong.vim) | Auto change html tags |
| [emmet-vim](https://github.com/mattn/emmet-vim) | Emmet for vim |
| [goyo](https://github.com/junegunn/goyo.vim) | Zen mode |
| [vim-which-key](https://github.com/liuchengxu/vim-which-key) | See what keys do |
| [nvim-colorizer.lua](https://github.com/norcalli/nvim-colorizer.lua) | Colorizer |
| [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter) | Syntax highlighting |
| [vim-signify](https://github.com/mhinz/vim-signify) | Show git modifications |
| [vim-fugitive](https://github.com/tpope/vim-fugitive) | Git wrapper |
| [vim-rhubarb](https://github.com/tpope/vim-rhubarb) | Github wrapper |
| [gv.vim](https://github.com/junegunn/gv.vim) | Git commit browser |
| [git-messenger.vim](https://github.com/rhysd/git-messenger.vim) | Git hidden messages |
| [vim-omni](https://github.com/GuiLra/vim-omni/) | Omni colorscheme |
| [challenger-deep-theme](https://github.com/challenger-deep-theme/vim) | Challenger deep colorscheme |
| [coc.nvim](https://github.com/neoclide/coc.nvim) | LSP integration |
| [earthly.vim](https://github.com/earthly/earthly.vim) | Earthfile syntax highlighting |
| [haskell-vim](https://github.com/neovimhaskell/haskell-vim) | Haskell syntax highlighting and identation |
| [minimap.vim](https://github.com/wfxr/minimap.vim) | Blazzing fast minimap |
| [vim-elixir](https://github.com/elixir-editors/vim-elixir) | Support for Elixir |
| [purescript-vim](https://github.com/purescript-contrib/purescript-vim) | Support forPurescript |
| [identLine](https://github.com/Yggdroot/indentLine) | Display identation levels |
| [vim-surround](https://github.com/tpope/vim-surround) | Surround anything |
| [vim-commentary](https://github.com/tpope/vim-commentary) | Comments anything |
| [nvim-bufferline.lua](https://github.com/akinsho/nvim-bufferline.lua) | Bufferline |
| [spaceline.vim](https://github.com/glepnir/spaceline.vim) | Spaceline like spacemacs |
| [nvim-web-devicons](https://github.com/kyazdani42/nvim-web-devicons) | Icons |
| [vim-easymotion](https://github.com/easymotion/vim-easymotion) | Better motion |
| [markdown-preview.nvim](https://github.com/iamcco/markdown-preview.nvim) | Markdown Preview |
| [any-jump](https://github.com/pechorin/any-jump.vim) | Jump to any definition |

## Coc extensions

- coc-pairs
- coc-tsserver
- coc-eslint
- coc-prettier
- coc-json
- coc-css
- coc-html
- coc-markdownlint
- coc-svg
- coc-vimlsp
- coc-emoji
- coc-elixir
- coc-docker
- coc-erlang_ls
- coc-solargraph
- coc-fish

extras:

- haskell-language-server
- purescript-language-server
