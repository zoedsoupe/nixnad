local set = vim.opt
local wo = vim.wo
local g = vim.g
local cmd = vim.cmd
local env = vim.env

g.mapleader = ','
g.maplocalleader = '\\'

 -- my custom options
set.hidden = true
set.tabstop = 2
set.shiftwidth = 2
set.number = true
set.relativenumber = true
set.background = 'dark'
set.formatoptions:remove({ 'cro' })
set.autoindent = true
set.autoread = true
set.cmdheight = 1
set.whichwrap:append({ '<', '>', '[', ']', 'h', 'l' })
set.shortmess:append({ 'c' })
set.wildignore:append({ '**/node_modules/**', '**/deps/**', '**/_build/**' })
set.conceallevel = 0 -- `` on markdown and ** in org shows up
set.so = 999 -- cursor never leaves mid screen
set.nocompatible = true
set.nowrap = true
set.encoding = 'utf-8'
set.fileencoding = 'utf-8'
set.pumheight = 10 -- popup smaller
set.ruler = true
set.splitbelow = true -- horizontal split will be below
set.splitright = true -- vertical split will be to the right
set.t_Co = 256 -- support 256 colors
set.showtabline = 0 -- never show tabs
set.updatetime = 300
set.timeoutlen = 1000
set.incsearch = true
set.wildmenu = true
set.smarttab = true
set.expandtab = true
set.smartindent = true
set.laststatus = 2 -- always shoes status line
set.ignorecase = true -- make searches with lower case characters case insensative
set.smartcase  = true -- search is case sensitive only if it contains uppercase chars

-- load no config plugins
require('orgmode').setup()
require('gitsigns').setup()
require('colorizer').setup()
require('nvim-web-devicons').setup({ default = true; })

-- telescope keybindings
vim.api.nvim_set_keymap('', '<leader>ff', ':Telescpope find_files<cr>', { norepmap = true })
vim.api.nvim_set_keymap('', '<leader>fg', ':Telescpope live_grep<cr>', { norepmap = true })
vim.api.nvim_set_keymap('', '<leader>fb', ':Telescpope buffers<cr>', { norepmap = true })
vim.api.nvim_set_keymap('', '<leader>fh', ':Telescpope help_tags<cr>', { norepmap = true })

-- small plugins config and extra config
g.rainbow_active = 1
g.direnv_silent_load = 1
g.dashboard_default_executive = 'telescope'
g.bullets_enabled_file_types = { 'markdown', 'text', 'gitcommit', 'orgmode', 'scratch' }

cmd 'colorscheme dracula'
cmd 'filetype plugin on'
cmd 'filetype plugin indent on'

if g.syntax_on then
  cmd 'syntax enable'
end

-- true colors
--g.'\&t_8f' = '\<Esc>[38;2;%lu;%lu;%lum'
--g.'\&t_8b' = '\<Esc>[48;2;%lu;%lu;%lum'
set.termguicolors = true

vim.api.nvim_set_keymap('', '<cr>', [[:noh<cr><cr>]], { noremap = true })

-- you can't stop me
vim.api.nvim_set_keymap('c', 'w!!', [[!sudo tee %]])
