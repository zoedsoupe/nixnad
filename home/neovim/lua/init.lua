local set = vim.opt
local g = vim.g
local cmd = vim.cmd

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
set.shortmess:append({ c = true })
set.wildignore:append({ '**/node_modules/**', '**/deps/**', '**/_build/**' })
set.conceallevel = 0 -- `` on markdown and ** in org shows up
set.so = 999 -- cursor never leaves mid screen
set.encoding = 'utf-8'
set.fileencoding = 'utf-8'
set.pumheight = 10 -- popup smaller
set.ruler = true
set.splitbelow = true -- horizontal split will be below
set.splitright = true -- vertical split will be to the right
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


-- true colors
vim.api.nvim_set_var('&t_8f', '\\<Esc>[38;2;%lu;%lu;%lum')
vim.api.nvim_set_var('&t_8b', '\\<Esc>[48;2;%lu;%lu;%lum')
set.termguicolors = true

-- load no config plugins
--require('orgmode').setup()
require('gitsigns').setup()
require('colorizer').setup()
require('nvim-web-devicons').setup({ default = true; })

-- telescope keybindings
vim.api.nvim_set_keymap('', '<leader>ff', ':Telescope find_files<cr>', { noremap = true })
vim.api.nvim_set_keymap('', '<leader>fg', ':Telescope live_grep<cr>', { noremap = true })
vim.api.nvim_set_keymap('', '<leader>fb', ':Telescope buffers<cr>', { noremap = true })
vim.api.nvim_set_keymap('', '<leader>fh', ':Telescope help_tags<cr>', { noremap = true })

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

vim.api.nvim_set_keymap('', '<cr>', [[:noh<cr><cr>]], { noremap = true })

-- you can't stop me
vim.api.nvim_set_keymap('c', 'w!!', [[!sudo tee %]], { noremap = true })
