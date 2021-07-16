local g = vim.g

vim.cmd 'highlight NvimTreeFolderIcon guibg=blue'

g.nvim_tree_git_hl = 1
g.nvim_tree_width = 40
g.nvim_tree_follow = 1
g.nvim_tree_gitignore = 1
g.nvim_tree_auto_open = 1
g.nvim_tree_auto_close = 1
g.nvim_tree_indent_markers = 1
g.nvim_tree_highlight_opened_files = 1
g.nvim_tree_auto_ignore_ft = { 'dashboard' }
g.nvim_tree_ignore = { '.git', 'node_modules', '_build', '.nix-hex', '.nix-mix', '.postgres' }

vim.api.nvim_set_keymap('', '<C-n>', ':NvimTreeToggle<cr>', { noremap = true })
vim.api.nvim_set_keymap('', '<C-n>', ':NvimTreeFindFile<cr>', { noremap = true })
