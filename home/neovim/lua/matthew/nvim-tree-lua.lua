local g = vim.g

vim.cmd 'highlight NvimTreeFolderIcon guibg=blue'

local config = {
   side = "left",
    show_icons = {
      git = 1,
      folders = 1,
      files = 1,
      folder_arrows = 1,
      tree_width = 30,
    },
    ignore = { '.git', 'node_modules', '_build', '.nix-hex', '.nix-mix', '.postgres' },
    auto_open = 1,
    auto_close = 1,
    quit_on_open = 0,
    follow = 1,
    hide_dotfiles = 1,
    git_hl = 1,
    root_folder_modifier = ":t",
    tab_open = 0,
    allow_resize = 1,
    lsp_diagnostics = 1,
    auto_ignore_ft = { "startify", "dashboard" },
    icons = {
      default = "",
      symlink = "",
      git = {
        unstaged = "",
        staged = "S",
        unmerged = "",
        renamed = "➜",
        deleted = "",
        untracked = "U",
        ignored = "◌",
      },
      folder = {
        default = "",
        open = "",
        empty = "",
        empty_open = "",
        symlink = "",
      },
    }
}

for opt, val in pairs(config) do
  g["nvim_tree_" .. opt] = val
end

vim.api.nvim_set_keymap('', '<C-n>', ':NvimTreeToggle<cr>', { noremap = true })
vim.api.nvim_set_keymap('', '<leader>n', ':NvimTreeFindFile<cr>', { noremap = true })
