require'bufferline'.setup {
  options = {
    view = 'multiwindow',
    separator_style = 'slant',
  },
}
-- Colors are taken care of directly in colorscheme
vim.cmd 'au! BufferlineColors ColorScheme'
