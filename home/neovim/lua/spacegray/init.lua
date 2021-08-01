vim.api.nvim_command "hi clear"
if vim.fn.exists "syntax_on" then
  vim.api.nvim_command "syntax reset"
end
vim.o.background = "dark"
vim.o.termguicolors = true
vim.g.colors_name = "spacegray"

Config = require "spacegray.config"
C = require('spacegray.palette')

local function highlight(group, properties)
  local bg = properties.bg == nil and "" or "guibg=" .. properties.bg
  local fg = properties.fg == nil and "" or "guifg=" .. properties.fg
  local style = properties.style == nil and "" or "gui=" .. properties.style

  local cmd = table.concat({
    "highlight",
    group,
    bg,
    fg,
    style,
  }, " ")

  vim.api.nvim_command(cmd)
end

local highlights = require('spacegray.highlights')
local markdown = require('spacegray.markdown')
local Git = require('spacegray.Git')

local skeletons = {
  highlights,
  markdown,
  Git
}

for _, skeleton in ipairs(skeletons) do
  for group, properties in pairs(highlights) do
    highlight(group, properties)
  end
end
