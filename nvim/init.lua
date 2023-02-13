--[[
Filename: init.lua

Purpose: Toplevel Neovim configuration
----------------------------------------
History:
20.Dec.2022    Initial set up.
28.Dec.2022    Refine layout and add options. 
----------------------------------------
Comments:

---------------------------------------
]]

require "user.options"
require "user.keymaps"
require "user.plugins"
require "user.colorscheme"
require "user.cmp"
require "user.lsp"
require "user.telescope"
require "user.gitsigns"
require "user.treesitter"
require "user.autopairs"
require "user.comment"
require "user.nvim-tree"
require "user.bufferline"
require "user.lualine"
require "user.toggleterm"
require "user.project"
require "user.impatient"
require "user.indentline"
require "user.alpha"
require "user.whichkey"
require "user.autocommands"
