--[[
Filename: confog.lua

Purpose: Toplevel LunarVim configuration
----------------------------------------
History:
21.Dec.2022    Initial set up.
24.Dec.2022    Refine layout and add options. 
----------------------------------------
Comments:
lvim is the global options object

Linters should be
filled in as strings with either
a global executable or a path to
an executable
]]


reload "user.plugins"
reload "user.options"
reload "user.colorschemes"
reload "user.telescope"
reload "colorizer.lua"
reload "user.zk"

-- require "user.keymaps"

-- require "user.cmp"
-- require "user.lsp"
-- require "user.telescope"
-- require "user.gitsigns"
-- require "user.treesitter"
-- require "user.autopairs"
-- require "user.comment"
-- require "user.nvim-tree"
-- require "user.bufferline"
-- require "user.lualine"
-- require "user.toggleterm"
-- require "user.project"
-- require "user.impatient"
-- require "user.indentline"
-- require "user.alpha"
-- require "user.whichkey"
-- require "user.autocommands"

-- -----------------------------------------------------------------------------
-- -- General
-- -----------------------------------------------------------------------------

-- lvim.log.level = "warn"
-- lvim.format_on_save.enabled = false

-- ----------------------------------------------------------------------------
-- -- Colourscheme settings
-- -----------------------------------------------------------------------------
-- vim.g.gruvbox_baby_function_style = "NONE"
-- vim.g.gruvbox_baby_keyword_style = "NONE"
-- vim.g.gruvbox_baby_comment_style = "NONE"

-- -- Each highlight group must follow the structure:
-- -- ColorGroup = {fg = "foreground color", bg = "background_color", style = "some_style(:h attr-list)"}
-- -- See also :h highlight-guifg
-- -- Example:
-- -- viim.g.gruvbox_baby_highlights = {Normal = {fg = "#123123", bg = "NONE", style="underline"}}

-- -- Enable telescope theme
-- vim.g.gruvbox_baby_telescope_theme = 1

-- -- Enable transparent mode
-- -- vim.g.gruvbox_baby_transparent_mode = 1

-- lvim.colorscheme = "gruvbox-baby"

-- --------------------------------------------------------------------------------
-- -- Load plug-ins
-- --------------------------------------------------------------------------------
-- lvim.plugins = {
--   -- https://github.com/luisiacc/gruvbox-baby -- colourscheme
--   {"luisiacc/gruvbox-baby"},
--   -- https://github.com/tpope/vim-fugitive -- Git operations
--   {
--   "tpope/vim-fugitive",
--   cmd = {
--     "G",
--     "Git",
--     "Gdiffsplit",
--     "Gread",
--     "Gwrite",
--     "Ggrep",
--     "GMove",
--     "GDelete",
--     "GBrowse",
--     "GRemove",
--     "GRename",
--     "Glgrep",
--     "Gedit"
--   },
--   ft = {"fugitive"}
--   },
--   -- https://github.com/norcalli/nvim-colorizer.lua -- CSS colour highlighter
--   {
--    "norcalli/nvim-colorizer.lua",
--      config = function()
--        require("colorizer").setup({ "css", "scss", "html", "javascript" }, {
--            RGB = true, -- #RGB hex codes
--            RRGGBB = true, -- #RRGGBB hex codes
--            RRGGBBAA = true, -- #RRGGBBAA hex codes
--            rgb_fn = true, -- CSS rgb() and rgba() functions
--            hsl_fn = true, -- CSS hsl() and hsla() functions
--            css = true, -- Enable all CSS features: rgb_fn, hsl_fn, names, RGB, RRGGBB
--            css_fn = true, -- Enable all CSS *functions*: rgb_fn, hsl_fn
--            })
--    end,
--  },
-- }
