local telescope = require("telescope")
local fb_actions = telescope.extensions.file_browser.actions

local actions = require("telescope.actions")
local builtin = require("telescope.builtin")
local utils = require("telescope.utils")

telescope.setup({
  defaults = {
    sorting_strategy = "ascending",
    -- layout_config = {
    --   prompt_position = "top",
    -- },
    mappings = {
      i = {
        ["<C-g>"] = "close",
        ["<C-h>"] = false,
        ["<C-u>"] = false,
        ["<C-b>"] = actions.preview_scrolling_up,
        ["<C-f>"] = actions.preview_scrolling_down,
        ["<c-a>"] = { "<Home>", type = "command" },
        ["<c-e>"] = { "<End>", type = "command" },
      },
      n = {
        ["<C-g>"] = "close",
      },
    },
  },
  pickers = {
    find_files = {
      disable_devicons = false,
    },
  },
  extensions = {
    frecency = {
      db_safe_mode = false,
      disable_devicons = false,
      -- show_scores = true,
    },
    file_browser = {
      sorting_strategy = "ascending",
      hijack_netrw = false,
      mappings = {
        ["i"] = {
          ["<A-n>"] = fb_actions.create,
        },
        ["n"] = {
          ["N"] = fb_actions.create,
          ["K"] = fb_actions.create,
        },
      },
    },
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  },
})

local find_files_cwd = function()
  builtin.find_files({
    cwd = utils.buffer_dir(),
  })
end

local live_grep_cwd = function()
  builtin.live_grep({
    cwd = utils.buffer_dir(),
  })
end

local grep_string_cwd = function()
  builtin.grep_string({
    cwd = utils.buffer_dir(),
    search = "",
    only_sort_text = true,
  })
end

local map = vim.keymap.set

map("n", "<Leader>uf", builtin.find_files, {})
-- Find files starting from the directory where the file currently being
-- edited is located at
map("n", "<Leader>uF", find_files_cwd, {})
map("n", "<Leader>ug", live_grep_cwd, {})
map("n", "<Leader>us", grep_string_cwd, {})
map("n", "<Leader>ut", builtin.treesitter, {})
map("n", "<Leader>ud", builtin.diagnostics, {})

telescope.load_extension("file_browser")
telescope.load_extension("frecency")
telescope.load_extension("fzf")
telescope.load_extension("ui-select")

map("n", "<Leader>um", "<cmd>Telescope frecency<CR>", {})
map("n", "<Leader>ub", "<cmd>Telescope file_browser<CR>", {})
