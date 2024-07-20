require("lspsaga").setup({
  ui = {
    border = "rounded",
  },
  beacon = {
    enable = false,
  },
  symbol_in_winbar = {
    enable = false,
  },
  callhierarchy = {
  },
  code_action = {
    show_server_name = true,
    extend_gitsigns = true,
  },
  definition = {
  },
  diagnostic = {
    text_hl_follow = true,
  },
  finder = {
  },
  hover = {
    open_link = "gK",
  },
  lightbulb = {
    enable = false,
  },
  rename = {
    in_select = false,
    keys = {
      quit = "<C-q>",
      exec = "<CR>",
      select = "<Space>",
    }
  },
})

local map = vim.keymap.set

-- Code action
map({ "n", "i" }, "<M-CR>", "<cmd>Lspsaga code_action<CR>", {})
-- Check definition
map("n", "<M-d>", "<cmd>Lspsaga peek_definition<CR>", {})
