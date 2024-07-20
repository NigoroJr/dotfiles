require("nightfox").setup({
  options = {
    -- transparent = true,
  },
  specs = {
    nightfox = {
      -- inactive = "bg0",
    },
  },
  groups = {
    nightfox = {
      Normal = { fg = "fg1", bg = "bg1" },
      NormalNC = { fg = "fg0", bg = "bg2" },
    },
  },
})
vim.cmd("colorscheme nightfox")
