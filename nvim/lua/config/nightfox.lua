require("nightfox").setup({
  options = {
    -- transparent = true,
  },
  specs = {
    nightfox = {
      inactive = "bg0",
    },
  },
  groups = {
    nightfox = {
      NormalNC = { fg = "fg0", bg = "inactive" },
    },
  },
})
vim.cmd("colorscheme nightfox")
