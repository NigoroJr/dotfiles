require("illuminate").configure({
  providers = {
    "lsp",
    "treesitter",
    "regex",
  },
  delay = vim.o.updatetime,
  min_count_to_highlight = 2,
})
