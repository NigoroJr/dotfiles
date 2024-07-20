require("illuminate").configure({
  providers = {
    "lsp",
    "treesitter",
    "regex",
  },
  delay = 500,
  min_count_to_highlight = 2,
})
