require("luasnip.loaders.from_snipmate").lazy_load({paths = "./snippets"})

vim.keymap.set("n", "<Leader>es", require("luasnip.loaders").edit_snippet_files)
