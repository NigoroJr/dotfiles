require("luasnip.loaders.from_snipmate").lazy_load({ paths = "./snippets" })

local ls = require("luasnip")

local function load_snippets(filetype, path)
  if path == nil then
    path = filetype
  end

  local snippets = dofile(vim.fn.stdpath("config") .. "/snippets/" .. path .. ".lua")
  ls.add_snippets(filetype, snippets)
end

load_snippets("all")
load_snippets("python")
load_snippets("dockerfile", "docker")
load_snippets("zsh")
load_snippets("sh")
load_snippets("make")
load_snippets("c")
load_snippets("cmake")
load_snippets("go")
load_snippets("cpp")

vim.keymap.set("n", "<Leader>es", require("luasnip.loaders").edit_snippet_files)
