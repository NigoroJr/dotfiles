local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
  vim.fn.system(
    {
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable",
      lazypath,
    }
  )
end
vim.opt.rtp:prepend(lazypath)

local plugins = require("plugins")

local opts = {
  install = {
    colorscheme = { "nightfox", "habamax" },
  },
  ui = {
    -- single, double, rounded, solid, shadow
    border = "rounded",
    size = {
      width = 0.8,
      height = 0.8,
    },
  },
}

require("lazy").setup(plugins, opts)

vim.keymap.set("n", "<Leader>la", "<cmd>Lazy<CR>")
