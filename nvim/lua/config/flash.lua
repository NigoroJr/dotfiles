local flash = require("flash")
flash.setup({
  labels = "abcdefghijklmnopqrstuvwxyz",
  modes = { search = { enabled = true } },
})

local map = vim.keymap.set
map({ "n", "x", "o" }, "s", function() flash.jump() end)
map({ "n", "x", "o" }, "S", function() flash.treesitter() end)
map("c", "<C-s>", function() flash.toggle() end)
