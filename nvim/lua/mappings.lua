local opts = {
  noremap = true,
}
local opts_s = {
  noremap = true,
  silent = true,
}

vim.g.mapleader = ","

local map = vim.keymap.set

map("n", "gj", "j", opts)
map("n", "gj", "j", opts)
map("n", "gk", "k", opts)
map("n", "j", "gj", opts)
map("n", "k", "gk", opts)
map("n", "'", "`", opts)
map("n", "`", "'", opts)

-- More powerful ex command history
map("c", "<C-p>", "<Up>", opts)
map("c", "<C-n>", "<Down>", opts)
-- Shell key bindings in ex mode
map("c", "<C-b>", "<Left>", opts)
map("c", "<C-f>", "<Right>", opts)
map("c", "<M-f>", "<S-Right>", opts)
map("c", "<M-b>", "<S-Left>", opts)
-- Mimic M-d on emacs
map("c", "<M-d>", "<S-Right><C-w><Backspace><Right>", opts)
map("c", "<C-a>", "<Home>", opts)
map("c", "<C-e>", "<End>", opts)
map("c", "<C-g>", "<C-c>", opts)
map("c", "<C-d>", "<Delete>", opts)

-- Insert mode
map("i", "<C-d>", "<Delete>", opts)
map("i", "<C-t>", "<Nop>", opts)

-- Use modifiers for substitution
map("n", "&", "<cmd>&&<CR>", opts)
map("x", "&", "<cmd>&&<CR>", opts)

-- Reset highlight search by pressing Escape 2 times
map("n", "<Esc><Esc>", "<cmd>nohlsearch<CR>", opts_s)

-- Move pwd to directory of buffer
map("n", "<Leader>cd", "<cmd>cd %:h<CR>", opts_s)
map("n", "<Leader>lcd", "<cmd>lcd %:h<CR>", opts_s)

-- Q to quit
map("n", "Q", "<cmd>q<CR>", opts_s)

-- Function to toggle variables and show message
local function toggle(cmd, display)
  vim.o[cmd] = not vim.o[cmd]

  vim.print(display .. " " .. (vim.o[cmd] and "ON" or "OFF"))
end

local spell = function() toggle("spell", "Spell") end
map("n", "<Leader>ss", spell, opts_s)
local scrollbind = function() toggle("scrollbind", "ScrollBind") end
map("n", "<Leader>sb", scrollbind, opts_s)
local expandtab = function() toggle("expandtab", "ExpandTab") end
map("n", "<Leader>set", expandtab, opts_s)
local wrap = function() toggle("wrap", "Wrap") end
map("n", "<Leader>sw", wrap, opts)
local paste = function() toggle("paste", "Paste") end
map("n", "<Leader>sp", paste, opts)

-- Toggle diff
local function toggle_diff()
  vim.opt.diff = not vim.opt.diff:get()
  vim.o.scrollbind = vim.opt.diff:get()
  vim.print("Diff " .. (vim.opt.diff:get() and "ON" or "OFF"))
end
map("n", "<Leader>dt", toggle_diff, opts_s)

-- Save as super user
map("n", "<Leader>ws", require("utils").sudo_write, opts_s)

map({ "n", "i" }, "<C-k>", vim.lsp.buf.signature_help, opts_s)
