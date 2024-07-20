local options = {
  backup = true,
  backupdir = vim.fs.normalize("~/.local/share/nvim/backups/"),
  viewdir = vim.fs.normalize("~/.local/share/nvim/view/"),
  undodir = vim.fs.normalize("~/.local/share/nvim/undo/"),
  undofile = true,
  tabstop = 4,
  softtabstop = 4,
  expandtab = true,
  shiftwidth = 4,
  shiftround = true,
  textwidth = 78,
  number = true,
  wildmode = {"longest", "list"},
  foldenable = true,
  foldmethod = "marker",
  viewoptions = {"cursor", "folds"},
  fileencodings = {"ucs-bom", "utf-8", "iso-2022-jp", "sjis", "cp932", "euc-jp", "cp20932", "utf-16", "utf-16le", "guess"},
  splitright = true,
  splitbelow = true,
  modeline = true,
  autoread = true,
  showtabline = 0,
  signcolumn = "yes",
  updatetime = 500,
  autoindent = true,
  smartindent = true,
  smartcase = true,
  termguicolors = true,
  formatlistpat = [[^\\s*\\(\\d\\+[\\]:.)}\\t\ ]\\\|[*+-]\\)\\s*]],
  syntax = "on",
}

vim.opt.shortmess:append("c")

for k, v in pairs(options) do
  vim.opt[k] = v
end

vim.opt.completeopt:append({"noinsert", "noselect"})
vim.opt.completeopt:remove("preview")
vim.opt.formatoptions:append("nmBj")
vim.opt.matchpairs:append("<:>")

-- Disable netrw
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.cmd([[highlight Normal guibg = NONE ctermbg=NONE]])
vim.cmd([[highlight Pmenu ctermbg = 32 ctermfg=235]])
vim.cmd([[highlight PmenuSel ctermfg = 252]])
