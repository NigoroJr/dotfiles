local prettier_bin = ""

if vim.fn.executable("prettierd") then
  prettier_bin = "prettierd"
elseif vim.fn.executable("prettier") then
  prettier_bin = "prettier"
else
  return false
end

require("prettier").setup({
  bin = prettier_bin,
  filetypes = {
    "css",
    "graphql",
    "html",
    "javascript",
    "javascriptreact",
    "json",
    "less",
    "markdown",
    "scss",
    "typescript",
    "typescriptreact",
    "yaml",
  },
})
