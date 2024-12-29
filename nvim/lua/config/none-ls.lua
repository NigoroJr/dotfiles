local null_ls = require("null-ls")

local sources = {}

if vim.fn.executable("prettierd") == 1 then
  table.insert(sources, null_ls.builtins.formatting.prettierd)
elseif vim.fn.executable("prettier") == 1 then
  table.insert(sources, null_ls.builtins.formatting.prettier)
end

if vim.fn.executable("black") == 1 then
  table.insert(sources, null_ls.builtins.formatting.black)
end

if vim.fn.executable("buf") == 1 then
  table.insert(sources, null_ls.builtins.formatting.buf)
end

if vim.fn.executable("gofmt") == 1 then
  table.insert(sources, null_ls.builtins.formatting.gofmt)
end

if vim.fn.executable("goimports") == 1 then
  table.insert(sources, null_ls.builtins.formatting.goimports)
end

local augroup = vim.api.nvim_create_augroup("LspFormattingOnSave", {})
null_ls.setup({
  sources = sources,
  on_attach = function(client, bufnr)
    if client.supports_method("textDocument/formatting") then
      vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = augroup,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({ bufnr = bufnr, async = false })
        end,
      })
    end
  end,
})
