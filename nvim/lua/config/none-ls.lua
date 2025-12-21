local none_ls = require("null-ls")

-- Setup none-ls with format-on-save
none_ls.setup({
  sources = {
    -- Add any custom sources here that aren't available in Mason
  },
  on_attach = function(client, bufnr)
    -- Format on save (same as LSP servers)
    if client.supports_method("textDocument/formatting") then
      local augroup = vim.api.nvim_create_augroup("NoneLsFormattingOnSave", {})
      vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = augroup,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({
            bufnr = bufnr,
            async = false,
            filter = function(format_client)
              return format_client.name == "null-ls"
            end,
          })
        end,
      })
    end
  end,
})
