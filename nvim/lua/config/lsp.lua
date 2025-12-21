local capabilities = require("cmp_nvim_lsp").default_capabilities()

local on_attach = function(client, bufnr)
  -- vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

  vim.lsp.config('*', {
  handlers = {
    ['textDocument/hover'] = {
      border = 'rounded',
    }
  }
})

  require("lsp_signature").on_attach(
    {
      bind = true,
      doc_lines = 30,
      max_height = 30,
      hint_enable = false,
      handler_opts = {
        border = "rounded",
      },
    },
    bufnr
  )

  -- Format on save
  if client.supports_method("textDocument/formatting") then
    local augroup = vim.api.nvim_create_augroup("LspFormattingOnSave", {})
    vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = augroup,
      buffer = bufnr,
      callback = function()
        vim.lsp.buf.format({ bufnr = bufnr, async = false })
      end,
    })
  end
end

vim.diagnostic.config({
  float = {
    border = "rounded",
  },
  virtual_text = false,
  underline = true,
  -- signs = false,
  update_in_insert = false,
})

local map = vim.keymap.set
-- Added in neovim 0.11
local jump_prev = function()
  vim.diagnostic.jump({ count = -1, float = true })
end
local jump_next = function()
  vim.diagnostic.jump({ count = 1, float = true })
end
map("n", "<S-M-p>", jump_prev)
map("n", "<S-M-n>", jump_next)
map("n", "K", vim.lsp.buf.hover, {})
map({ "n", "i" }, "<S-M-r>", vim.lsp.buf.rename, {})

-- Configure LSP servers with custom settings
-- mason-lspconfig automatically enables installed servers
vim.lsp.config("*", {
  on_attach = on_attach,
  capabilities = capabilities,
})

-- Server-specific configurations
vim.lsp.config("gopls", {
  settings = {
    gopls = {
      analyses = {
        unusedparams = true,
      },
      staticcheck = true,
      gofumpt = true,
    },
  },
})

vim.lsp.config("lua_ls", {
  on_init = function(client)
    local path = client.workspace_folders[1].name
    if vim.uv.fs_stat(path.."/.luarc.json") or vim.uv.fs_stat(path.."/.luarc.jsonc") then
      return
    end
    client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
      runtime = { version = "LuaJIT" },
      workspace = {
        checkThirdParty = false,
        library = { vim.env.VIMRUNTIME }
      },
    })
  end,
  settings = {
    Lua = {
      diagnostics = { globals = { "vim" } },
    },
  },
})
