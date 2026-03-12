local capabilities = require("cmp_nvim_lsp").default_capabilities()

vim.api.nvim_create_autocmd("LspAttach", {
  group = vim.api.nvim_create_augroup("UserLspConfig", {}),
  callback = function(args)
    local bufnr = args.buf
    local client = vim.lsp.get_client_by_id(args.data.client_id)

    if not client then return end

    vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

    require("lsp_signature").on_attach({
      bind = true,
      handler_opts = { border = "rounded" },
    }, bufnr)

    if client.supports_method("textDocument/formatting", { bufnr = bufnr }) then
      local augroup = vim.api.nvim_create_augroup("LspFormattingOnSave", { clear = false })
      vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
      vim.api.nvim_create_autocmd("BufWritePre", {
        group = augroup,
        buffer = bufnr,
        callback = function()
          vim.lsp.buf.format({ bufnr = bufnr, id = client.id, async = false })
        end,
      })
    end
  end,
})

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
