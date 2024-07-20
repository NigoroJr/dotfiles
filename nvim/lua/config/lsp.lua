local capabilities = require("cmp_nvim_lsp").default_capabilities()

local mason = require("mason")
local mason_lspconfig = require("mason-lspconfig")
local lspconfig = require("lspconfig")

mason.setup()
mason_lspconfig.setup()

local on_attach = function(client, bufnr)
  -- vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  vim.api.nvim_set_option_value("omnifunc", "v:lua.vim.lsp.omnifunc", { buf = bufnr })

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      underline = true,
      -- signs = false,
      update_in_insert = false,
    }
  )
end

mason_lspconfig.setup_handlers({
  function(server_name)
    lspconfig[server_name].setup({
      on_attach=on_attach,
      capabilities = capabilities,
    })
  end,
  ["clangd"] = function()
    lspconfig.clangd.setup({
      on_attach = on_attach,
    })
  end,
  ["gopls"] = function()
    lspconfig.gopls.setup({
      cmd = {"gopls", "serve"},
      filetypes = {"go", "gomod"},
      root_dir = lspconfig.util.root_pattern("go.work", "go.mod", ".git"),
      settings = {
        gopls = {
          analyses = {
            unusedparams = true,
          },
          staticcheck = true,
        },
      },
      on_attach=on_attach,
      capabilities=capabilities,
    })
  end,
  ["pyright"] = function()
    lspconfig.pyright.setup({
      filetypes = {"python"},
      root_dir = function(fname)
        local root_files = {
          "pyproject.toml",
          "setup.py",
          "setup.cfg",
          "requirements.txt",
          "Pipfile",
          "pyrightconfig.json",
        }
        return lspconfig.util.root_pattern(unpack(root_files))(fname)
      end,
      settings = {
        python = {
          analysis = {
            autoSearchPaths = true,
            useLibraryCodeForTypes = true,
            typeCheckingMode = "off",
          },
          linting = {
            enabled = false,
          }
        },
      },
      on_attach=on_attach,
      capabilities=capabilities,
    })
  end,
  ["lua_ls"] = function()
    lspconfig.lua_ls.setup({
      on_attach=on_attach,
      on_init = function(client)
        local path = client.workspace_folders[1].name
        if vim.loop.fs_stat(path.."/.luarc.json") or vim.loop.fs_stat(path.."/.luarc.jsonc") then
          return
        end

        client.config.settings.Lua = vim.tbl_deep_extend("force", client.config.settings.Lua, {
          runtime = {
            -- Tell the language server which version of Lua you're using
            -- (most likely LuaJIT in the case of Neovim)
            version = "LuaJIT"
          },
          -- Make the server aware of Neovim runtime files
          workspace = {
            checkThirdParty = false,
            library = {
              vim.env.VIMRUNTIME
              -- Depending on the usage, you might want to add additional paths here.
              -- "${3rd}/luv/library"
              -- "${3rd}/busted/library",
            }
            -- or pull in all of "runtimepath". NOTE: this is a lot slower
            -- library = vim.api.nvim_get_runtime_file("", true)
          },
        })
      end,
      settings = {
        Lua = {
          diagnostics = {
            globals = { "vim" },
          },
        },
      }
    })
  end,
})

-- nnoremap <buffer> <silent> gd <cmd>lua vim.lsp.buf.declaration()<CR>
-- nnoremap <buffer> <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
-- nnoremap <buffer> <silent> K <cmd>lua vim.lsp.buf.hover()<CR>

-- LSP servers to setup:
-- "black", "clang-format", "clangd", "cmake-language-server",
-- "cpptools", "css-lsp", "css-variables-language-server",
-- "cssmodules-language-server", "docker-compose-language-service",
-- "dockerfile-language-server", "gopls", "lua-language-server",
-- "protolint", "pyright", "rubocop", "ruby-lsp",
-- "tailwindcss-language-server", "typescript-language-server",
-- "vue-language-server", "yaml-language-server",
