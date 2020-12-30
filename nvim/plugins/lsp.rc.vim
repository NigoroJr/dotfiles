lua << END
local lsp = require 'lspconfig'

on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      underline = true,
      signs = false,
      update_in_insert = false,
    }
  )
end

if vim.fn.executable('clangd') then
  lsp.clangd.setup{}
end

lsp.cmake.setup{}

-- Use pyright if available
if vim.fn.executable('pyright-langserver') then
  lsp.pyright.setup{
    root_dir = function(fname)
      return vim.fn.getcwd()
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
    on_attach = on_attach,
  }
-- Use jedi-language-server otherwise
else
  local jedi_ls_cmd = ''
  -- Prefer using the jedi-language-server in the current env
  if vim.fn.executable('jedi-language-server') == 1 then
    jedi_ls_cmd = 'jedi-language-server'
  else
    local python3_dirname = vim.fn.fnamemodify(vim.g.python3_host_prog, ':h')
    if vim.fn.executable(python3_dirname .. '/jedi-language-server') then
      jedi_ls_cmd = python3_dirname .. '/jedi-language-server'
    end
  end
  if vim.fn.executable(jedi_ls_cmd) then
    lsp.jedi_language_server.setup{
      cmd = {jedi_ls_cmd},
      init_options = {
        diagnostics = {
          enable = false,
        },
        completion = {
          disableSnippets = true,
        },
        jediSettings = {
          autoImportModules = {'numpy', 'pandas'}
        }
      },
      on_attach = on_attach,
    }
  end
end

lsp.yamlls.setup{}
END

nnoremap <buffer> <silent> gd <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <buffer> <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <buffer> <silent> K <cmd>lua vim.lsp.buf.hover()<CR>

autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
