lua << END
local lsp = require "lspconfig"

on_attach = function(client, bufnr)
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    vim.lsp.diagnostic.on_publish_diagnostics, {
      virtual_text = false,
      underline = true,
      signs = false,
      update_in_insert = false,
    }
  )
end

if vim.fn.executable("clangd") == 1 then
  lsp.clangd.setup{
    on_attach=on_attach,
  }
end

if vim.fn.executable("cmake-language-server") == 1 then
  local cmake_ls_cmd = ""
  -- Prefer using the cmake-language-server in the current env
  if vim.fn.executable("cmake-language-server") == 1 then
    cmake_ls_cmd = "cmake-language-server"
  else
    local python3_dirname = vim.fn.fnamemodify(vim.g.python3_host_prog, ":h")
    if vim.fn.executable(python3_dirname .. "/cmake-language-server") == 1 then
      cmake_ls_cmd = python3_dirname .. "/cmake-language-server"
    end
  end
  lsp.cmake.setup{
    cmd = {cmake_ls_cmd},
    on_attach=on_attach,
  }
end

-- Use pyright if available
if vim.fn.executable("pyright-langserver") == 1 then
  lsp.pyright.setup{
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
      return lsp.util.root_pattern(unpack(root_files))(fname) or vim.fn.getcwd()
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
  local jedi_ls_cmd = ""
  -- Prefer using the jedi-language-server in the current env
  if vim.fn.executable("jedi-language-server") == 1 then
    jedi_ls_cmd = "jedi-language-server"
  else
    local python3_dirname = vim.fn.fnamemodify(vim.g.python3_host_prog, ":h")
    if vim.fn.executable(python3_dirname .. "/jedi-language-server") == 1 then
      jedi_ls_cmd = python3_dirname .. "/jedi-language-server"
    end
  end
  if vim.fn.executable(jedi_ls_cmd) == 1 then
    lsp.jedi_language_server.setup{
      cmd = {jedi_ls_cmd},
      filetypes = {"python"},
      init_options = {
        diagnostics = {
          enable = true,
        },
        completion = {
          disableSnippets = true,
        },
        jediSettings = {
          autoImportModules = {"numpy", "pandas"}
        }
      },
      on_attach = on_attach,
    }
  end
end

if vim.fn.executable("yaml-language-server") == 1 then
  lsp.yamlls.setup{
    on_attach=on_attach,
  }
end
END

nnoremap <buffer> <silent> gd <cmd>lua vim.lsp.buf.declaration()<CR>
nnoremap <buffer> <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
nnoremap <buffer> <silent> K <cmd>lua vim.lsp.buf.hover()<CR>

autocmd CursorHold * lua vim.lsp.diagnostic.show_line_diagnostics()
