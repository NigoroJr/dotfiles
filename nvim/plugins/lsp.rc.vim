lua << END
local lspconfig = require 'lspconfig'

local on_attach = function(_, bufnr)
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
end

lspconfig.clangd.setup{}
lspconfig.cmake.setup{}

-- Use the jedi-language-server found, otherwise use the one in the virtualenv
local jedi_ls_cmd = ''
if vim.fn.executable('jedi-language-server') == 1 then
  jedi_ls_cmd = 'jedi-language-server'
else
  local python3_dirname = vim.fn.fnamemodify(vim.g.python3_host_prog, ':h')
  if vim.fn.executable(python3_dirname .. '/jedi-language-server') then
    jedi_ls_cmd = python3_dirname .. '/jedi-language-server'
  end
end
if vim.fn.executable(jedi_ls_cmd) then
  lspconfig.jedi_language_server.setup{
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

lspconfig.yamlls.setup{}
END

" nnoremap <buffer> <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
" nnoremap <buffer> <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
" nnoremap <buffer> <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
"
" autocmd Filetype python setlocal omnifunc=v:lua.vim.lsp.omnifunc
