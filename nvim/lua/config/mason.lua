require("mason").setup({
  ui = {
    border = "rounded",
    width = 0.8,
    height = 0.8,
  },
})

-- LSP servers
local lsp_servers = {
  "clangd",
  "cmake",
  "docker_language_server",
  "lua_ls",
  "protols",
  "ruff",
  "terraformls",
  "ty",
}

-- Formatters and other tools
local tools = {
  "goimports",
  "prettierd",
}

local add_lsp = function(name)
  table.insert(lsp_servers, name)
end

local add_tool = function(name)
  table.insert(tools, name)
end

if vim.fn.executable("npm") == 1 then
  add_lsp("cssls")
  add_lsp("cssmodules_ls")
  add_lsp("css_variables")
  add_lsp("docker_compose_language_service")
  add_lsp("dockerls")
  add_lsp("svelte")
  add_lsp("tailwindcss")
  add_lsp("ts_ls")
  add_lsp("vuels")
  add_lsp("yamlls")
end

if vim.fn.executable("go") == 1 then
  add_lsp("gopls")
end

if vim.fn.executable("ruby") == 1 then
  local ruby_version = vim.fn.system({ "ruby", "--version" })
  local major, minor, patch = string.match(ruby_version, "(%d+)%.(%d+)%.(%d+)")
  major = tonumber(major)
  minor = tonumber(minor)
  patch = tonumber(patch)
  if major >= 3 or (major >= 2 and minor >= 7) then
    -- Ruby 2.7+ required
    add_tool("rubocop")
    add_lsp("ruby_lsp")
  end
end

-- Setup mason-lspconfig for LSP servers
require("mason-lspconfig").setup({
  ensure_installed = lsp_servers,
  automatic_installation = true,
})

-- Setup mason-null-ls for formatters/linters
require("mason-null-ls").setup({
  ensure_installed = tools,
  automatic_installation = true,
  handlers = {},
})
