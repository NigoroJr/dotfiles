require("mason").setup({
  ui = {
    border = "rounded",
    width = 0.8,
    height = 0.8,
  },
})

local ensure_installed = {
  "clangd",
  "cmake",
  "css_variables",
  "cssls",
  "cssmodules_ls",
  "docker_compose_language_service",
  "dockerls",
  "lua_ls",
  "pyright",
  "tailwindcss",
  "tsserver",
  "vuels",
  "yamlls",
}

if vim.fn.executable("go") == 1 then
  table.insert(ensure_installed, "gopls")
end

if vim.fn.executable("ruby") == 1 then
  local ruby_version = vim.fn.system({ "ruby", "--version" })
  local major, minor, patch = string.match(ruby_version, "(%d+)%.(%d+)%.(%d+)")
  major = tonumber(major)
  minor = tonumber(minor)
  patch = tonumber(patch)
  if major >= 3 or (major >= 2 and minor >= 7) then
    -- Ruby 2.7+ required
    table.insert(ensure_installed, "rubocop")
    table.insert(ensure_installed, "ruby_lsp")
  end
end

require("mason-lspconfig").setup({
  ensure_installed = ensure_installed,
  -- Note: Not related to ensure_installed
  automatic_installation = true,
})
