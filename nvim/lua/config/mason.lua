require("mason").setup({
  ui = {
    border = "rounded",
    width = 0.8,
    height = 0.8,
  },
})

require("mason-lspconfig").setup({
  ensure_installed = {
    "clangd",
    "cmake",
    "css_variables",
    "cssls",
    "cssmodules_ls",
    "docker_compose_language_service",
    "dockerls",
    "gopls",
    "lua_ls",
    "pyright",
    -- Ruby 2.7+ required
    -- "rubocop",
    -- "ruby_lsp",
    "tailwindcss",
    "tsserver",
    "vuels",
    "yamlls",
  },
  -- Note: Not related to ensure_installed
  automatic_installation = true,
})
