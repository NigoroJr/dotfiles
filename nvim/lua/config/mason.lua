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
  "lua_ls",
}

local add = function(name)
  table.insert(ensure_installed, name)
end

if vim.fn.executable("npm") == 1 then
  add("cssls")
  add("cssmodules_ls")
  add("css_variables")
  add("docker_compose_language_service")
  add("dockerls")
  add("pyright")
  add("tailwindcss")
  add("ts_ls")
  add("vuels")
  add("yamlls")
end

if vim.fn.executable("go") == 1 then
  add("gopls")
end

if vim.fn.executable("ruby") == 1 then
  local ruby_version = vim.fn.system({ "ruby", "--version" })
  local major, minor, patch = string.match(ruby_version, "(%d+)%.(%d+)%.(%d+)")
  major = tonumber(major)
  minor = tonumber(minor)
  patch = tonumber(patch)
  if major >= 3 or (major >= 2 and minor >= 7) then
    -- Ruby 2.7+ required
    add("rubocop")
    add("ruby_lsp")
  end
end

require("mason-lspconfig").setup({
  ensure_installed = ensure_installed,
  -- Note: Not related to ensure_installed
  automatic_installation = true,
})
