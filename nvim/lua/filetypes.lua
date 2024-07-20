-- Indentations
local indentations = {
  -- 2
  cmake = 2,
  eruby = 2,
  html = 2,
  lua = 2,
  ruby = 2,
  toml = 2,
  vim = 2,
  xhtml = 2,
  xml = 2,
  yaml = 2,
  -- 4
  python = 4,
  scss = 4,
  -- 8
  man = 8,
}

local function create_ftype_autocmd(ftype, arg, callback)
  vim.api.nvim_create_autocmd(
    "FileType",
    {
      pattern = ftype,
      callback = function() callback(arg) end,
    }
  )
end

for ftype, val in pairs(indentations) do
  create_ftype_autocmd(
    ftype,
    val,
    function()
      vim.bo.shiftwidth = val
      vim.bo.tabstop = val
      vim.bo.softtabstop = val
    end
  )
end

-- Other filetype-specific options
local other_options = {
  text = {textwidth = 0},
  vimshell = {textwidth = 0},
  tex = {textwidth = 0},
  python = {textwidth = 88},
  go = {expandtab = false},
  man = {number = false, expandtab = false},
}

for ftype, d in pairs(other_options) do
  for opt, val in pairs(d) do
    create_ftype_autocmd(ftype, val, function() vim.bo[opt] = val end)
  end
end

local ftypes = {
  ["*launch"] = "xml",
  ["*.world"] = "xml",
  ["*.sdf"] = "xml",
  ["*.urdf"] = "xml",
  ["*.qml"] = "qml",
  ["Dockerfile.*"] = "dockerfile",
}

-- File type overrides
local function create_ftdetect_autocmd(pattern, ftype)
  vim.api.nvim_create_autocmd(
    {"BufNewFile", "BufRead"},
    {
      pattern = pattern,
      callback = function() vim.bo.filetype = ftype end,
    }
  )
end

for pattern, ftype in pairs(ftypes) do
  create_ftdetect_autocmd(pattern, ftype)
end

