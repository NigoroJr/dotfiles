require("lualine").setup({
  options = {
    icons_enabled = false,
    theme = "auto",
    component_separators = { left = "", right = ""},
    section_separators = { left = " ", right = " "},
    disabled_filetypes = {
      statusline = {},
      winbar = {},
    },
    ignore_focus = {},
    always_divide_middle = true,
    globalstatus = false,
    refresh = {
      statusline = 1000,
      tabline = 1000,
      winbar = 1000,
    }
  },
  sections = {
    lualine_a = {
      {
        "filename",
        path = 1,
        symbols = {
          modified = "[+]",
          readonly = "[RO]",
          unnamed = "[No Name]",
          newfile = "[New]",
        },
      }
    },
    lualine_b = {"branch", "diff", "diagnostics"},
    lualine_c = {},
    lualine_x = {"encoding", "fileformat", "filetype"},
    lualine_y = {"location"},
    lualine_z = {"progress"}
  },
  inactive_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = { { "filename", path = 1 } },
    lualine_x = {"progress"},
    lualine_y = {},
    lualine_z = {}
  },
  tabline = {},
  winbar = {},
  inactive_winbar = {},
  extensions = {}
})
