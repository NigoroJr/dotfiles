require("jaq-nvim").setup({
  cmds = {
    -- Uses vim commands
    internal = {
      lua = "luafile %",
      vim = "source %"
    },

    -- Uses shell commands
    external = {
      markdown = "glow %",
      python = "python3 %",
      ruby = "ruby %",
      go = "go run %",
      sh = "sh %"
    }
  },

  behavior = {
    -- Default type
    default = "terminal",

    -- Start in insert mode
    startinsert = false,

    -- Use `wincmd p` on startup
    wincmd = true,

    -- Auto-save files
    autosave = true,

    -- Close existing buffers
    closeexisting = true
  },

  ui = {
    float = {
      -- See ':h nvim_open_win'
      border = "rounded",

      -- See ':h winhl'
      winhl = "Normal",
      borderhl = "FloatBorder",

      -- See ':h winblend'
      winblend = 0,

      -- Num from `0-1` for measurements
      height = 0.8,
      width = 0.8,
      x = 0.5,
      y = 0.5
    },

    terminal = {
      -- Window position
      position = "vert",

      -- Window size
      size = 80,

      -- Disable line numbers
      line_no = false
    },

    quickfix = {
      -- Window position
      position = "vert",

      -- Window size
      size = 80
    }
  }
})
