return {
  {
    "EdenEast/nightfox.nvim",
    priority = 1000,
    lazy = false,
    config = function()
      require("config/nightfox")
    end,
  },
  {
    "nvim-tree/nvim-web-devicons",
    event = "VeryLazy",
    cmd = {
      "NvimWebDeviconsHiTest",
    },
  },
  {
    "goolord/alpha-nvim",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
    },
    config = function()
      require("config/alpha-nvim")
    end,
  },
  {
    "RRethy/nvim-treesitter-endwise",
    lazy = true,
    event = { "BufReadPost", "BufNewFile" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    config = function()
      require("config/nvim-autopairs")
    end,
  },
  {
    "numToStr/Comment.nvim",
    config = function()
      require("config/comment")
    end,
    keys = { "g" },
  },
  {
    "norcalli/nvim-colorizer.lua",
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require("config/nvim-colorizer")
    end,
  },
  {
    "kylechui/nvim-surround",
    config = function()
      require("config/nvim-surround")
    end,
    keys = { "<Leader>s" }
  },
  {
    "williamboman/mason.nvim",
    cmd = {
      "Mason",
      "MasonInstall",
      "MasonUninstall",
      "MasonUninstallAll",
      "MasonLog",
      "MasonUpdate",
    },
  },
  {
    "williamboman/mason-lspconfig.nvim",
    event = "InsertEnter",
    dependencies = {
      "williamboman/mason.nvim",
      "neovim/nvim-lspconfig",
      "nvimdev/lspsaga.nvim",
    },
    config = function()
      require("config/mason")
    end,
  },
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPost", "BufNewFile" },
    dependencies = { "williamboman/mason-lspconfig.nvim" },
    config = function()
      require("config/lsp")
    end,
    cmd = { "LspInfo", "LspInstall", "LspUninstall" },
  },
  {
    "nvim-lualine/lualine.nvim",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    config = function()
      require("config/lualine")
    end,
  },
  {
    "lewis6991/gitsigns.nvim",
    events = { "BufReadPost", "BufNewFile" },
    config = function()
      require("config/gitsigns")
    end,
  },
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = true,
    events = { "BufReadPost", "BufNewFile" },
    config = function()
      require("config/nvim-treesitter")
    end,
  },
  {
    "nvim-telescope/telescope.nvim",
    lazy = true,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "nvim-treesitter/nvim-treesitter",
      "nvim-telescope/telescope-fzf-native.nvim",
      "nvim-telescope/telescope-frecency.nvim",
      "nvim-telescope/telescope-file-browser.nvim",
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      require("config/telescope")
    end,
    keys = { "<Leader>u" },
    cmd = { "Telescope" },
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    lazy = true,
    build = "cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release",
  },
  {
    "hrsh7th/nvim-cmp",
    event = { "InsertEnter", "CmdwinEnter", "CmdlineEnter", },
    dependencies = {
      "hrsh7th/cmp-nvim-lsp",
      "hrsh7th/cmp-nvim-lua",
      "hrsh7th/cmp-buffer",
      "hrsh7th/cmp-path",
      "hrsh7th/cmp-cmdline",
      "ray-x/lsp_signature.nvim",
      "L3MON4D3/LuaSnip",
    },
    config = function()
      require("config/nvim-cmp")
    end,
  },
  {
    "L3MON4D3/LuaSnip",
    event = "InsertEnter",
    dependencies = {
      "saadparwaiz1/cmp_luasnip",
      "nvim-telescope/telescope-ui-select.nvim",
    },
    config = function()
      require("config/luasnip")
    end,
    keys = { "<Leader>es" },
  },
  {
    "folke/flash.nvim",
    config = function()
      require("config/flash")
    end,
    keys = { "/", "?", "f", "F", "t", "T", "s", "S", "<C-s>", },
  },
  {
    "nvim-neo-tree/neo-tree.nvim",
    branch = "v3.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-tree/nvim-web-devicons",
      "MunifTanjim/nui.nvim",
    },
    config = function()
      require("config/neo-tree")
    end,
    keys = { "<Leader>f" },
  },
  {
    "windwp/nvim-ts-autotag",
    events = { "BufReadPost", "BufNewFile" },
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("config/nvim-ts-autotag")
    end,
  },
  {
    "MunifTanjim/prettier.nvim",
    event = "BufWritePre",
    dependencies = {
      "nvimtools/none-ls.nvim",
    },
    config = function()
      require("config/prettier")
    end,
  },
  {
    "nvimtools/none-ls.nvim",
    lazy = true,
    config = function()
      require("config/none-ls")
    end,
  },
  {
    "RRethy/vim-illuminate",
    event = { "BufReadPost", "BufNewFile" },
    config = function()
      require("config/illuminate")
    end,
  },
  {
    "nvimdev/lspsaga.nvim",
    layy = true,
    config = function()
      require("config/lspsaga")
    end,
    dependencies = {
      "nvim-treesitter/nvim-treesitter",
      "nvim-tree/nvim-web-devicons",
    },
    keys = { "<M-CR>", "<M-d>" },
  },
  {
    -- "is0n/jaq-nvim",
    "naoki-mizuno/jaq-nvim",
    branch = "close-existing",
    lazy = true,
    config = function()
      require("config/jaq-nvim")
    end,
    keys = {
      { "<Leader>r", "<cmd>Jaq<CR>" },
    },
    cmd = { "Jaq" },
  },
}
