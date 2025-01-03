require("nvim-treesitter.configs").setup({
  highlight = {
    enable = true,
  },
  indent = {
    enable = true,
  },
  ensure_installed = {
    "bash",
    "bibtex",
    "c",
    "c_sharp",
    "cmake",
    "comment",
    "cpp",
    "css",
    "csv",
    "dart",
    "diff",
    "dockerfile",
    "dot",
    "doxygen",
    "ebnf",
    "git_config",
    "git_rebase",
    "gitattributes",
    "gitcommit",
    "gitignore",
    "gnuplot",
    "go",
    "gpg",
    "graphql",
    "gstlaunch",
    "haskell",
    "html",
    "java",
    "javascript",
    "jq",
    "json",
    "json5",
    "kdl",
    "kotlin",
    "latex",
    "llvm",
    "lua",
    "luadoc",
    "make",
    "markdown",
    "markdown_inline",
    "matlab",
    "nginx",
    "ninja",
    "passwd",
    "pem",
    "perl",
    "php",
    "printf",
    "proto",
    "python",
    "r",
    "readline",
    "regex",
    "requirements",
    "robots",
    "rst",
    "ruby",
    "sql",
    "ssh_config",
    "tmux",
    "toml",
    "tsv",
    "tsx",
    "typescript",
    "vala",
    "vim",
    "vimdoc",
    "verilog",
    "vhdl",
    "vue",
    "xml",
    "yaml",
  },
  auto_install = vim.fn.executable("tree-sitter") == 1,
  sync_install = false,

  -- RRenthy/nvim-treesitter-endwise
  endwise = {
    enable = true,
  },
})
