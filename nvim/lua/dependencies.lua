local bin_path = vim.env.HOME .. "/bin"
vim.fn.mkdir(bin_path, "p'")

function install_rg(version)
  local uname = vim.uv.os_uname()
  if uname.sysname == "Linux" then
    system = "x86_64-unknown-linux-musl"
  elseif uname.sysname == "Darwin" then
    system = "aarch64-apple-darwin"
  end

  -- Download
  local tmpfile = vim.fn.system("mktemp"):gsub("%s+", "")
  vim.fn.system(
    {
      "wget",
      string.format(
        "https://github.com/BurntSushi/ripgrep/releases/download/%s/ripgrep-%s-%s.tar.gz",
        version,
        version,
        system
      ),
      "-O",
      tmpfile,
    }
  )

  -- Extract file
  vim.fn.mkdir(tmpfile..".d", "p")
  vim.fn.system(
    {
      "tar",
      "xzf",
      tmpfile,
      "-C",
      tmpfile .. ".d",
    }
  )

  -- Move extracted file
  vim.fn.system(
    {
      "mv",
      string.format(
        "%s.d/ripgrep-%s-%s/rg",
        tmpfile,
        version,
        system
      ),
      bin_path,
    }
  )
end

function install_tree_sitter_cli(version)
  local uname = vim.uv.os_uname()
  if uname.sysname == "Linux" then
    system = "linux-x64"
  elseif uname.sysname == "Darwin" then
    system = "macos-arm64"
  end

  -- Download
  local tmpfile = vim.fn.system("mktemp"):gsub("%s+", "")
  vim.fn.system(
    {
      "wget",
      string.format(
        "https://github.com/tree-sitter/tree-sitter/releases/download/v%s/tree-sitter-%s.gz",
        version,
        system
      ),
      "-O",
      tmpfile .. ".gz",
    }
  )

  -- Extract file
  vim.fn.system(
    {
      "gzip",
      "-f",
      "-d",
      tmpfile .. ".gz",
    }
  )
  vim.fn.system(
    {
      "chmod",
      "+x",
      tmpfile
    }
  )

  -- Move extracted file
  vim.fn.system(
    {
      "mv",
      tmpfile,
      bin_path .. "/tree-sitter",
    }
  )
end

if vim.fn.executable("rg") == 0 then
  print("Installing dependeny: ripgrep")
  install_rg("14.1.0")
end

if vim.fn.executable("tree-sitter") == 0 then
  print("Installing dependeny: tree-sitter CLI")
  install_tree_sitter_cli("0.22.6")
end
