-- Automatically make scripts executable
local function make_executable(fpath)
  local dpath = vim.fs.dirname(fpath)
  local res = vim.system({"git", "status"}, {cwd = dpath}):wait()
  if res.code == 0 then
    -- Do nothing if it's a git repo
    return
  end

  local first_line = vim.api.nvim_buf_get_lines(0, 0, 1, false)[1]
  if string.match(first_line, "^#!") and string.match(first_line, "/bin") then
    vim.system({"chmod", "a+x", fpath})
  end
end

vim.api.nvim_create_autocmd(
  "BufWritePost",
  {
    pattern = "*",
    callback = function(ev) make_executable(ev.match) end,
  }
)

-- https://github.com/neovim/neovim/issues/16339#issuecomment-1457394370
vim.api.nvim_create_autocmd(
  "BufRead",
  {
    callback = function(opts)
      vim.api.nvim_create_autocmd(
      "BufWinEnter",
      {
        once = true,
        buffer = opts.buf,
        callback = function()
          local ft = vim.bo[opts.buf].filetype
          local last_known_line = vim.api.nvim_buf_get_mark(opts.buf, '"')[1]
          if not (ft:match('commit') and ft:match('rebase'))
            and last_known_line > 1
            and last_known_line <= vim.api.nvim_buf_line_count(opts.buf) then
            vim.api.nvim_feedkeys([[g`"]], "nx", false)
          end
        end,
      }
      )
    end,
  }
)

vim.api.nvim_create_autocmd(
  "CursorMoved", {
    group = vim.api.nvim_create_augroup("auto-hlsearch", { clear = true }),
    callback = function ()
      if vim.v.hlsearch == 1 and vim.fn.searchcount().exact_match == 0 then
        vim.schedule(function () vim.cmd.nohlsearch() end)
      end
    end
  }
)

vim.api.nvim_create_autocmd(
  "FileType", {
    group = vim.api.nvim_create_augroup("vert-vim-help", { clear = true }),
    pattern = "help",
    command = "wincmd L",
  }
)
