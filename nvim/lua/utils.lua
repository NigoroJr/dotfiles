-- Thanks to ibhagwan
-- https://github.com/ibhagwan/nvim-lua/blob/main/lua/utils.lua

-- help to inspect results, e.g.:
-- ':lua _G.dump(vim.fn.getwininfo())'
-- neovim 0.7 has 'vim.pretty_print())
function _G.dump(...)
  local objects = vim.tbl_map(vim.inspect, { ... })
  print(unpack(objects))
end

local M = {}

M.__HAS_NVIM_08 = vim.fn.has("nvim-0.8") == 1
M.__HAS_NVIM_010 = vim.fn.has("nvim-0.10") == 1
M.__HAS_NVIM_011 = vim.fn.has("nvim-0.11") == 1
M.IS_WINDOWS = vim.fn.has("win32") == 1 or vim.fn.has("win64") == 1

M._if_win = function(a, b)
  if M.IS_WINDOWS then
    return a
  else
    return b
  end
end

M._if_win_fs_norm = function(a, b)
  return M._if_win(vim.fs.normalize(a), b or a)
end

local fast_event_aware_notify = function(msg, level, opts)
  if vim.in_fast_event() then
    vim.schedule(function()
      vim.notify(msg, level, opts)
    end)
  else
    vim.notify(msg, level, opts)
  end
end

function M.info(msg)
  fast_event_aware_notify(msg, vim.log.levels.INFO, {})
end

function M.warn(msg)
  fast_event_aware_notify(msg, vim.log.levels.WARN, {})
end

function M.err(msg)
  fast_event_aware_notify(msg, vim.log.levels.ERROR, {})
end

function M.is_root()
  return not M.IS_WINDOWS and vim.loop.getuid() == 0
end

function M.is_darwin()
  return vim.loop.os_uname().sysname == "Darwin"
end

function M.is_NetBSD()
  return vim.loop.os_uname().sysname == "NetBSD"
end

function M.shell_error()
  return vim.v.shell_error ~= 0
end

function M.have_compiler()
  if vim.fn.executable("cc") == 1 or
      vim.fn.executable("gcc") == 1 or
      vim.fn.executable("clang") == 1 or
      vim.fn.executable("cl") == 1 then
    return true
  end
  return false
end

function M.git_root(cwd, noerr)
  local cmd = { "git", "rev-parse", "--show-toplevel" }
  if cwd then
    table.insert(cmd, 2, "-C")
    table.insert(cmd, 3, vim.fn.expand(cwd))
  end
  local output = vim.fn.systemlist(cmd)
  if M.shell_error() then
    if not noerr then M.info(unpack(output)) end
    return nil
  end
  return output[1]
end

function M.set_cwd(pwd)
  if not pwd then
    local parent = vim.fn.expand("%:h")
    pwd = M.git_root(parent, true) or parent
  end
  if vim.loop.fs_stat(pwd) then
    vim.cmd("cd " .. pwd)
    M.info(("pwd set to %s"):format(vim.fn.shellescape(pwd)))
  else
    M.warn(("Unable to set pwd to %s, directory is not accessible")
      :format(vim.fn.shellescape(pwd)))
  end
end

M.sudo_exec = function(cmd, print_output)
  vim.fn.inputsave()
  local password = vim.fn.inputsecret("Password: ")
  vim.fn.inputrestore()
  if not password or #password == 0 then
    M.warn("Invalid password, sudo aborted")
    return false
  end
  local out = vim.fn.system(string.format("sudo -p '' -S %s", cmd), password)
  if vim.v.shell_error ~= 0 then
    print("\r\n")
    M.err(out)
    return false
  end
  if print_output then print("\r\n", out) end
  return true
end

M.sudo_write = function(tmpfile, filepath)
  if not tmpfile then tmpfile = vim.fn.tempname() end
  if not filepath then filepath = vim.fn.expand("%") end
  if not filepath or #filepath == 0 then
    M.err("E32: No file name")
    return
  end
  -- `bs=1048576` is equivalent to `bs=1M` for GNU dd or `bs=1m` for BSD dd
  -- Both `bs=1M` and `bs=1m` are non-POSIX
  local cmd = string.format("dd if=%s of=%s bs=1048576",
    vim.fn.shellescape(tmpfile),
    vim.fn.shellescape(filepath))
  -- no need to check error as this fails the entire function
  vim.api.nvim_exec2(string.format("write! %s", tmpfile), { output = true })
  if M.sudo_exec(cmd) then
    -- refreshes the buffer and prints the "written" message
    vim.cmd.checktime()
    -- exit command mode
    vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(
      "<Esc>", true, false, true), "n", true)
  end
  vim.fn.delete(tmpfile)
end

function M.input(prompt)
  local ok, res
  if vim.ui then
    ok, _ = pcall(vim.ui.input, { prompt = prompt },
      function(input)
        res = input
      end)
  else
    ok, res = pcall(vim.fn.input, { prompt = prompt, cancelreturn = 3 })
    if res == 3 then
      ok, res = false, nil
    end
  end
  return ok and res or nil
end

function M.lsp_get_clients(...)
  ---@diagnostic disable-next-line: deprecated
  return M.__HAS_NVIM_011 and vim.lsp.get_clients(...) or vim.lsp.get_active_clients(...)
end

return M
