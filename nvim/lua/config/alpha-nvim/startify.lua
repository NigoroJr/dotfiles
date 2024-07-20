local if_nil = vim.F.if_nil
local fnamemodify = vim.fn.fnamemodify
local filereadable = vim.fn.filereadable

local path_ok, plenary_path = pcall(require, "plenary.path")
if not path_ok then
  return
end

local default_header = {
  type = "text",
  val = {
    [[                                  __]],
    [[     ___     ___    ___   __  __ /\_\    ___ ___]],
    [[    / _ `\  / __`\ / __`\/\ \/\ \\/\ \  / __` __`\]],
    [[   /\ \/\ \/\  __//\ \_\ \ \ \_/ |\ \ \/\ \/\ \/\ \]],
    [[   \ \_\ \_\ \____\ \____/\ \___/  \ \_\ \_\ \_\ \_\]],
    [[    \/_/\/_/\/____/\/___/  \/__/    \/_/\/_/\/_/\/_/]],
  },
  opts = {
    position = "center",
    hl = "Type",
  },
}

local leader = ","

--- @param sc string
--- @param txt string
--- @param keybind string? optional
--- @param keybind_opts table? optional
local function button(sc, txt, keybind, keybind_opts)
  local sc_ = sc:gsub("%s", ""):gsub(leader, "<Leader>")

  local opts = {
    position = "center",
    width = 78,
    shortcut = "[" .. sc .. "] ",
    cursor = 1,
    align_shortcut = "left",
    hl_shortcut = { { "Operator", 0, 1 }, { "Number", 1, #sc + 1 }, { "Operator", #sc + 1, #sc + 2 } },
    -- shrink_margin = false,
  }
  if keybind then
    keybind_opts = if_nil(keybind_opts, { noremap = true, silent = true, nowait = true })
    opts.keymap = { "n", sc_, keybind, keybind_opts }
  end

  local function on_press()
    local key = vim.api.nvim_replace_termcodes(keybind .. "<Ignore>", true, false, true)
    vim.api.nvim_feedkeys(key, "t", false)
  end

  return {
    type = "button",
    val = txt,
    on_press = on_press,
    opts = opts,
  }
end

local nvim_web_devicons = {
  enabled = true,
  highlight = true,
}

local function get_extension(fn)
  local match = fn:match("^.+(%..+)$")
  local ext = ""
  if match ~= nil then
    ext = match:sub(2)
  end
  return ext
end

local function icon(fn)
  local nwd = require("nvim-web-devicons")
  local ext = get_extension(fn)
  return nwd.get_icon(fn, ext, { default = true })
end

local function file_button(fn, sc, short_fn, autocd)
  short_fn = if_nil(short_fn, fn)
  local ico_txt
  local fb_hl = {}
  if nvim_web_devicons.enabled then
    local ico, hl = icon(fn)
    local hl_option_type = type(nvim_web_devicons.highlight)
    if hl_option_type == "boolean" then
      if hl and nvim_web_devicons.highlight then
        table.insert(fb_hl, { hl, 0, #ico })
      end
    end
    if hl_option_type == "string" then
      table.insert(fb_hl, { nvim_web_devicons.highlight, 0, #ico })
    end
    ico_txt = ico .. "  "
  else
    ico_txt = ""
  end
  local cd_cmd = (autocd and " | cd %:p:h" or "")
  local file_button_el = button(sc, ico_txt .. short_fn, "<cmd>e " .. vim.fn.fnameescape(fn) .. cd_cmd .." <CR>")
  local fn_start = short_fn:match(".*[/\\]")
  if fn_start ~= nil then
    table.insert(fb_hl, { "Comment", #ico_txt, #fn_start + #ico_txt })
  end
  file_button_el.opts.hl = fb_hl
  return file_button_el
end

local default_mru_ignore = { "gitcommit" }

local mru_opts = {
  ignore = function(path, ext)
    return (string.find(path, "COMMIT_EDITMSG")) or (vim.tbl_contains(default_mru_ignore, ext))
  end,
  autocd = false
}

--- @param start number | string
--- @param cwd string? optional
--- @param items_number number? optional number of items to generate, default = 20
local function mru(start, cwd, items_number, opts)
  opts = opts or mru_opts
  items_number = if_nil(items_number, 20)

  local oldfiles = {}
  for _, v in pairs(vim.v.oldfiles) do
    if #oldfiles == items_number then
      break
    end
    local cwd_cond
    if not cwd then
      cwd_cond = true
    else
      cwd_cond = vim.startswith(v, cwd)
    end
    local ignore = (opts.ignore and opts.ignore(v, get_extension(v))) or false
    if (filereadable(v) == 1) and cwd_cond and not ignore then
      oldfiles[#oldfiles + 1] = v
    end
  end
  local target_width = 74

  local tbl = {}
  local num_skipped = 0
  for i, fn in ipairs(oldfiles) do
    local short_fn
    if cwd then
      short_fn = fnamemodify(fn, ":.")
    else
      short_fn = fnamemodify(fn, ":~")
    end

    if #short_fn > target_width then
      short_fn = plenary_path.new(short_fn):shorten(1, { -2, -1 })
      if #short_fn > target_width then
        short_fn = plenary_path.new(short_fn):shorten(1, { -1 })
      end
    end

    local contains = function(arr, val)
      for _, v in ipairs(arr) do
        if v == val then
          return true
        end
      end
      return false
    end

    local shortcut = nil
    while true do
      if type(start) == "string" then
        shortcut = string.char(string.byte(start) + i + num_skipped - 1)
      else
        shortcut = tostring(start + i + num_skipped - 1)
      end

      if not opts.skip_shortcuts then
        break
      elseif contains(opts.skip_shortcuts, shortcut) then
        num_skipped = num_skipped + 1
      else
        break
      end
    end

    local file_button_el = file_button(fn, shortcut, short_fn, opts.autocd)
    tbl[i] = file_button_el
  end
  return {
    type = "group",
    val = tbl,
    opts = {},
  }
end

local function mru_title()
  return "[ Recent Files in " .. vim.fn.getcwd() .. " ]"
end

local section = {
  header = default_header,
  top_buttons = {
    type = "group",
    val = {
      { type = "text", val = "[ General Operations ]", opts = { hl = "SpecialComment", position = "center" } },
      { type = "padding", val = 1 },
      button("N", "New file & start editing", "<cmd>ene<CR>a"),
      button("L", "Open Lazy & sync", "<cmd>Lazy sync<CR>"),
      button("M", "Open Mason", "<cmd>Mason<CR>"),
    },
  },
  -- note about MRU: currently this is a function,
  -- since that means we can get a fresh mru
  -- whenever there is a DirChanged. this is *really*
  -- inefficient on redraws, since mru does a lot of I/O.
  -- should probably be cached, or maybe figure out a way
  -- to make it a reference to something mutable
  -- and only mutate that thing on DirChanged
  mru = {
    type = "group",
    val = {
      { type = "text", val = "[ Recent Files ]", opts = { hl = "SpecialComment", position = "center" } },
      { type = "padding", val = 1 },
      {
        type = "group",
        val = function()
          return {
            mru(
              "A", nil, 21,
              { skip_shortcuts = { "G", "L", "M", "N", "Q" } }
            )
          }
        end,
        opts = {},
      },
    },
  },
  mru_cwd = {
    type = "group",
    val = {
      { type = "text", val = mru_title, opts = { hl = "SpecialComment", position = "center" } },
      { type = "padding", val = 1 },
      {
        type = "group",
        val = function()
          -- return { mru(0, vim.fn.getcwd()) }
          return {
            mru(
              "a", vim.fn.getcwd(), 22,
              { skip_shortcuts = { "j", "k", "g", "q" } }
            )
          }
        end,
        opts = {},
      },
    },
  },
  bottom_buttons = {
    type = "group",
    val = {
      { type = "text", val = "[ Other Operations ]", opts = { hl = "SpecialComment", position = "center" } },
      { type = "padding", val = 1 },
      button("q", "Quit", "<cmd>q <CR>"),
    },
  },
  footer = {
    type = "group",
    val = {
      { type = "text", val = "...and as always, have a great day!", opts = { hl = "SpecialComment", position = "center" } },
    },
  },
}

local config = {
  layout = {
    { type = "padding", val = 1 },
    section.header,
    { type = "padding", val = 1 },
    section.top_buttons,
    { type = "padding", val = 1 },
    section.mru_cwd,
    { type = "padding", val = 1 },
    section.mru,
    { type = "padding", val = 1 },
    section.bottom_buttons,
    { type = "padding", val = 2 },
    section.footer,
  },
  opts = {
    -- redraw_on_resize = false,
    setup = function()
      vim.api.nvim_create_autocmd("DirChanged", {
        pattern = "*",
        group = "alpha_temp",
        callback = function ()
          require("alpha").redraw()
          vim.cmd("AlphaRemap")
        end,
      })
    end,
  },
}

return {
  icon = icon,
  button = button,
  file_button = file_button,
  mru = mru,
  mru_opts = mru_opts,
  section = section,
  config = config,
  -- theme config
  nvim_web_devicons = nvim_web_devicons,
  leader = leader,
}
