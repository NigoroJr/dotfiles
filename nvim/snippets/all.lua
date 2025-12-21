local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_line = utils.is_beginning_of_line
local is_beginning_of_file = utils.is_beginning_of_file

-- Utility function to create shebang snippets
-- lines: can be a string (single line) or table of strings (multiple lines)
-- Each line will be on its own line in the output
local function shebang(trigger, lines, description, filetype)
  -- Convert single string to table
  if type(lines) == "string" then
    lines = {lines}
  end

  description = description or lines[1]

  local snippet_opts = {
    trig = trigger,
    dscr = description,
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }

  -- Build the text node content with proper line breaks
  local text_lines = {}
  for i, line in ipairs(lines) do
    table.insert(text_lines, line)
  end
  -- Add empty lines after the content
  table.insert(text_lines, "")
  table.insert(text_lines, "")

  local snippet_nodes = {
    t(text_lines),
    i(0),
  }

  if filetype then
    local callbacks = {
      [-1] = {
        [require("luasnip.util.events").enter] = function(node)
          local bufnr = vim.api.nvim_get_current_buf()
          vim.bo[bufnr].filetype = filetype

          -- Trigger LSP to attach after filetype is set
          vim.schedule(function()
            -- Trigger FileType autocmd for this buffer
            vim.api.nvim_exec_autocmds("FileType", {
              buffer = bufnr,
            })

            -- Give LSP a moment to fully initialize and send didChange notification
            vim.defer_fn(function()
              -- Trigger buffer update to notify LSP
              vim.api.nvim_buf_call(bufnr, function()
                vim.cmd("checktime")
              end)
            end, 100)
          end)
        end
      }
    }

    return s(snippet_opts, snippet_nodes, { callbacks = callbacks })
  else
    return s(snippet_opts, snippet_nodes)
  end
end

-- Utility function for UV script shebang
local function uv_script()
  local snippet_opts = {
    trig = "uv",
    dscr = "#!/usr/bin/env -S uv run --script",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }

  local snippet_nodes = {
    t({"#!/usr/bin/env -S uv run --script", "# /// script", "# requires-python = \">="}),
    i(1),
    t({"\"", "# dependencies = [", "#\t\""}),
    i(2),
    t(">="),
    i(3),
    t({"\",", "# ]", "# ///", ""}),
    i(0),
  }

  local callbacks = {
    [-1] = {
      [require("luasnip.util.events").enter] = function(node)
        local bufnr = vim.api.nvim_get_current_buf()
        vim.bo[bufnr].filetype = "python"

        -- Trigger LSP to attach after filetype is set
        vim.schedule(function()
          -- Trigger FileType autocmd for this buffer
          vim.api.nvim_exec_autocmds("FileType", {
            buffer = bufnr,
          })

          -- Give LSP a moment to fully initialize and send didChange notification
          vim.defer_fn(function()
            -- Trigger buffer update to notify LSP
            vim.api.nvim_buf_call(bufnr, function()
              vim.cmd("checktime")
            end)
          end, 100)
        end)
      end
    }
  }

  return s(snippet_opts, snippet_nodes, { callbacks = callbacks })
end

-- Define all snippets
return {
  s({
    trig = "copyright",
    dscr = "Copyright",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("Copyright "),
    f(function() return os.date("%Y") end),
    t(" "),
    i(1, "Naoki Mizuno"),
  }),

  -- Shebang snippets - only at beginning of file
  shebang("sh", "#!/bin/sh", "Shebang for sh", "sh"),
  shebang("zsh", "#!/usr/bin/env zsh", "Shebang for zsh", "zsh"),
  shebang("bash", "#!/bin/bash", "Shebang for bash", "bash"),
  shebang("ruby", "#!/usr/bin/env ruby", "Shebang for Ruby", "ruby"),
  shebang("py", "#!/usr/bin/env python", "Shebang for Python", "python"),
  shebang("perl", {
    "#!/usr/bin/env perl",
    "",
    "use strict;",
    "use warnings;",
  }, "Shebang for Perl", "perl"),

  -- UV script with custom content
  uv_script(),
}

