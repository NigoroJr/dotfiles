-- Helper condition functions
local M = {}

function M.is_beginning_of_line(line_to_cursor)
  -- No whitespace before the cursor
  return not (line_to_cursor:find(" ", 1, true) or line_to_cursor:find("\t", 1, true))
end

function M.is_first_word_of_line(line_to_cursor)
  -- Matches if line_to_cursor is only whitespace followed by non-whitespace characters
  return line_to_cursor:match("^%s*%S+$") ~= nil
end

function M.is_beginning_of_file(line_to_cursor)
  if not M.is_beginning_of_line(line_to_cursor) then
    return false
  end

  -- Check if we're on the first line
  return vim.api.nvim_win_get_cursor(0)[1] == 1
end

return M