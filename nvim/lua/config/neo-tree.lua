local renderer = require("neo-tree.ui.renderer")

-- https://github.com/nvim-neo-tree/neo-tree.nvim/issues/1165#issuecomment-1740137096
local indexOf = function(array, value)
    for i, v in ipairs(array) do
        if v == value then
            return i
        end
    end
    return nil
end

local getSiblings = function(state, node)
    local parent = state.tree:get_node(node:get_parent_id())
    local siblings = parent:get_child_ids()
    return siblings
end

local nextSibling = function(state)
    local node = state.tree:get_node()
    local siblings = getSiblings(state, node)
    if not node.is_last_child then
        local currentIndex = indexOf(siblings, node.id)
        local nextIndex = siblings[currentIndex + 1]
        renderer.focus_node(state, nextIndex)
    end
end

local prevSibling = function(state)
    local node = state.tree:get_node()
    local siblings = getSiblings(state, node)
    local currentIndex = indexOf(siblings, node.id)
    if currentIndex > 1 then
        local nextIndex = siblings[currentIndex - 1]
        renderer.focus_node(state, nextIndex)
    end
end

local firstSibling = function(state)
    local node = state.tree:get_node()
    local siblings = getSiblings(state, node)
    renderer.focus_node(state, siblings[1])
end

local lastSibling = function(state)
    local node = state.tree:get_node()
    local siblings = getSiblings(state, node)
    renderer.focus_node(state, siblings[#siblings])
end

require("neo-tree").setup({
  filesystem = {
    window = {
      position = "current",
      mappings = {
        ["e"] = "open",
        ["l"] = "open",
        ["h"] = "close_node",
        ["P"] = "navigate_up",
        ["K"] = "add_directory",
        ["N"] = "add",
        ["gg"] = firstSibling,
        ["G"] = lastSibling,
      },
    },
  },
  event_handlers = {
    -- Enter input popup with normal mode by default.
    -- {
    --     event = "neo_tree_popup_input_ready",
    --     handler = function()
    --         vim.cmd("stopinsert")
    --     end,
    -- },
    -- Map <esc> to enter normal mode (by default closes prompt)
    {
      event = "neo_tree_popup_input_ready",
      ---@param args { bufnr: integer, winid: integer }
      handler = function(args)
        -- don't forget `opts.buffer` to specify the buffer of the popup.
        vim.keymap.set(
          "i", "<esc>", vim.cmd.stopinsert,
          { noremap = true, buffer = args.bufnr }
        )
      end,
    },
  },
})

local command = require("neo-tree.command")

local normal_open = function()
  local reveal_file = vim.fn.expand('%:p')
  if (reveal_file == '') then
    reveal_file = vim.fn.getcwd()
  else
    local f = io.open(reveal_file, "r")
    if (f) then
      f.close(f)
    else
      reveal_file = vim.fn.getcwd()
    end
  end
  command.execute({
    action = "focus",
    source = "filesystem",
    position = "current",
    reveal_file = reveal_file,
    reveal_force_cwd = true,
  })
end

local map = vim.keymap.set
map("n", "<Leader>f", normal_open, {})
