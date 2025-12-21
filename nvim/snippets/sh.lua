local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_line = utils.is_beginning_of_line

return {
  -- getopts
  s({
    trig = "getopts",
    dscr = "while getopts",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("while getopts \"h"),
    i(1),
    t({
      "\" flag; do",
      "\tcase \"$flag\" in",
      "\t\t",
    }),
    i(2, "option"),
    t({ ")", "\t\t\t" }),
    i(3, "SOME_VAR"),
    t("=\""),
    i(4, "value"),
    t({
      "\"",
      "\t\t\t;;",
      "\t\th)",
      "\t\t\techo \"Usage: $0 [-h] [",
    }),
    ls.dynamic_node(5, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 2 }),
    t({
      "]\"",
      "\t\t\texit 0",
      "\tesac",
      "",
      "done",
      "shift $(( $OPTIND - 1 ))",
    }),
  }),
}

