local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_line = utils.is_beginning_of_line
local is_first_word_of_line = utils.is_first_word_of_line
local is_beginning_of_file = utils.is_beginning_of_file

return {
  -- main
  s({
    trig = "main",
    dscr = "main",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({ "int main(int argc, char const* argv[]) {" }),
    i(0, "TARGET"),
    t({
      "",
      "\treturn 0;",
      "}",
    }),
  }),

  -- printf
  s({
    trig = "pf",
    dscr = "printf",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("printf(\""),
    i(1),
    t("\\n\""),
    i(2),
    t(");"),
    i(0),
  }),

  -- malloc
  s({
    trig = "malloc",
    dscr = "malloc",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("("),
    i(3),
    t("*)malloc(sizeof("),
    i(1, "#:type"),
    t(") * "),
    i(2),
    t(");"),
    i(0),
  }),

  -- hello world
  s({
    trig = "hello",
    dscr = "Hello, World!",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t({
      "#include <stdio.h>",
      "#include <stdlib.h>",
      "#include <string.h>",
      "",
      "int main(const int argc, char const *argv[]) {",
      "\t",
    }),
    i(1, "printf(\"Hello, World!\\\\n\");"),
    t({
      "",
      "",
      "\treturn 0;",
      "}",
    }),
  }),

  -- implement me
  s({
    trig = "impl_me",
    dscr = "Implement me!",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t({ "// TODO: Implement me!", "" }),
  }),

  -- switch-case
  s({
    trig = "switch",
    dscr = "switch-case",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("switch ("),
    i(1, "#var"),
    t({
      ") {",
      "\tcase ",
    }),
    i(2, "#val"),
    t({
      ":",
      "\t\t",
    }),
    i(0),
    t({
      "",
      "\t\tbreak;",
      "}",
    }),
  }),

  -- unsigned
  s({
    trig = "us",
    dscr = "unsigned",
  }, {
    t("unsigned "),
    i(0),
  }),
}

