local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_file = utils.is_beginning_of_file
local is_first_word_of_line = utils.is_first_word_of_line

return {
  -- fmt.Print
  s({
    trig = "p",
    dscr = "fmt.Print()",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("fmt.Print("),
    i(1),
    t(")"),
    i(0),
  }),

  -- fmt.Println
  s({
    trig = "pl",
    dscr = "fmt.Println()",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("fmt.Println("),
    i(1),
    t(")"),
    i(0),
  }),

  -- fmt.Printf
  s({
    trig = "pf",
    dscr = "fmt.Printf()",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("fmt.Printf(\""),
    i(1),
    t("\", "),
    i(2),
    t(")"),
    i(0),
  }),

  -- fmt.Fprint
  s({
    trig = "fp",
    dscr = "fmt.Fprint()",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("fmt.Fprint("),
    i(1, "w"),
    t(", "),
    i(2),
    t(")"),
    i(0),
  }),

  -- fmt.Fprintln
  s({
    trig = "fpl",
    dscr = "fmt.Fprintln()",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("fmt.Fprintln("),
    i(1, "w"),
    t(", "),
    i(2),
    t(")"),
    i(0),
  }),

  -- fmt.Fprintf
  s({
    trig = "fpf",
    dscr = "fmt.Fprintf()",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("fmt.Fprintf("),
    i(1, "w"),
    t(", \""),
    i(2),
    t("\", "),
    i(3),
    t(")"),
    i(0),
  }),

  -- interface implementation
  s({
    trig = "impl",
    dscr = "impl",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("func ("),
    i(1, "#:name"),
    t(" "),
    i(2, "#:interface"),
    t(") "),
    i(3, "Func"),
    t("("),
    i(4, "#:args"),
    t(") "),
    i(5, "#:ret"),
    t({
      "{",
      "\t",
    }),
    i(0),
    t({
      "",
      "}",
    }),
  }),

  -- func main
  s({
    trig = "main",
    dscr = "func main()",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t({
      "package main",
      "",
      "import (",
      "\t\"fmt\"",
      ")",
      "",
      "func main() {",
      "\t",
    }),
    i(1, "fmt.Println(\"Hello, 世界\")"),
    t({
      "",
      "}",
    }),
  }),

  -- if-else
  s({
    trig = "ife",
    dscr = "if-else",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("if "),
    i(1, "#:cond"),
    t({
      " {",
      "\t",
    }),
    i(2),
    t({
      "",
      "} else {",
      "\t",
    }),
    i(0),
    t({
      "",
      "}",
    }),
  }),

  -- for range
  s({
    trig = "for",
    dscr = "for",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("for "),
    i(1, "k"),
    t(", "),
    i(2, "v"),
    t(" := range "),
    i(3),
    t({
      " {",
      "\t",
    }),
    i(0),
    t({
      "",
      "}",
    }),
  }),

  -- for loop
  s({
    trig = "for2",
    dscr = "for2",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("for "),
    i(1, "i"),
    t(" := "),
    i(2),
    t("; "),
    ls.dynamic_node(3, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t(" < "),
    i(4),
    t("; "),
    ls.dynamic_node(5, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "++ {",
      "\t",
    }),
    i(0),
    t({
      "",
      "}",
    }),
  }),

  -- switch
  s({
    trig = "swi",
    dscr = "switch",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("switch "),
    i(1, "val"),
    t({
      " {",
      "case ",
    }),
    i(2, "#:cond"),
    t({
      ":",
      "\t",
    }),
    i(0),
    t({
      "",
      "}",
    }),
  }),

  -- select
  s({
    trig = "sel",
    dscr = "select",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t({
      "select {",
      "case ",
    }),
    i(1, "v"),
    t(" := <-"),
    i(2, "ch"),
    t({
      "",
      "\t",
    }),
    i(0),
    t({
      "",
      "}",
    }),
  }),

  -- test function
  s({
    trig = "test",
    dscr = "test",
  }, {
    t("func Test"),
    i(1, "Function"),
    t({
      "(t *testing.T) {",
      "\t",
    }),
    i(2),
    t({
      "",
      "}",
    }),
  }),
}

