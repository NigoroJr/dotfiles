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
    t({
      "int",
      "main(int argc, char const* argv[]) {",
    }),
    i(0),
    t({
      "",
      "\treturn 0;",
      "}",
    }),
  }),

  -- include
  s({
    trig = "incl",
    dscr = "incl",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("#include \""),
    i(1),
    t("\""),
    i(0),
  }),

  -- short class
  s({
    trig = "class_short",
    dscr = "class_short",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("class "),
    i(1, "Name"),
    t({
      " {",
      "public:",
      "\t",
    }),
    i(0),
    t({
      "",
      "};",
    }),
  }),

  -- include headers
  s({
    trig = "cpp",
    dscr = "Include headers",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t({
      "#include <algorithm>",
      "#include <chrono>",
      "#include <functional>",
      "#include <iomanip>",
      "#include <iostream>",
      "#include <iterator>",
      "#include <map>",
      "#include <memory>",
      "#include <numeric>",
      "#include <set>",
      "#include <sstream>",
      "#include <string>",
      "#include <thread>",
      "#include <tuple>",
      "#include <type_traits>",
      "#include <unordered_map>",
      "#include <unordered_set>",
      "#include <utility>",
      "#include <vector>",
      "",
      "int",
      "main(const int argc, char* const argv[]) {",
    }),
    i(0),
    t({
      "",
      "\treturn 0;",
      "}",
    }),
  }),

  -- hello world
  s({
    trig = "hello",
    dscr = "hello_world",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t({
      "#include <iostream>",
      "",
      "int",
      "main(const int argc, char* const argv[]) {",
    }),
    i(0),
    t({
      "",
      "\treturn 0;",
      "}",
    }),
  }),

  -- operator overload
  s({
    trig = "operator",
    dscr = "operator overload",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    i(1, "Foo&"),
    t({
      "",
      "",
    }),
    i(2, "Foo::"),
    t("operator"),
    i(3, "="),
    t("("),
    i(4, "const Foo& other"),
    t(")"),
    i(0),
  }),

  -- template
  s({
    trig = "template",
    dscr = "template<typename T>",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("template<typename "),
    i(1, "T"),
    t(">"),
    i(0),
  }),

  -- quick class declaration
  s({
    trig = "class_quick",
    dscr = "quick class declaration",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("class "),
    i(1, "Foo"),
    t(" "),
    i(0),
    t({
      "{",
      "public:",
      "\t/* Constructors, Destructor, and Assignment operator {{{ */",
      "\t// Default constructor",
      "\t",
    }),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "()",
      "\t{",
      "\t}",
      "",
      "\t// Copy constructor",
      "\t",
    }),
    ls.dynamic_node(3, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("(const "),
    ls.dynamic_node(4, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "& other)",
      "\t\t",
    }),
    i(5, "#init member vars"),
    t({
      "",
      "\t{",
      "\t}",
      "",
      "\t// Move constructor",
      "\t",
    }),
    ls.dynamic_node(6, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("("),
    ls.dynamic_node(7, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&& other)",
      "\t\t",
    }),
    i(8, "#init member vars"),
    t({
      "",
      "\t{",
      "\t}",
      "",
      "\t// Destructor",
      "\t~",
    }),
    ls.dynamic_node(9, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "()",
      "\t{",
      "\t}",
      "",
      "\t// Assignment operator",
      "\t",
    }),
    ls.dynamic_node(10, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&",
      "\toperator=(const ",
    }),
    ls.dynamic_node(11, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "& other) {",
      "\t\t",
    }),
    i(12, "#assignments"),
    t({
      "",
      "\t\treturn *this;",
      "\t}",
      "",
      "\t// Move assignment operator",
      "\t",
    }),
    ls.dynamic_node(13, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&",
      "\toperator=(",
    }),
    ls.dynamic_node(14, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&& other) {",
      "\t\t",
    }),
    i(15, "#assignments"),
    t({
      "",
      "\t\treturn *this;",
      "\t}",
      "\t/* }}} */",
      "",
      "private:",
      "\t",
    }),
    i(16, "#member vars"),
    t({
      "",
      "};",
    }),
  }),

  -- class declaration
  s({
    trig = "class_declaration",
    dscr = "class declaration",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("class "),
    i(1, "Foo"),
    t(" "),
    i(0),
    t({
      "{",
      "public:",
      "\t/* Constructors, Destructor, and Assignment operators {{{ */",
      "\t// Default constructor",
      "\t",
    }),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "();",
      "",
      "\t// Copy constructor",
      "\t",
    }),
    ls.dynamic_node(3, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("(const "),
    ls.dynamic_node(4, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "& other);",
      "",
      "\t// Move constructor",
      "\t",
    }),
    ls.dynamic_node(5, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("("),
    ls.dynamic_node(6, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&& other);",
      "",
      "\t// Destructor",
      "\t~",
    }),
    ls.dynamic_node(7, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "();",
      "",
      "\t// Assignment operator",
      "\t",
    }),
    ls.dynamic_node(8, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&",
      "\toperator=(const ",
    }),
    ls.dynamic_node(9, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "& other);",
      "",
      "\t// Move assignment operator",
      "\t",
    }),
    ls.dynamic_node(10, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&",
      "\toperator=(",
    }),
    ls.dynamic_node(11, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&& other);",
      "\t/* }}} */",
      "",
      "private:",
      "\t",
    }),
    i(12, "#member vars"),
    t({
      "",
      "};",
    }),
  }),

  -- class definition
  s({
    trig = "class_definition",
    dscr = "class definition",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t({
      "/* Constructors, Destructor, and Assignment operators {{{ */",
      "// Default constructor",
      "",
    }),
    ls.dynamic_node(1, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("::"),
    i(1, "#class name"),
    t({
      "()",
      "\t",
    }),
    i(2, " #initializations"),
    t({
      "",
      "{",
      "}",
      "",
      "// Copy constructor",
      "",
    }),
    ls.dynamic_node(3, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("::"),
    ls.dynamic_node(4, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("(const "),
    ls.dynamic_node(5, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "& other)",
      "\t",
    }),
    i(6, " #initializations"),
    t({
      "",
      "{",
      "}",
      "",
      "// Move constructor",
      "",
    }),
    ls.dynamic_node(7, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("::"),
    ls.dynamic_node(8, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("("),
    ls.dynamic_node(9, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&& other)",
      "\t",
    }),
    i(10, " #initializations"),
    t({
      "",
      "{",
      "}",
      "",
      "// Destructor",
      "",
    }),
    ls.dynamic_node(11, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("::~"),
    ls.dynamic_node(12, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "()",
      "{",
      "}",
      "",
      "// Assignment operator",
      "",
    }),
    ls.dynamic_node(13, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&",
      "",
    }),
    ls.dynamic_node(14, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("::operator=(const "),
    ls.dynamic_node(15, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "& other) {",
      "\t",
    }),
    i(16),
    t({
      "",
      "\treturn *this;",
      "}",
      "",
      "// Move assignment operator",
      "",
    }),
    ls.dynamic_node(17, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&",
      "",
    }),
    ls.dynamic_node(18, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t("::operator=("),
    ls.dynamic_node(19, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "&& other) {",
      "\t",
    }),
    i(20),
    t({
      "",
      "\treturn *this;",
      "}",
      "/* }}} */",
      "",
    }),
  }),

  -- operator==
  s({
    trig = "operator==",
    dscr = "operator==",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t({
      "bool",
      "operator",
    }),
    i(1, "=="),
    t("(const "),
    i(2, "Foo"),
    t("& "),
    i(3, "other"),
    t(") "),
    i(4, "const"),
    i(0),
  }),

  -- operator<<
  s({
    trig = "operator<<",
    dscr = "operator<<",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t({
      "std::ostream&",
      "operator<<(std::ostream& ",
    }),
    i(1, "os"),
    t(", const "),
    i(2, "Foo"),
    t("& "),
    i(3, "f"),
    t(")"),
    i(0),
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

  -- std::copy with ostream_iterator
  s({
    trig = "dump",
    dscr = "std::copy(v.begin(), v.end(), std::ostream_iterator<>)",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("std::copy("),
    i(1, "v"),
    t(".begin(), "),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t(".end(), std::ostream_iterator<"),
    i(3, "int"),
    t(">("),
    i(4, "std::cout"),
    t(", "),
    i(5, "\" \""),
    t({
      "));",
      "std::cout << std::endl;",
    }),
    i(0),
  }),

  -- begin-end
  s({
    trig = "begin-end",
    dscr = "begin-end",
  }, {
    i(1, "#container"),
    t(".begin(), "),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t(".end()"),
    i(0),
  }),

  -- std::vector
  s({
    trig = "vector",
    dscr = "std::vector",
  }, {
    t("std::vector<"),
    i(1, "int"),
    t(">"),
    i(0),
  }),

  -- std::pair
  s({
    trig = "pair",
    dscr = "std::pair",
  }, {
    t("std::pair<"),
    i(1, "unsigned"),
    t(", "),
    i(2, "unsigned"),
    t(">"),
    i(0),
  }),

  -- include vector
  s({
    trig = "include_vector",
    dscr = "#include <vector>",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({ "#include <vector>" }),
    i(0),
  }),

  -- decltype
  s({
    trig = "dt",
    dscr = "decltype()",
  }, {
    t("decltype("),
    i(1, "#var"),
    t(")"),
    i(0),
  }),

  -- typename
  s({
    trig = "tn",
    dscr = "typename",
  }, {
    t("typename "),
    i(1, "T"),
    i(0),
  }),
}

