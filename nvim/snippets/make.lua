local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_file = utils.is_beginning_of_file

return {
  -- template for g++
  s({
    trig = "main",
    dscr = "Template for g++",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t("CXX="),
    i(1, "g++"),
    t({
      "",
      "CXXFLAGS=",
    }),
    i(2, "-std=c++11 -Wall"),
    t({
      "",
      "PROG_NAME=",
    }),
    i(3),
    t({
      "",
      "LIBS=",
    }),
    i(4),
    t({
      "",
      "OBJS=",
    }),
    i(5),
    t({
      "",
      "MAIN=",
    }),
    i(6),
    t({
      "",
      "",
      "all: $(PROG_NAME)",
      "",
      "$(PROG_NAME): $(LIBS) $(OBJS) $(MAIN)",
      "\t$(CXX) $(CXXFLAGS) -o $@ $(OBJS) $(MAIN)",
      "",
      "%.o: %.cpp %.h",
      "\t$(CXX) $(CXXFLAGS) -c $<",
      "",
      "clean:",
      "\trm -rf $(PROG_NAME) *.o",
    }),
  }),
}

