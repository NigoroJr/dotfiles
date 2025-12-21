local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_file = utils.is_beginning_of_file
local is_first_word_of_line = utils.is_first_word_of_line

return {
  -- minimum cmake template
  s({
    trig = "hello",
    dscr = "minimum",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t("cmake_minimum_required(VERSION "),
    i(1, "2.8"),
    t({
      ")",
      "project(",
    }),
    i(2),
    t({
      ")",
      "",
      "file(GLOB ",
    }),
    i(3, "SOURCES"),
    t(" "),
    i(4, "\"src/*.cpp\""),
    t({
      ")",
      "",
      "add_executable(",
    }),
    i(5),
    t(" "),
    i(6, "SOURCES"),
    t({
      ")",
      "set_property(TARGET ",
    }),
    ls.dynamic_node(7, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 2 }),
    t({ " PROPERTY CXX_STANDARD 11)" }),
    i(0),
  }),

  -- cmake_minimum_required
  s({
    trig = "cmake_minimum_required",
    dscr = "cmake_minimum_required",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("cmake_minimum_required("),
    i(1, "VERSION"),
    t(" "),
    i(2, "2.8"),
    t(")"),
    i(0),
  }),

  -- add_library
  s({
    trig = "add_library",
    dscr = "add_library",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("add_library("),
    i(1, "#name"),
    t(" "),
    i(2, "SHARED"),
    t(" "),
    i(3, "#sources"),
    t(")"),
    i(0),
  }),

  -- set_property
  s({
    trig = "set_property",
    dscr = "set_property",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("set_property(TARGET "),
    i(1, "#name"),
    t(" PROPERTY "),
    i(2, "#property"),
    t(")"),
    i(0),
  }),

  -- include_directories
  s({
    trig = "include_directories",
    dscr = "include_directories",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("include_directories("),
    i(1, "src"),
    t(")"),
    i(0),
  }),

  -- boost.test
  s({
    trig = "add_unittest",
    dscr = "Boost.Test",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t({
      "find_package(Boost REQUIRED COMPONENTS unit_test_framework)",
      "",
      "",
    }),
    i(1, "add_definitions(-DBOOST_TEST_DYN_LINK)"),
    t({
      "",
      "enable_testing()",
      "macro (add_unittest NAME MAIN_SRC)",
      "\tadd_executable (${NAME} ${MAIN_SRC} ${ARGN})",
      "\ttarget_link_libraries (${NAME} ${Boost_UNIT_TEST_FRAMEWORK_LIBRARY})",
      "\tadd_test (${NAME} ${NAME})",
      "endmacro (add_unittest)",
    }),
    i(0),
  }),

  -- set flags
  s({
    trig = "set_flags",
    dscr = "set(CMAKE_CXX_FLAGS \"-std=c++11\")",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("set(CMAKE_CXX_FLAGS \""),
    i(1, "-std=c++11 -Wall -Wextra"),
    t({
      "\")",
      "set(CMAKE_CXX_FLAGS_DEBUG \"",
    }),
    i(2, "-O0 -g"),
    t({
      "\")",
      "set(CMAKE_CXX_FLAGS_RELEASE \"",
    }),
    i(3, "-O3"),
    t("\")"),
    i(0),
  }),

  -- add_subdirectory
  s({
    trig = "add_subdirectory",
    dscr = "add_subdirectory",
    condition = is_first_word_of_line,
    show_condition = is_first_word_of_line,
  }, {
    t("add_subdirectory("),
    i(1, "src"),
    t(")"),
    i(0),
  }),
}

