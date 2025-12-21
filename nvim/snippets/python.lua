local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_line = utils.is_beginning_of_line

return {
  -- argparse
  s({
    trig = "argparse",
    dscr = "argparse",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({
      "import argparse",
      "",
      "",
      "def parse_args():",
      "\tdescription = \"",
    }),
    i(1),
    t({
      "\"",
      "\tparser = argparse.ArgumentParser(",
      "\t\tdescription=description,",
      "\t\tadd_help=False,",
      "\t)",
      "\tparser.add_argument(",
      "\t\t\"-h\",",
      "\t\t\"--help\",",
      "\t\taction=\"help\",",
      "\t\thelp=\"show this help message and exit\",",
      "\t)",
    }),
    i(0),
    t({
      "",
      "",
      "\targs = parser.parse_args()",
      "\treturn args",
      "",
    }),
  }),

  -- add_argument (for argparse)
  s({
    trig = "add_argument",
    dscr = "add_argument",
  }, {
    t({
      "parser.add_argument(",
      "\t\"",
    }),
    i(1, "arg"),
    t({
      "\",",
      "\tdefault=\"",
    }),
    i(2, "val"),
    t({
      "\",",
      "\tnargs=",
    }),
    i(3, "\"?\""),
    t({
      ",",
      "\thelp=\"",
    }),
    i(4, "stuff"),
    t({
      "\",",
      ")",
    }),
  }),

  -- add_argument_full (for argparse)
  s({
    trig = "add_argument_full",
    dscr = "add_argument",
  }, {
    t({
      "parser.add_argument(",
      "\t\"",
    }),
    i(1, "short"),
    t("\", \""),
    i(2, "long"),
    t({
      "\",",
      "\tdest=\"",
    }),
    i(3, "val"),
    t({
      "\",",
      "\tdefault=\"",
    }),
    i(4, "val"),
    t({
      "\",",
      "\ttype=",
    }),
    i(5, "int"),
    t({
      ",",
      "\taction=\"",
    }),
    i(6, "store"),
    t({
      "\",",
      "\tnargs=",
    }),
    i(7, "\"?\""),
    t({
      ",",
      "\tmetavar=\"",
    }),
    i(8, "FOO"),
    t({
      "\",",
      "\thelp=\"",
    }),
    i(9, "stuff"),
    t({
      "\",",
      ")",
    }),
  }),

  -- seaborn
  s({
    trig = "seaborn",
    dscr = "seaborn",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({
      "import numpy as np",
      "import pandas as pd",
      "import matplotlib.pyplot as plt",
      "import seaborn as sns",
      "",
      "sns.set(style=\"darkgrid\")",
      "",
    }),
    i(0),
  }),

  -- main
  s({
    trig = "main",
    dscr = "main",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({
      "def main():",
      "\t",
    }),
    i(0, "pass"),
    t({
      "",
      "",
      "",
      "if __name__ == \"__main__\":",
      "\tmain()",
    }),
  }),

  -- roslaunch
  s({
    trig = "roslaunch",
    dscr = "launch",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({
      "from launch import LaunchDescription",
      "from launch_ros.actions import Node",
      "",
      "",
      "def generate_launch_description():",
      "\treturn LaunchDescription(",
      "\t\t[",
      "\t\t\tNode(",
      "\t\t\t\tname=\"",
    }),
    i(3),
    t("\","),
    t({
      "",
      "\t\t\t\tpackage=\"",
    }),
    i(1),
    t("\","),
    t({
      "",
      "\t\t\t\texecutable=\"",
    }),
    i(2),
    t({
      "\",",
      "\t\t\t),",
    }),
    i(0),
    t({
      "",
      "\t\t]",
      "\t)",
    }),
  }),

  -- roslaunch-node
  s({
    trig = "roslaunch-node",
    dscr = "launch-node",
  }, {
    t({
      "Node(",
      "\tnamespace=\"",
    }),
    i(4),
    t("\","),
    t({
      "",
      "\tname=\"",
    }),
    i(3),
    t("\","),
    t({
      "",
      "\tpackage=\"",
    }),
    i(1),
    t("\","),
    t({
      "",
      "\texecutable=\"",
    }),
    i(2),
    t({
      "\",",
      "\tremappings=[",
      "\t\t(\"",
    }),
    i(5, "/from"),
    t("\", \""),
    i(6, "/to"),
    t({
      "\"),",
      "\t],",
      "),",
    }),
    i(0),
  }),

  -- super
  s({
    trig = "super",
    dscr = "super",
  }, {
    t("super("),
    i(1, "this_class"),
    t(", self)"),
    i(0),
  }),

  -- logging
  s({
    trig = "logging",
    dscr = "logging",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({
      "import logging",
      "",
      "formatter = logging.Formatter(",
      "\tfmt=\"[%(asctime)s : %(levelname)s] %(message)s\",",
      "\tdatefmt=\"%Y-%m-%d %H:%M:%S\",",
      ")",
      "handler = logging.StreamHandler()",
      "handler.setFormatter(formatter)",
    }),
    i(1, "logger"),
    t(" = logging.getLogger(__name__)"),
    t({
      "",
      "",
    }),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      ".addHandler(handler)",
      "",
    }),
    ls.dynamic_node(3, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t(".setLevel(logging.INFO)"),
  }),

  -- logging-rich
  s({
    trig = "logging-rich",
    dscr = "logging-rich",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t({
      "import logging",
      "from rich.logging import RichHandler",
      "",
      "logging.basicConfig(",
      "\tlevel=logging.INFO,",
      "\tformat=\"%(message)s\",",
      "\tdatefmt=\"[%Y-%m-%d %H:%M:%S]\",",
      "\thandlers=[RichHandler()],",
      ")",
    }),
    i(1, "logger"),
    t(" = logging.getLogger(__name__)"),
  }),
}
