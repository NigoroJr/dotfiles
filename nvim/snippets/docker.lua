local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node

local utils = dofile(vim.fn.stdpath("config") .. "/snippets/utils.lua")
local is_beginning_of_file = utils.is_beginning_of_file
local is_beginning_of_line = utils.is_beginning_of_line

return {
  -- main
  s({
    trig = "main",
    dscr = "Main starting point",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t("FROM "),
    i(1, "ubuntu"),
    t({
      "",
      "",
      "ENV DEBIAN_FRONTEND=noninteractive",
      "",
      "RUN apt update \\",
      "\t&& apt install -y \\",
      "\t\t",
    }),
    i(2, "some-package"),
    t({
      " \\",
      "\t&& rm -rf /var/lib/apt/lists/*",
      "",
    }),
    i(0),
  }),

  -- starting point for ubuntu
  s({
    trig = "ubuntu",
    dscr = "Starting point for ubuntu",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t("FROM ubuntu:"),
    i(1, "latest"),
    t({
      "",
      "",
      "ENV DEBIAN_FRONTEND=noninteractive",
      "",
      "RUN apt update \\",
      "\t&& apt install -y \\",
      "\t\t",
    }),
    i(2, "some-package"),
    t({
      " \\",
      "\t&& rm -rf /var/lib/apt/lists/*",
      "",
    }),
  }),

  -- starting point for alpine
  s({
    trig = "alpine",
    dscr = "Starting point for alpine",
    condition = is_beginning_of_file,
    show_condition = is_beginning_of_file,
  }, {
    t("FROM alpine:"),
    i(1, "latest"),
    t({
      "",
      "",
      "RUN apk add --no-cache \\",
      "\t",
    }),
    i(2, "some-package"),
    t({ "", "" }),
  }),

  -- add user (ubuntu)
  s({
    trig = "user-ubuntu",
    dscr = "Add user (ubuntu)",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("RUN useradd -ms /bin/bash "),
    i(1, "user"),
    t({
      "",
      "",
      "USER ",
    }),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      "",
      "WORKDIR /home/",
    }),
    ls.dynamic_node(3, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({ "", "" }),
  }),

  -- add user (alpine)
  s({
    trig = "user-alpine",
    dscr = "Add user (alpine)",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("RUN adduser -D "),
    i(1, "user"),
    t({
      "",
      "",
      "USER ",
    }),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({ "", "" }),
  }),

  -- pip install
  s({
    trig = "pip",
    dscr = "pip install",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("RUN "),
    i(1, "python3"),
    t(" -m pip install --upgrade pip"),
    t({
      "",
      "RUN ",
    }),
    ls.dynamic_node(2, function(args)
      return ls.snippet_node(nil, {
        t(args[1][1]),
      })
    end, { 1 }),
    t({
      " -m pip install \\",
      "\t",
    }),
    i(3, "seaborn"),
    t({ "", "" }),
  }),

  -- sudoers
  s({
    trig = "sudoers",
    dscr = "Add user to sudoers",
    condition = is_beginning_of_line,
    show_condition = is_beginning_of_line,
  }, {
    t("RUN echo \""),
    i(1, "user"),
    t({ " ALL=(ALL:ALL) NOPASSWD: ALL\" >> /etc/sudoers", "" }),
  }),
}
