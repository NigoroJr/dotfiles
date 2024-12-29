local cmp = require("cmp")
local luasnip = require("luasnip")

cmp.setup({
  snippet = {
    expand = function(args)
      luasnip.lsp_expand(args.body)
    end,
  },
  window = {
    -- completion = cmp.config.window.bordered(),
    documentation = cmp.config.window.bordered(),
  },
  preselect = cmp.PreselectMode.None,
  mapping = cmp.mapping.preset.insert({
    ["<C-y>"] = cmp.mapping.scroll_docs(-1),
    ["<C-e>"] = cmp.mapping.scroll_docs(1),
    ["<C-u>"] = cmp.mapping.scroll_docs(-14),
    ["<C-d>"] = cmp.mapping.scroll_docs(14),
    ["<A-b>"] = cmp.mapping.scroll_docs(-28),
    ["<A-f>"] = cmp.mapping.scroll_docs(28),
    ["<C-g>"] = cmp.mapping.abort(),
    ["<CR>"] = cmp.mapping(
      function(fallback)
        if cmp.visible() then
          cmp.confirm({ select = true })
        else
          fallback()
        end
      end
    ),
    ["<C-l>"] = cmp.mapping(
      function(fallback)
        if luasnip.locally_jumpable(1) then
          luasnip.jump(1)
        else
          fallback()
        end
      end
    ),
    ["<C-k>"] = cmp.mapping(
      function(fallback)
        if luasnip.expandable() then
          luasnip.expand()
        else
          fallback()
        end
      end
    ),
    ["<Tab>"] = cmp.mapping(
      function(fallback)
        if cmp.visible() then
          cmp.confirm({ select = true })
        elseif luasnip.locally_jumpable(1) then
          luasnip.jump(1)
        elseif luasnip.expandable() then
          luasnip.expand()
        else
          fallback()
        end
      end,
      {
        "i",
        c = cmp.config.disable,
        "s",
      }
    ),
    ["<S-Tab>"] = cmp.mapping(
      function(fallback)
        if luasnip.locally_jumpable(-1) then
          luasnip.jump(-1)
        -- elseif cmp.visible() then
        --   cmp.select_prev_item()
        else
          fallback()
        end
      end,
      {
        "i",
        c = cmp.config.disable,
        "s",
      }
    ),
  }),
  sources = cmp.config.sources(
    {
      { name = "nvim_lsp" },
      { name = "luasnip" },
    },
    {
      { name = "buffer" },
    }
  ),
})

cmp.setup.cmdline(":", {
  mapping = {
    ["<Tab>"] = {
      c = function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        else
          fallback()
        end
      end,
    },
    ["<S-Tab>"] = {
      c = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        else
          fallback()
        end
      end,
    },
    ["<A-n>"] = {
      c = function(fallback)
        if cmp.visible() then
          cmp.select_next_item()
        else
          fallback()
        end
      end,
    },
    ["<A-p>"] = {
      c = function(fallback)
        if cmp.visible() then
          cmp.select_prev_item()
        else
          fallback()
        end
      end,
    },
    ["<C-g>"] = {
      c = cmp.mapping.abort(),
    },
    ["<CR>"] = {
      c = function(fallback)
        if cmp.visible() then
          -- Select, then send <CR>
          cmp.confirm({ select = false })
        end

        fallback()
      end
    },
  },
  sources = cmp.config.sources(
    {
      { name = "path" },
    },
    {
      {
        name = "cmdline",
        keyword_length = 2,
      }
    }
  ),
  matching = { disallow_symbol_nonprefix_matching = false },
})
