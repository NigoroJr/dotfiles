require("nvim-autopairs").setup({
  map_c_h = true,
  map_c_w = true,
  fast_wrap = {
    map = "<M-e>",
    chars = { "{", "[", "(", "<", "'", '"', },
    pattern = [=[[%'%"%>%]%)%}%,]]=],
    end_key = "$",
    before_key = "h",
    after_key = "l",
    cursor_pos_before = true,
    keys = "abcdefghijklmnopqrstuvwxyz",
    manual_position = true,
    highlight = "Search",
    highlight_grey="Comment"
  },
})
