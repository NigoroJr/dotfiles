" Mappings
function! s:denite_settings() abort
  nnoremap <silent><buffer><expr> <CR>
        \ denite#do_map("do_action")
  nnoremap <silent><buffer><expr> d
        \ denite#do_map("do_action", "delete")
  nnoremap <silent><buffer><expr> p
        \ denite#do_map("do_action", "preview")
  nnoremap <silent><buffer><expr> <C-g>
        \ denite#do_map("quit")
  nnoremap <silent><buffer><expr> Q
        \ denite#do_map("quit")
  nnoremap <silent><buffer><expr> i
        \ denite#do_map("open_filter_buffer")
  nnoremap <silent><buffer><expr> a
        \ denite#do_map("open_filter_buffer")
  nnoremap <silent><buffer><expr> <Space>
        \ denite#do_map("toggle_select")."j"
endfunction
autocmd FileType denite call s:denite_settings()

function! s:denite_filter_settings() abort
  call deoplete#custom#buffer_option("auto_complete", v:false)

  imap <silent><buffer> <Esc> <Plug>(denite_filter_quit)
  imap <silent><buffer> <C-g> <Plug>(denite_filter_quit)

  imap <silent><buffer><expr> <CR> denite#do_map("do_action")

  inoremap <silent><buffer> <C-p> <Esc>
        \:call denite#move_to_parent()<CR>
        \:call cursor(line(".")-1,0)<CR>
        \:call denite#move_to_filter()<CR>A
  inoremap <silent><buffer> <C-n> <Esc>
        \:call denite#move_to_parent()<CR>
        \:call cursor(line(".")+1,0)<CR>
        \:call denite#move_to_filter()<CR>A
endfunction
autocmd FileType denite-filter call s:denite_filter_settings()

" file/rec
call denite#custom#var("file/rec", "command",
      \ ["ag", "--follow", "--nocolor", "--nogroup", "-g", ""])
call denite#custom#source("file/rec",
      \ "matchers", ["matcher/fruzzy"])
call denite#custom#source("file/rec",
      \ "sorters", ["sorter/sublime"])

" file_mru
call denite#custom#source("file_mru",
      \ "matchers", ["matcher/substring"])

" grep
call denite#custom#var("grep", {
      \ "command": ["ag"],
      \ "default_opts": ["-i", "--vimgrep"],
      \ "recursive_opts": [],
      \ "pattern_opt": [],
      \ "separator": ["--"],
      \ "final_opts": [],
      \ })

" Change ignore_globs
call denite#custom#filter("matcher/ignore_globs",
      \ "ignore_globs",
      \ [ ".git/", ".ropeproject/", "__pycache__/",
      \   "venv/", "images/", "*.min.*", "img/", "fonts/"])
