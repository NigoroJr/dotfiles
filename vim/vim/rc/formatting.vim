" braceless.vim {{{
if neobundle#tap('braceless.vim')
  function! neobundle#hooks.on_source(bundle)
    autocmd FileType python BracelessEnable +indent +fold
  endfunction

  call neobundle#untap()
endif
" }}}
" matchit.zip {{{
if neobundle#tap('matchit.zip')
  function! neobundle#hooks.on_post_source(bundle)
    silent! execute 'doautocmd FileType' &filetype
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-better-whitespace {{{
if neobundle#tap('vim-better-whitespace')
  function! neobundle#hooks.on_source(bundle)
    let g:better_whitespace_filetypes_blacklist = [
          \ 'unite', 'help', 'vimshell', 'git', 'mail'
          \ ]
    let g:current_line_whitespace_disabled_soft = 1
    let g:current_line_whitespace_disabled_hard = 0
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-jplus {{{
if neobundle#tap('vim-jplus')
  nmap J <Plug>(jplus)
  nmap <Leader>J <Plug>(jplus-getchar)
  nmap <D_Leader>J <Plug>(jplus-input)

  function! neobundle#hooks.on_source(bundle)
    let g:jplus#input_config = {
          \ '__DEFAULT__': {
          \   'delimiter_format': ' %d ',
          \ },
          \ ',': {
          \   'delimiter_format': '%d, ',
          \ },
          \ }
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-surround {{{
if neobundle#tap('vim-surround')
  let g:surround_no_mappings = 1
  let g:surround_no_insert_mappings = 1

  " Only enable some key bindings
  nmap <Leader>sy <Plug>Ysurround
  nmap <Leader>sd <Plug>Dsurround
  nmap <Leader>sc <Plug>Csurround

  call neobundle#untap()
endif
" }}}
