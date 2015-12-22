" vim-ambicmd {{{
if neobundle#tap('vim-ambicmd')
  function! neobundle#hooks.on_source(bundle)
    cnoremap <expr> <C-d> ambicmd#expand("\<C-d>")
    cnoremap <expr> <Space> ambicmd#expand("\<Space>")
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-textobj-between {{{
if neobundle#tap('vim-textobj-between')
  omap a<Bar> <Plug>(textobj-between-a)<Bar>
  omap i<Bar> <Plug>(textobj-between-i)<Bar>
  call neobundle#untap()
endif
" }}}
" vim-textobj-line {{{
if neobundle#tap('vim-textobj-line')
  omap al <Plug>(textobj-line-a)
  omap il <Plug>(textobj-line-i)

  call neobundle#untap()
endif
" }}}
" vim-textobj-user {{{
if neobundle#tap('vim-textobj-user')

  call neobundle#untap()
endif
" }}}
