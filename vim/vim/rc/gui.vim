" restart.vim {{{
if neobundle#tap('restart.vim')
  function! neobundle#hooks.on_source(bundle)
    let g:restart_sessionoptions
          \ = 'blank,buffers,curdir,folds,help,localoptions,tabpages'
    let g:restart_no_default_menus = 1
  endfunction

  call neobundle#untap()
endif
" }}}
