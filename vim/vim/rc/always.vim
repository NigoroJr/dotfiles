" neobundle.vim {{{
if neobundle#tap('neobundle.vim')
  let g:neobundle#types#git#enable_submodule = 1
  "let g:neobundle#install_process_timeout = 1500

  " Smart cache clearing
  function! s:clear_cache() abort
    if neobundle#is_sourced('vim-marching')
      MarchingBufferClearCache
      let what = 'marching.vim'
    elseif neobundle#is_sourced('rsense')
      RSenseClear
      let what = 'rsense'
    else
      NeoBundleClearCache
      let what = 'neobundle.vim'
    endif
    echo 'Cleared cache: '.what
  endfunction

  nmap <silent> <Leader>cc :call <SID>clear_cache()<CR>

  call neobundle#untap()
endif
" }}}
