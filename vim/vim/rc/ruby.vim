" rsense {{{
if neobundle#tap('rsense')
  function! neobundle#hooks.on_source(bundle)
    let g:rsenseUseOmniFunc = 1

    " Generate ~/.rsense if it doesn't exist
    if !filereadable(expand('~/.rsense'))
      let config_rb = '~/.vim/bundle/rsense/etc/config.rb'
      call system('ruby '.config_rb.' > ~/.rsense')
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-monster {{{
if neobundle#tap('vim-monster')
  function! neobundle#hooks.on_source(bundle)
    let g:monster#completion#rcodetools#backend = 'async_rct_complete'
    set updatetime=100
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-rails {{{
if neobundle#tap('vim-rails')
  function! neobundle#hooks.on_source(bundle)
    command! Rtree NERDTreeFind

    function! UniteInRails(source)
      if exists('b:rails_root')
        execute 'Unite' a:source . ':' . b:rails_root
      else
        execute 'Unite' a:source
      endif
    endfunction

    nmap <silent> <Leader>uf :call UniteInRails('file_rec/async')<CR>
    nmap <silent> <Leader>ug :call UniteInRails('grep')<CR>
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    " Don't overrwrite omnifunc
    " Also, omni complete is disabled in neocomplete.vim
    if exists('b:rails_root') && &filetype == 'ruby'
      if neobundle#is_installed('rsense')
        NeoBundleDisable rsense
        call neobundle#config('rsense', { 'disabled': 1 })
      endif
      if neobundle#is_installed('vim-monster')
        NeoBundleDisable vim-monster
        call neobundle#config('vim-monster', { 'disabled': 1 })
      endif
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
