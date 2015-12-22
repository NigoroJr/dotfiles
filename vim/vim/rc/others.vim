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
" vimshell.vim {{{
if neobundle#tap('vimshell.vim')
  " VimShell with ,vs
  nnoremap <silent> <Leader>vs :VimShellBuffer -split-command=vsplit<CR>
  " Open VimShell vertically
  nnoremap <silent> <Leader>vvs :VimShellBuffer -split-command=split<CR>
  " Interactive python with ,py
  nnoremap <silent> <Leader>vp :VimShellInteractive python<CR>
  nnoremap <silent> <Leader>vr :VimShellInteractive irb<CR>

  function! neobundle#hooks.on_source(bundle)
    " Hit escape twice to exit vimshell
    autocmd FileType vimshell imap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
    autocmd FileType vimshell nmap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
    " Enter in normal mode goes into insertion mode first
    autocmd FileType vimshell nmap <silent> <buffer> <CR> A<CR>

    " Set user prompt to pwd
    function! GetShortenedCWD() abort
      " Use ~ for $HOME
      return substitute(getcwd(), '^'.$HOME, '~', '')
    endfunction
    let g:vimshell_prompt_expr = 'GetShortenedCWD()." > "'
    let g:vimshell_prompt_pattern = '^\f\+ > '
  endfunction

  call neobundle#untap()
endif
" }}}
" vinarise.vim {{{
if neobundle#tap('vinarise.vim')
  let g:vinarise_enable_auto_detect = 1

  " Source when -b option is specified when starting Vim
  if &binary
    call neobundle#source('vinarise.vim')
  endif

  call neobundle#untap()
endif
" }}}
" vim-fugitive {{{
if neobundle#tap('vim-fugitive')
  function! neobundle#hooks.on_post_source(bundle)
    call fugitive#detect(resolve(expand('%')))
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-gista {{{
if neobundle#tap('vim-gista')
  function! neobundle#hooks.on_source(bundle)
    let g:gista#github_user = 'NigoroJr'
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-niceblock {{{
if neobundle#tap('vim-niceblock')
  xmap I <Plug>(niceblock-I)
  xmap A <Plug>(niceblock-A)

  call neobundle#untap()
endif
" }}}
" vim-ref {{{
if neobundle#tap('vim-ref')
  nmap K <Plug>(ref-keyword)

  function! neobundle#hooks.on_source(bundle)
    let g:ref_detect_filetype = {
          \ 'sh': 'man',
          \ 'zsh': 'man',
          \ 'bash': 'man',
          \ }
    let g:ref_use_vimproc = 1
  endfunction

  call neobundle#untap()
endif
" }}}
