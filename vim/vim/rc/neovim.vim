" deoplete.nvim {{{
if neobundle#tap('deoplete.nvim')
  function! neobundle#hooks.on_source(bundle)
    inoremap <expr> <C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr> <BS> neocomplete#smart_close_popup()."\<BS>"

    let g:deoplete#enable_at_startup = 1
    let g:deoplete#enable_smart_case = 1
    let g:deoplete#min_keyword_length = 3

    let g:deoplete#omni_patterns = {
          \ 'c': '[^. *\t]\%(\.\|->\)\w*',
          \ 'cpp': '[^. *\t]\%(\.\|->\)\w*\|::\w*',
          \ 'go': '[^.[:digit:] *\t]\.\w*',
          \ 'java': '[^.[:digit:] *\t]\.\w*',
          \ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
          \ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
          \ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
          \ 'ruby': '[^. *\t]\.\w*\|\h\w*::',
          \ 'tex': '\v\\\a*(ref|cite)\a*([^]]*\])?\{([^}]*,)*[^}]*',
          \ }
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    call deoplete#enable()
  endfunction

  call neobundle#untap()
endif
" }}}
