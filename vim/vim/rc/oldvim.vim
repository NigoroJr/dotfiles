set backspace=indent,eol,start
set encoding=utf-8
set history=10000
set hlsearch
" blowfish2 requires Vim 7.4.399. Encryption available after Vim 7.3
if HasVersion('7.3')
  execute 'set cryptmethod='.(HasVersion('7.4.399') ? 'blowfish2' : 'blowfish')
endif

" neocomplete.vim {{{
if neobundle#tap('neocomplete.vim')
  function! neobundle#hooks.on_source(bundle)
    inoremap <expr> <C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr> <BS> neocomplete#smart_close_popup()."\<BS>"

    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#min_keyword_length = 3

    let g:neocomplete#force_omni_input_patterns = {
          \ 'cpp': '[^. *\t]\%(\.\|->\)\w*\|[A-Za-z>]\w*::\w*',
          \ 'go': '[^.[:digit:] *\t]\.\w*',
          \ 'java': '[^.[:digit:] *\t]\.\w*',
          \ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
          \ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
          \ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
          \ 'ruby' : '[^. *\t]\.\w*\|\h\w*::',
          \ 'tex': '\v\\\a*(ref|cite)\a*([^]]*\])?\{([^}]*,)*[^}]*',
          \ }

    " Note: This is taking advantage of the fact that neocomplete.vim is
    " lazy-loaded when it goes into insert mode, thus, will be sourced after
    " vim-rails is sourced.
    if exists('b:rails_root') && exists('g:neocomplete#force_omni_input_patterns.ruby')
      call remove(g:neocomplete#force_omni_input_patterns, 'ruby')
    endif
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    call neocomplete#initialize()
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-javacomplete2 {{{
if neobundle#tap('vim-javacomplete2')
  function! neobundle#hooks.on_source(bundle)
    autocmd FileType java set omnifunc=javacomplete#Complete
  endfunction

  call neobundle#untap()
endif
" }}}
