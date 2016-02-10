" clever-f.vim {{{
if neobundle#tap('clever-f.vim')
  let g:clever_f_fix_key_direction = 0
  let g:clever_f_chars_match_any_signs = ''

  call neobundle#untap()
endif
" }}}
" incsearch.vim {{{
if neobundle#tap('incsearch.vim')
  map / <Plug>(incsearch-forward)
  map ? <Plug>(incsearch-backward)
  map g/ <Plug>(incsearch-stay)

  map n  <Plug>(incsearch-nohl-n)
  map N  <Plug>(incsearch-nohl-N)
  map *  <Plug>(incsearch-nohl-*)
  map #  <Plug>(incsearch-nohl-#)
  map g* <Plug>(incsearch-nohl-g*)
  map g# <Plug>(incsearch-nohl-g#)

  function! neobundle#hooks.on_source(bundle)
    let g:incsearch#consistent_n_direction = 1
    let g:incsearch#emacs_like_keymap = 1
    let g:incsearch#auto_nohlsearch = 1
  endfunction

  call neobundle#untap()
endif
" }}}
" vimfiler.vim {{{
if neobundle#tap('vimfiler.vim')
  nnoremap <silent> <Leader>f :VimFilerBufferDir -status<CR>

  function! neobundle#hooks.on_source(bundle)
    " vim-rails maps <Leader>uf to file_rec/async:!
    if !neobundle#is_sourced('vim-rails')
      " Otherwise override ':Unite file_rec/async' if in vimfiler.vim
      autocmd FileType vimfiler nmap <buffer> <silent> <Leader>uf :UniteWithBufferDir file_rec/async<CR>
      autocmd FileType vimfiler nmap <buffer> <silent> <Leader>ul :UniteWithBufferDir file/async<CR>
    endif

    let g:vimfiler_as_default_explorer = 1
    let g:vimfiler_time_format = "%m/%d/%y %H:%M%S"

    autocmd FileType vimfiler nmap <buffer> e <Plug>(vimfiler_cd_or_edit)
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-rooter {{{
if neobundle#tap('vim-rooter')
  nmap <silent> <Leader>cdr <Plug>RooterChangeToRootDirectory

  function! neobundle#hooks.on_source(bundle)
    let g:rooter_disable_map = 1
    let g:rooter_use_lcd = 1
    let g:rooter_manual_only = 1
    let g:rooter_patterns = ['pom.xml', '.git', '.git/', '_darcs/', '.hg/', '.bzr/', '.svn/']
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-sneak {{{
if neobundle#tap('vim-sneak')
  function! neobundle#hooks.on_source(bundle)
    let g:sneak#s_next = 1
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-windowswap {{{
if neobundle#tap('vim-windowswap')
  nnoremap [window] <Nop>
  nmap <Leader>w [window]
  nnoremap <silent> [window]w :call WindowSwap#EasyWindowSwap()<CR>
  nnoremap <silent> [window]p :call WindowSwap#DoWindowSwap()<CR>
  nnoremap <silent> [window]y :call WindowSwap#MarkWindowSwap()<CR>

  function! neobundle#hooks.on_source(bundle)
    let g:windowswap_map_keys = 0
  endfunction

  call neobundle#untap()
endif
" }}}
