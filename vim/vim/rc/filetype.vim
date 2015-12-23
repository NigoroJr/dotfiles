" context_filetype.vim {{{
if neobundle#tap('context_filetype.vim')
  function! neobundle#hooks.on_source(bundle)
    let g:context_filetype#search_offset = 150
  endfunction

  call neobundle#untap()
endif
" }}}
" previm {{{
if neobundle#tap('previm')
  function! neobundle#hooks.on_source(bundle)
    if filereadable(expand('~/Dropbox/Vim/previm.css'))
      let g:previm_custom_css_path = expand('~/Dropbox/Vim/previm.css')
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-latex {{{
if neobundle#tap('vim-latex')
  function! neobundle#hooks.on_source(bundle)
    let g:vimtex_indent_enabled = 1
    let g:vimtex_motion_matchparen = 0
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    VimtexCompile
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-markdown {{{
if neobundle#tap('vim-markdown')
  function! neobundle#hooks.on_source(bundle)
    let g:vim_markdown_no_default_key_mappings = 1
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-precious {{{
if neobundle#tap('vim-precious')
  function! neobundle#hooks.on_source(bundle)
    " Don't change the filetype because that will change the
    " shiftwidth, tabstop, and softtabstop and leave it after exiting
    let g:precious_enable_switchers = {
          \ '*': {
          \   'setfiletype': 0,
          \ },
          \ 'html': {
          \   'setfiletype': 1,
          \ },
          \ 'xhtml': {
          \   'setfiletype': 1,
          \ },
          \ }

    " Only change syntax
    augroup PreciousSyntaxChange
      autocmd!
      autocmd User PreciousFileType let &l:syntax = precious#context_filetype()
    augroup END
  endfunction

  call neobundle#untap()
endif
" }}}
