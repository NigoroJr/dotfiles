" Returns the gem paths for rbenv {{{
function! s:get_ruby_gems() abort
  if &filetype != 'ruby' || !executable('rbenv')
    return
  endif

  if HasVersion('7.4.256')
    let rbenv_prefix = systemlist('rbenv prefix')[0] . '/lib/ruby'
  else
    let rbenv_prefix = split(system('rbenv prefix'), '\n')[0] . '/lib/ruby'
  endif

  " stdlib is in the directory with the version number
  let stdlib_path = sort(split(expand(rbenv_prefix . '/*'), '\n'))[0]

  let gem_path = rbenv_prefix . '/gems/*/gems/'
  let dirs = split(globpath(gem_path, '*'), '\n')

  call map(dirs, 'v:val."/lib"')
  return stdlib_path . ',' . join(dirs, ',')
endfunction
" }}}

" emmet-vim {{{
if neobundle#tap('emmet-vim')
  function! neobundle#hooks.on_source(bundle)
    let g:user_emmet_complete_tag = 1
  endfunction

  call neobundle#untap()
endif
" }}}
" jedi-vim {{{
if neobundle#tap('jedi-vim')
  function! neobundle#hooks.on_source(bundle)
    let g:jedi#auto_initialization = 0
    let g:jedi#popup_select_first = 0
    let g:jedi#completions_enabled = 0
    let g:jedi#auto_vim_configuration = 0

    autocmd FileType python setlocal omnifunc=jedi#completions
  endfunction

  call neobundle#untap()
endif
" }}}
" neoinclude.vim {{{
if neobundle#tap('neoinclude.vim')
  function! neobundle#hooks.on_source(bundle)
    if !exists('g:neoinclude#paths')
      let g:neoinclude#paths = {}
    endif
    let g:neoinclude#paths.ruby = s:get_ruby_gems()

    if !exists('g:cpp_include_paths')
      execute 'source' expand('%:h') . '/cpp.vim'
    endif
    let include_paths = join(g:cpp_include_paths, ',')
    let g:neoinclude#paths.cpp = include_paths
  endfunction

  call neobundle#untap()
endif
" }}}
" neosnippet.vim {{{
if neobundle#tap('neosnippet.vim')
  nnoremap <Leader>es :<C-u>NeoSnippetEdit

  function! neobundle#hooks.on_source(bundle)
    " First directory has the local snippets.  Note that this risks local
    " snippets being overwritten by the global snippets loaded later. However,
    " it is set like this so that it's easier to add global snippets when doing
    " <Leader>es
    let g:neosnippet#snippets_directory = '~/.local_snippets/,~/.vim/snippets/'

    if !neobundle#is_installed('neosnippet-snippets')
      let g:neosnippet#disable_runtime_snippets = {
            \ '_': 1,
            \ }
    endif

    " <TAB>: completion
    imap <expr> <TAB> neosnippet#expandable_or_jumpable() ?
          \ "\<Plug>(neosnippet_expand_or_jump)" :
          \ pumvisible() ?
          \ "\<C-n>" : "\<TAB>"
    smap <expr> <TAB> neosnippet#expandable_or_jumpable() ?
          \ "\<Plug>(neosnippet_jump_or_expand)" :
          \ pumvisible() ?
          \ "\<C-n>" : "\<TAB>"

    imap <silent> <C-k> <Plug>(neosnippet_jump_or_expand)
    smap <silent> <C-k> <Plug>(neosnippet_jump_or_expand)
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-go {{{
if neobundle#tap('vim-go')
  function! neobundle#hooks.on_source(bundle)
    let g:go_highlight_trailing_whitespace_error = 0
    let g:go_highlight_operators = 0
    let g:go_fmt_autosave = 0

    nmap <silent> <Leader>gf :GoFmt<CR>
    nmap <Leader>gi :GoImport<Space>
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-smartchr {{{
if neobundle#tap('vim-smartchr')
  function! s:smartchr_cpp()
    " Stream operators
    inoremap <expr> @ smartchr#loop(' << ', ' >> ', '@')
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    if exists('*s:smartchr_'.&filetype)
      execute 'call s:smartchr_'.&filetype.'()'
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
