" Returns the gem paths for rbenv {{{
function! s:get_ruby_gems() abort
  if &filetype != 'ruby' || !executable('rbenv')
    return
  endif

  let current_version = split(system('rbenv version'), ' ')[0]
  let gem_path = expand('~/.rbenv/versions/'.current_version.'/lib/ruby/gems/*/gems/')
  let dirs = split(globpath(gem_path, '*'), '\n')
  call map(dirs, 'v:val."/lib"')
  return join(dirs, ',')
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
  let g:jedi#popup_select_first = 0
  let g:jedi#completions_enabled = 0
  let g:jedi#auto_vim_configuration = 0

  function! neobundle#hooks.on_source(bundle)
    autocmd FileType python setlocal omnifunc=jedi#completions
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplcache.vim {{{
if neobundle#tap('neocomplcache.vim')
  function! neobundle#hooks.on_source(bundle)
    let g:neocomplcache#enable_at_startup = 1
    let g:neocomplcache#enable_smart_case = 1
    let g:neocomplcache#min_keyword_length = 3

    inoremap <expr> <C-h> neocomplcache#smart_close_popup()."\<C-h>"
    inoremap <expr> <BS> neocomplcache#smart_close_popup()."\<BS>"
    inoremap <expr> <C-y> neocomplcache#close_popup()
    inoremap <expr> <C-g> neocomplcache#cancel_popup()

    let g:neocomplcache#force_omni_input_patterns = {
          \ 'go': '[^.[:digit:] *\t]\.\w*',
          \ 'cpp': '[^. *\t]\%(\.\|->\)\w*\|\h\w*::\w*',
          \ 'java': '[^.[:digit:] *\t]\.\w*',
          \ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
          \ 'ruby': '[^. *\t]\.\w*\|\h\w*::',
          \ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
          \ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
          \ }

    if !exists('g:neocomplcache#sources#include#paths')
      let g:neocomplcache#sources#include#paths = {}
    endif
    let g:neocomplcache#sources#include#paths.ruby = s:get_ruby_gems()

    " Note: This is taking advantage of the fact that neocomplcache.vim is
    " lazy-loaded when it goes into insert mode, thus, will be sourced after
    " vim-rails is sourced.
    if exists('b:rails_root')
      call remove(g:neocomplcache#force_omni_input_patterns, 'ruby')
    endif
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    call neocomplcache#initialize()
  endfunction

  call neobundle#untap()
endif
" }}}
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
          \ 'tex': '\v\\\a*(ref|cite)\a*([^]]*\])?\{([^}]*,)*[^}]*',
          \ }

    if g:use_rsense
      let g:neocomplete#force_omni_input_patterns.ruby =
            \ '[^. *\t]\.\w*\|\h\w*::'
    else
      let g:neocomplete#sources#omni#input_patterns = {
            \   "ruby" : '[^. *\t]\.\w*\|\h\w*::',
            \}
    endif

    if !exists('g:neoinclude#paths')
      let g:neoinclude#paths = {}
    endif
    let g:neoinclude#paths.ruby = s:get_ruby_gems()

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
" vim-eclim {{{
if neobundle#tap('vim-eclim')
  function! neobundle#hooks.on_source(bundle)
    let g:EclimCompletionMethod = 'omnifunc'
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
" vim-javacomplete2 {{{
if neobundle#tap('vim-javacomplete2')
  function! neobundle#hooks.on_source(bundle)
    autocmd FileType java set omnifunc=javacomplete#Complete
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
