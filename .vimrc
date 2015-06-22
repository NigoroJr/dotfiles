" Don't go further if feature size is tiny or small
if 0 | endif

" Setup runtimepath on start {{{
if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
" }}}
" Function to check current Vim's version {{{
function! s:has_version(ver_str)
  let split_version = split(a:ver_str, '\.')
  let major = get(split_version, 0, 0)
  let minor = get(split_version, 1, 0)
  let patch = get(split_version, 2, 0)

  let min_version = major * 100 + minor
  return v:version > min_version ||
        \ (patch != 0 && v:version == min_version && has('patch'.patch))
endfunction
" }}}
set backup
set backupdir=~/.vim/backups/
set viewdir=~/.vim/view/
" Persistent undo requires Vim 7.3+
if s:has_version('7.3')
  set undodir=~/.vim/undo/
  set undofile
endif
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set shiftround
set textwidth=78
set number
set wildmode=longest,list
set foldenable
set foldmethod=marker
set history=10000
set viewoptions=cursor,folds
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932,guess
set backspace=indent,eol,start
set hlsearch
set splitright
set splitbelow
set modeline
set completeopt-=preview
set autoread
set showtabline=0
" blowfish2 requires Vim 7.4.399. Encryption available after Vim 7.3
if s:has_version('7.3')
  execute 'set cryptmethod='.(s:has_version('7.4.399') ? 'blowfish2' : 'blowfish')
endif
let mapleader = ','
noremap \ ,
syntax on
" Jump to matching keyword
runtime macros/matchit.vim

" Global variables {{{
let g:use_rsense = 0
let g:use_vim_clang = 0
let g:show_startup_time = 1
let g:case_insensitive_cmd = 0
" Disable netrw
let g:loaded_netrwPlugin = 1
" }}}
" Measure startup time in milliseconds {{{
if exists('g:show_startup_time') && g:show_startup_time
      \ && has('vim_starting') && has('reltime')
  function! s:show_elapsed_millisec(start) abort
    if &filetype == 'man'
      return
    end

    let duration = str2float(reltimestr(reltime(a:start)))
    let duration = duration * 1000
    echomsg string(duration)
  endfunction

  let s:start = reltime()
  autocmd VimEnter * call s:show_elapsed_millisec(s:start)
end
" }}}
" Filetype-specific text properties {{{
autocmd FileType text,vimshell setlocal textwidth=0
autocmd FileType ruby,html,xhtml,eruby,vim,toml setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType python,scss setlocal shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType go setlocal noexpandtab
autocmd FileType man setlocal nonumber noexpandtab shiftwidth=8 tabstop=8 softtabstop=8
autocmd FileType help syntax clear helpNote
autocmd BufNewFile,BufRead *.tex setlocal filetype=tex
" }}}
" General key bindings   {{{
" Move by physical/logical line
nnoremap gj j
nnoremap gk k
nnoremap j gj
nnoremap k gk

" More powerful ex command history
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" Shell key bindings in ex mode
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
" NOTE: Meta keys do not work in terminal Vim
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-g> <C-c>

" Use modifiers for substitution
nnoremap & :&&<CR>
xnoremap & :&&<CR>

" Save as super user
nnoremap <Leader>ws :w sudo:%<CR>
nnoremap <Leader>xs :x sudo:%<CR>
au BufWriteCmd  sudo:*,sudo:*/* SudoWrite <afile>
au FileWriteCmd sudo:*,sudo:*/* SudoWrite <afile>

" Reset highlight search by pressing Escape 2 times
nnoremap <silent> <Esc><Esc> :<C-u>nohlsearch<CR>

" Move pwd to directory of buffer
nnoremap <silent> <Leader>cdc :cd %:h<CR>
nnoremap <silent> <Leader>cdl :lcd %:h<CR>

" Q to quit (overrides 'Q', but this is the same as 'gq')
nmap <silent> Q :q<CR>

" Key bindings for tab operations
" Go to specefic tab
function! s:goto_tab() abort
  let tab_num = nr2char(getchar())
  silent exec ':tabnext '.tab_num
endfunction
nnoremap [tab] <Nop>
nmap <Leader>t [tab]
nnoremap <silent> [tab]g :call <SID>goto_tab()<CR>
nnoremap <silent> [tab]s :tabs<CR>
nnoremap <silent> [tab]n :tabnext<CR>
nnoremap <silent> [tab]p :tabprevious<CR>
nnoremap <silent> [tab]f :tabfirst<CR>
nnoremap <silent> [tab]l :tablast<CR>
nnoremap <silent> [tab]w :tabnew<CR>

" Function to toggle variables and show message
function! s:toggle(var, mes) abort
  silent exec 'setlocal '.a:var.'!'
  echo a:mes.' '.(eval('&'.a:var) ? 'ON' : 'OFF')
endfunction

nmap <silent> <Leader>ss :call <SID>toggle('spell', 'Spell')<CR>
nmap <silent> <Leader>sp :call <SID>toggle('paste', 'Paste')<CR>
nmap <silent> <Leader>sb :call <SID>toggle('scrollbind', 'Scrollbind')<CR>
nmap <silent> <Leader>set :call <SID>toggle('expandtab', 'expandtab')<CR>

" Toggle diff
function! s:toggle_diff() abort
  if &diff
    diffoff
  else
    diffthis
  end
  echo 'Diff '.(&diff ? 'ON' : 'OFF')
endfunction
" Note that <Leader>sd is mapped to surround.vim <Plug>Dsurround
nmap <silent> <Leader>dt :call <SID>toggle_diff()<CR>
" }}}
" Case-sensitive search, case-insensitive command completion  {{{
if exists('g:case_insensitive_cmd') && g:case_insensitive_cmd
  nnoremap : :call IgnoreCase()<CR>:
  function! IgnoreCase()
    set ignorecase
  endfunction
  nnoremap / :call NoIgnoreCase()<CR>/
  function! NoIgnoreCase()
    set noignorecase
  endfunction
endif
" }}}
" Mapping for inserting closing curly brace {{{
function! s:closing_brace_mapping()
  if &filetype == 'vim'
    inoremap {<CR> {<CR>\<Space>}<Esc>O\<Space>
  " Don't do this for LaTeX documents
  elseif &filetype !~ 'tex\|plaintex'
    inoremap {<CR> {<CR>}<Esc>O
  endif
  " Otherwise, no mapping is done
endfun
autocmd FileType * call <SID>closing_brace_mapping()
" }}}
" C++ {{{
" Snippets
augroup cpp-namespace
  autocmd!
  autocmd FileType cpp inoremap <buffer> <expr> ; <SID>expand_namespace()
  autocmd FileType cpp inoremap <buffer> <expr> {<CR> <SID>class_declaration()
augroup END
function! s:expand_namespace()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<b;'
    return "\<BS>oost::"
  elseif s =~# '\<s;' && s[col('.')-2] != 's'
    return "\<BS>td::"
  elseif s =~# '\<d;'
    return "\<BS>etail::"
  else
    return ';'
  endif
endfunction

function! s:class_declaration()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<class\>' ||
        \ (s =~# '\<struct\>' && s !~# '\<typedef\>')
    return "{\<CR>};\<Esc>O"
  endif

  " Otherwise insert closing '}'
  return "{\<CR>}\<Esc>O"
endfunction

" Include paths
let g:cpp_include_paths = filter(
      \ split(glob('/usr/lib/gcc/x86_64-pc-linux-gnu/*/include/*/'), '\n') +
      \ split(glob('/usr/include/boost/'), '\n') +
      \ split(glob('/usr/include/c++/*'), '\n'),
      \ 'isdirectory(v:val)')
" }}}
" Create backup view, and undo directories {{{
if !isdirectory(expand(&backupdir))
  call mkdir(&backupdir)
endif
if !isdirectory(expand(&viewdir))
  call mkdir(&viewdir)
endif
if !isdirectory(expand(&undodir)) && s:has_version('7.3')
  call mkdir(&undodir)
endif
" }}}
" Set filetypes to not save/load the view {{{
autocmd FileType gitcommit autocmd! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
let no_view = ['gitcommit', 'tex', 'plaintex']
augroup vimrc
  autocmd BufWritePost *
        \   if expand('%') != '' && &buftype !~ 'nofile' && index(no_view, &filetype) == -1
        \|      mkview!
        \|  endif
  autocmd BufRead *
        \   if expand('%') != '' && &buftype !~ 'nofile' && index(no_view, &filetype) == -1
        \|      silent loadview
        \|  endif
augroup END
" }}}
" Whether neocomplete.vim can be run on Vim {{{
function! NeoCompleteCompatible()
  " Vim must be compiled with lua interface AND version larger than 7.3.855
  return has('lua') && s:has_version('7.3.855')
endfunction
" }}}
" Make scripts executable if it's a script {{{
function! s:make_executable(filename)
  let line = getline(1)
  if line =~ '^#!' && line =~ '/bin'
    execute 'silent !chmod a+x' a:filename
    filetype detect
  endif
endfunction
autocmd BufWritePost * call s:make_executable(@%)
" }}}

" Clone neobundle.vim if not installed {{{
if !isdirectory(expand('~/.vim/bundle/neobundle.vim')) && executable('git')
  let url = 'https://github.com/Shougo/neobundle.vim'
  let dest = '~/.vim/bundle/neobundle.vim'
  let cmd = 'git clone '.url.' '.dest
  call system(cmd)
endif
" }}}

" Init neobundle.vim {{{
call neobundle#begin(expand('~/.vim/bundle/'))

if neobundle#load_cache()
  NeoBundleFetch 'davidhalter/jedi'
  NeoBundleFetch 'Shougo/neobundle.vim'

  call neobundle#load_toml(expand('~/.vim/NeoBundle.toml'))
  call neobundle#load_toml(expand('~/.vim/NeoBundleLazy.toml'), {'lazy': 1})

  NeoBundleSaveCache
end

call neobundle#local(expand('~/src/local_plugins'))

call neobundle#end()
filetype plugin indent on
if has('vim_starting')
    NeoBundleCheck
end
" }}}

" Configurations for individual plugins {{{
" clever-f.vim {{{
if neobundle#tap('clever-f.vim')
  let g:clever_f_fix_key_direction = 0
  let g:clever_f_chars_match_any_signs = ''
  let g:clever_f_across_no_line = 1

  call neobundle#untap()
endif
" }}}
" context_filetype.vim {{{
if neobundle#tap('context_filetype.vim')
  function! neobundle#hooks.on_source(bundle)
    let g:context_filetype#search_offset = 150
  endfunction

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
    end
    echo 'Cleared cache: '.what
  endfunction

  nmap <silent> <Leader>cc :call <SID>clear_cache()<CR>

  call neobundle#untap()
endif
" }}}
" neocomplcache.vim {{{
if neobundle#tap('neocomplcache.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'insert': !NeoCompleteCompatible(),
        \ },
        \ })

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
          \ 'cpp': '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*',
          \ 'java': '\%(\h\w*\|)\)\.\w*',
          \ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
          \ 'ruby': '[^. *\t]\.\w*\|\h\w*::',
          \ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
          \ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
          \ }

    " Note: This is taking advantage of the fact that neocomplcache.vim is
    " lazy-loaded when it goes into insert mode, thus, will be sourced after
    " vim-rails is sourced.
    if exists('b:rails_root')
      call remove(g:neocomplcache#force_omni_input_patterns, 'ruby')
    end
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    call neocomplcache#initialize()
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplcache-rsense.vim {{{
if neobundle#tap('neocomplcache-rsense.vim')
  function! neobundle#hooks.on_source(bundle)
    let g:neocomplcache#sources#rsense#home_directory = g:rsenseHome
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplete.vim {{{
if neobundle#tap('neocomplete.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'insert': NeoCompleteCompatible(),
        \ },
        \ })

  function! neobundle#hooks.on_source(bundle)
    inoremap <expr> <C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr> <BS> neocomplete#smart_close_popup()."\<BS>"
    inoremap <expr> <C-y> neocomplete#close_popup()
    inoremap <expr> <C-g> neocomplete#cancel_popup()

    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#min_keyword_length = 3

    let g:neocomplete#force_omni_input_patterns = {
          \ 'go': '[^.[:digit:] *\t]\.\w*',
          \ 'cpp': '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*',
          \ 'java': '\%(\h\w*\|)\)\.\w*',
          \ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
          \ 'ruby': '[^. *\t]\.\w*\|\h\w*::',
          \ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
          \ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
          \ }

    " Note: This is taking advantage of the fact that neocomplete.vim is
    " lazy-loaded when it goes into insert mode, thus, will be sourced after
    " vim-rails is sourced.
    if exists('b:rails_root')
      call remove(g:neocomplete#force_omni_input_patterns, 'ruby')
    end
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    call neocomplete#initialize()
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplete-rsense.vim {{{
if neobundle#tap('neocomplete-rsense.vim')
  function! neobundle#hooks.on_source(bundle)
    let g:neocomplete#sources#rsense#home_directory = g:rsenseHome
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
    end

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
" nerdtree {{{
if neobundle#tap('nerdtree')
  function! neobundle#hooks.on_source(bundle)
    " Don't use nerdtree when opening directory (use vimfiler.vim)
    if neobundle#is_sourced('vimfiler.vim')
      let g:NERDTreeHijackNetrw = 0
    end
    let g:NERDTreeMinimalUI = 1
    let g:NERDTreeDirArrows = 0
    let g:NERDTreeMapToggleHidden = '.'
    let g:NERDTreeShowHidden = 0
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
" rsense {{{
if neobundle#tap('rsense')
  function! neobundle#hooks.on_source(bundle)
    let g:rsenseUseOmniFunc = 1

    " Generate ~/.rsense if it doesn't exist
    if !filereadable(expand('~/.rsense'))
      let config_rb = '~/.vim/bundle/rsense/etc/config.rb'
      call system('ruby '.config_rb.' > ~/.rsense')
    end
  endfunction

  call neobundle#untap()
endif
" }}}
" unite.vim {{{
if neobundle#tap('unite.vim')
  nnoremap [unite] <Nop>
  nmap <Leader>u [unite]
  nnoremap [unite]n :Unite<Space>
  nnoremap [unite]u :UniteWithBufferDir<Space>
  nnoremap [unite]gg :UniteWithBufferDir grep<CR>
  nnoremap [unite]gn :Unite grep:
  nnoremap [unite]b :Unite buffer<CR>
  nnoremap [unite]c :Unite command<CR>
  nnoremap [unite]f :Unite file_rec/async<CR>
  nnoremap [unite]l :Unite file/async<CR>
  nnoremap [unite]m :Unite neomru/file -buffer-name=mru -create<CR>
  nnoremap [unite]q :Unite quickfix -horizontal -no-quit<CR>
  nnoremap [unite]r :Unite register -buffer-name=register<CR>
  nnoremap [unite]t :Unite tab:no-current<CR>

  if neobundle#is_installed('unite-outline')
    nnoremap [unite]o :Unite outline<CR>
  end

  function! neobundle#hooks.on_source(bundle)
    call unite#custom#profile('default',
          \ 'context', {
          \   'start_insert': 1,
          \   'direction': 'botright',
          \   'prompt_direction': 'top',
          \   'auto_resize': 1,
          \ })

    call unite#custom#profile('source/file/async,source/file_rec/async',
          \ 'context', {
          \   'profile_name': 'files',
          \   'buffer_name': 'file',
          \   'create': 1,
          \ })

    " Case-insensitive command search
    call unite#custom#profile('source/command',
          \ 'context', {
          \   'smartcase': 1,
          \ })

    call unite#custom#profile('source/boost-online-doc',
          \ 'context', {
          \   'smartcase': 1,
          \ })

    call unite#custom#source('file_rec/async',
          \ 'matchers', [
          \   'matcher_hide_hidden_files',
          \   'matcher_hide_hidden_directories',
          \   'converter_relative_word',
          \   ])

    call unite#custom#source('command', 'matchers', 'matcher_fuzzy')

    let g:unite_source_alias_aliases = {
          \ 'fra': {
          \   'source': 'file_rec/async',
          \ },
          \ }

    " Use ag if available
    if executable('ag')
      let g:unite_source_grep_command = 'ag'
      let g:unite_source_grep_default_opts = '--nocolor --nogroup --line-numbers '
            \ . '--ignore ".git" --ignore ".svn" --ignore ".hg"'
    end

    autocmd FileType unite imap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
    " helm-like preview
    autocmd FileType unite imap <silent> <buffer> <C-z> <Esc><Plug>(unite_smart_preview)i
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-altr {{{
if neobundle#tap('vim-altr')
  nmap <Leader>a <Plug>(altr-forward)
  nmap <Leader>A <Plug>(altr-back)

  call neobundle#untap()
endif
" }}}
" vim-ambicmd {{{
if neobundle#tap('vim-ambicmd')
  function! neobundle#hooks.on_source(bundle)
    cnoremap <expr> <C-d> ambicmd#expand("\<C-d>")
    cnoremap <expr> <Space> ambicmd#expand("\<Space>")
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-clang {{{
if neobundle#tap('vim-clang')
  " This plugin is disabled when vimproc.vim and clang executable are
  " available, hence, when vim-marching can be used. The reason why
  " neobundle#is_installed('vim-marching') is not used for evaluating
  " 'disabled' is because neobundle#is_installed() returns undefined results
  " until finishing parsing the TOML file. The reasony why
  " neobundle#is_installed('vimproc.vim') works is because vimproc.vim is
  " stated in a different file (NeoBundle.toml), therefore, parsed before
  " NeoBundleLazy.toml. Care must be taken to ensure that neobundle.vim can
  " see vimproc.vim before parsing the vim-clang configs in the TOML file.

  function! neobundle#hooks.on_source(bundle)
    let g:clang_cpp_options = '-std=c++11'
    let g:clang_c_completeopt = 'longest,menuone'
    let g:clang_cpp_completeopt = 'longest,menuone'
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
" vim-latex {{{
if neobundle#tap('vim-latex')
  function! neobundle#hooks.on_source(bundle)
    let g:latex_indent_enabled = 1
    let g:latex_motion_matchparen = 0
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    " Start auto-compile
    silent call latex#latexmk#compile()

    " Re-set shiftwidth and tapstop
    set shiftwidth=4 tabstop=4 softtabstop=4
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-marching {{{
if neobundle#tap('vim-marching')
  function! neobundle#hooks.on_source(bundle)
    let g:marching_include_paths = g:cpp_include_paths
    let g:marching_enable_neocomplete = 1
    let g:marching_clang_command = 'clang++'
    let g:marching#clang_command#options = {
          \ 'cpp': '-std=c++11',
          \ }

    set updatetime=200
    imap <C-x><C-o> <Plug>(marching_force_start_omni_complete)
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
" vim-monster {{{
if neobundle#tap('vim-monster')
  function! neobundle#hooks.on_source(bundle)
    let g:monster#completion#rcodetools#backend = 'async_rct_complete'
    set updatetime=100
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
" vim-quickrun {{{
if neobundle#tap('vim-quickrun')
  nmap <silent> <Leader>r <Plug>(quickrun)
  nmap <silent> <Leader>m :QuickRun make<CR>

  function! neobundle#hooks.on_source(bundle)
    call neobundle#source(['shabadou.vim', 'wandbox-vim', 'unite-quickfix'])

    nnoremap <Leader>wb :QuickRun -runner wandbox<CR>

    let g:quickrun_config = {
          \ '_': {
          \   'hook/time/enable': 0,
          \   'hook/close_unite_quickfix/enable_hook_loaded': 1,
          \   'hook/unite_quickfix/enable_failure': 1,
          \   'hook/close_quickfix/enable_exit': 1,
          \   'hook/close_buffer/enable_empty_data': 1,
          \   'hook/close_buffer/enable_failure': 1,
          \   'outputter': 'multi:buffer:quickfix',
          \   'outputter/buffer/close_on_empty': 1,
          \   'hook/quickfix_replace_tempname_to_bufnr/enable_exit': 1,
          \   'hook/quickfix_replace_tempname_to_bufnr/priority_exit': -10,
          \   'runmode': 'async:remote:vimproc',
          \   'runner': 'vimproc',
          \   'runner/vimproc/updatetime': 60,
          \ },
          \ 'make': {
          \   'command': 'make',
          \   'exec': "%c %o",
          \   'runner': 'vimproc',
          \ },
          \ 'watchdogs_checker/_': {
          \   'hook/close_quickfix/enable_exit': 1,
          \   'hook/unite_quickfix/enable': 0,
          \   'hook/close_unite_quickfix/enable_exit': 1,
          \ },
          \ }
    let g:quickrun_config.cpp = {
          \ 'command': 'g++',
          \ 'cmdopt': '-std=c++11 -Wall -Wextra',
          \ }
    " Open in browser using previm
    let g:quickrun_config.markdown = {
          \ 'runner': 'vimscript',
          \ 'exec': 'PrevimOpen',
          \ 'outputter': 'null',
          \ 'hook/time/enable': 0,
          \ }
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
    " Also, omni complete is disabled in neocomplete.vim and neocomplcache.vim
    if exists('b:rails_root') && &filetype == 'ruby'
      if neobundle#is_installed('rsense')
        NeoBundleDisable rsense
        call neobundle#config('rsense', { 'disabled': 1 })
      end
      if neobundle#is_installed('vim-monster')
        NeoBundleDisable vim-monster
        call neobundle#config('vim-monster', { 'disabled': 1 })
      end
    endif
  endfunction

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
" vim-seek {{{
if neobundle#tap('vim-seek')
  function! neobundle#hooks.on_source(bundle)
    let g:seek_ignorecase = 1
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-smartchr {{{
if neobundle#tap('vim-smartchr')
  function! s:smartchr_cpp()
    " Stream operators
    inoremap <expr> @ smartchr#loop('<< ', '>> ', '@')
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    if exists('*s:smartchr_'.&filetype)
      execute 'call s:smartchr_'.&filetype.'()'
    end
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-snowdrop {{{
if neobundle#tap('vim-snowdrop')
  function! neobundle#hooks.on_source(bundle)
    let g:snowdrop#libclang_directory = '/usr/lib/'
    let g:snowdrop#include_paths = {
          \ 'cpp': g:cpp_include_paths,
          \ }
    let g:neocomplete#sources#snowdrop#enable = 1
    let g:neocomplete#skip_auto_completion_time = ''
  endfunction

  call neobundle#untap()
endif
"}}}
" vim-stargate {{{
if neobundle#tap('vim-stargate')
  nmap <Leader>sg :StargateInclude<Space>

  function! neobundle#hooks.on_source(bundle)
    let g:stargate#include_paths = {
          \ 'cpp': g:cpp_include_paths,
          \ }
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-surround {{{
if neobundle#tap('vim-surround')
  let g:surround_no_mappings = 1
  let g:surround_no_insert_mappings = 1

  " Only enable some key bindings
  nmap <Leader>sy <Plug>Ysurround
  nmap <Leader>sd <Plug>Dsurround
  nmap <Leader>sc <Plug>Csurround

  call neobundle#untap()
endif
" }}}
" vim-textobj-user {{{
if neobundle#tap('vim-textobj-user')

  call neobundle#untap()
endif
" }}}
" vim-trailing-whitespace {{{
if neobundle#tap('vim-trailing-whitespace')
  function! neobundle#hooks.on_source(bundle)
    let g:extra_whitespace_ignored_filetypes = ['unite', 'help']
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-watchdogs {{{
if neobundle#tap('vim-watchdogs')
  function! neobundle#hooks.on_source(bundle)
    nmap <silent> <Leader>wd :WatchdogsRun<CR>
    let g:watchdogs_check_BufWritePost_enable = 1
    call watchdogs#setup(g:quickrun_config)
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
  endfunction

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
  end

  call neobundle#untap()
endif
" }}}
" wildfire.vim {{{
if neobundle#tap('wildfire.vim')
  map <silent> <CR> <Plug>(wildfire-fuel)
  vmap <silent> <BS> <Plug>(wildfire-water)

  call neobundle#untap()
endif
" }}}
" }}}

" Source local configurations if any {{{
if filereadable(expand('~/.localrc/vimrc'))
  source ~/.localrc/vimrc
endif
" }}}
