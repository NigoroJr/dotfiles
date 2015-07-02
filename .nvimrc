if has('vim_starting')
  set runtimepath+=~/.nvim/bundle/neobundle.vim/
endif

set autoread
set backspace=indent,eol,start
set backup
set backupdir=~/.nvim/backups/
set completeopt-=preview
set expandtab
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932,guess
set foldenable
set foldmethod=marker
set history=10000
set hlsearch
set modeline
set number
set shiftround
set shiftwidth=4
set showtabline=0
set softtabstop=4
set splitbelow
set splitright
set tabstop=4
set textwidth=78
set undodir=~/.nvim/undo/
set undofile
set viewdir=~/.nvim/view/
set viewoptions=cursor,folds
set wildmode=longest,list

let mapleader = ','

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

" Reset highlight search by pressing Escape 2 times
nnoremap <silent> <Esc><Esc> :nohlsearch<CR>

" Move pwd to directory of buffer
nnoremap <Leader>cdc :cd %:h<CR>
nnoremap <Leader>cdl :lcd %:h<CR>

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
  endif
  echo 'Diff '.(&diff ? 'ON' : 'OFF')
endfunction
" Note that <Leader>sd is mapped to surround.vim <Plug>Dsurround
nmap <silent> <Leader>dt :call <SID>toggle_diff()<CR>
" }}}
" Mapping for inserting closing curly brace {{{
function! s:closing_brace_mapping()
  if &filetype == 'vim'
    inoremap <buffer> {<CR> {<CR>\<Space>}<Esc>O\<Space>
  " Don't do this for LaTeX documents
  elseif &filetype !~ 'tex\|plaintex'
    inoremap <buffer> {<CR> {<CR>}<Esc>O
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
" }}}
" Create backup view, and undo directories {{{
if !isdirectory(expand(&backupdir))
  call mkdir(&backupdir)
endif
if !isdirectory(expand(&viewdir))
  call mkdir(&viewdir)
endif
if !isdirectory(expand(&undodir))
  call mkdir(&undodir)
endif
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
if !isdirectory(expand('~/.nvim/bundle/neobundle.vim')) && executable('git')
  let url = 'https://github.com/Shougo/neobundle.vim'
  let dest = '~/.nvim/bundle/neobundle.vim'
  let cmd = 'git clone '.url.' '.dest
  call system(cmd)
endif
" }}}
" Init neobundle.vim {{{
call neobundle#begin(expand('~/.nvim/bundle/'))

if neobundle#load_cache()
  NeoBundleFetch 'Shougo/neobundle.vim'

  call neobundle#load_toml(expand('~/.nvim/NeoBundle.toml'))
  call neobundle#load_toml(expand('~/.nvim/NeoBundleLazy.toml'), {'lazy': 1})

  NeoBundleSaveCache
endif

call neobundle#end()
filetype plugin indent on
if has('vim_starting')
    NeoBundleCheck
  endif
" }}}
" Plugins {{{
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
" matchit.zip {{{
if neobundle#tap('matchit.zip')
  function! neobundle#hooks.on_post_source(bundle)
    silent! execute 'doautocmd FileType' &filetype
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
    endif
    echo 'Cleared cache: '.what
  endfunction

  nmap <silent> <Leader>cc :call <SID>clear_cache()<CR>

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
" vim-better-whitespace {{{
if neobundle#tap('vim-better-whitespace')
  function! neobundle#hooks.on_source(bundle)
    let g:better_whitespace_filetypes_blacklist = [
          \ 'unite', 'help', 'vimshell', 'git'
          \ ]
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
" vim-jplus {{{
if neobundle#tap('vim-jplus')
  nmap J <Plug>(jplus)
  nmap <Leader>J <Plug>(jplus-getchar)

  function! neobundle#hooks.on_source(bundle)
    let g:jplus#input_config = {
          \ '__DEFAULT__': {
          \   'delimiter_format': ' %d ',
          \ },
          \ ',': {
          \   'delimiter_format': '%d, ',
          \ },
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
" }}}
" Source local configurations if any {{{
if filereadable(expand('~/.localrc/nvimrc'))
  source ~/.localrc/nvimrc
endif
" }}}
