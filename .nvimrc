set autoread
set backspace=indent,eol,start
set backup
set backupdir=~/.vim/backups/
set completeopt-=preview
set encoding=utf-8
set expandtab
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932,guess
set foldenable
set foldmethod=marker
set history=10000
set hlsearch
set modeline
set nocompatible
set number
set shiftround
set shiftwidth=4
set showtabline=0
set softtabstop=4
set splitbelow
set splitright
set tabstop=4
set textwidth=78
set undodir=~/.vim/undo/
set undofile
set viewdir=~/.vim/view/
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
nnoremap <silent> <Leader>tg :call <SID>goto_tab()<CR>
nnoremap <silent> <Leader>ts :tabs<CR>
nnoremap <silent> <Leader>tn :tabnext<CR>
nnoremap <silent> <Leader>tp :tabprevious<CR>
nnoremap <silent> <Leader>tf :tabfirst<CR>
nnoremap <silent> <Leader>tl :tablast<CR>
nnoremap <silent> <Leader>tw :tabnew<CR>

" Function to toggle variables and show message
function! s:toggle(var, mes) abort
  silent exec 'setlocal '.a:var.'!'
  echo a:mes.' '.(eval('&'.a:var) ? 'ON' : 'OFF')
endfunction

" Toggle spell check
nmap <silent> <Leader>ss :call <SID>toggle('spell', 'Spell')<CR>

" Toggle paste
nmap <silent> <Leader>sp :call <SID>toggle('paste', 'Paste')<CR>

" Toggle scrollbind
nmap <silent> <Leader>sb :call <SID>toggle('scrollbind', 'Scrollbind')<CR>
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
" C++ snippets {{{
augroup cpp-namespace
  autocmd!
  autocmd FileType cpp inoremap <buffer><expr>; <SID>expand_namespace()
  autocmd FileType cpp inoremap <buffer><expr>{<CR> <SID>class_declaration()
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
" Source local configurations if any {{{
if filereadable(expand('~/.localrc/nvimrc'))
  source ~/.localrc/nvimrc
endif
" }}}
