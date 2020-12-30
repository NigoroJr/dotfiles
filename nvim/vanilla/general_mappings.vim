" Leader
let mapleader = ','
noremap \ ,

" Move by physical/logical line
nnoremap gj j
nnoremap gk k
nnoremap j gj
nnoremap k gk
nnoremap ' `
nnoremap ` '

" More powerful ex command history
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" Shell key bindings in ex mode
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>
" Mimic M-d on emacs
cnoremap <M-d> <S-Right><C-w><Backspace><Right>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-g> <C-c>
cnoremap <C-d> <Delete>

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

" Q to quit
nmap <silent> Q :q<CR>

" Function to toggle variables and show message
function! s:toggle(var, mes) abort
  silent exec 'setlocal '.a:var.'!'
  echo a:mes.' '.(eval('&'.a:var) ? 'ON' : 'OFF')
endfunction

nmap <silent> <Leader>ss :call <SID>toggle("spell", "Spell")<CR>
nmap <silent> <Leader>sp :call <SID>toggle("paste", "Paste")<CR>
nmap <silent> <Leader>sb :call <SID>toggle("scrollbind", "Scrollbind")<CR>
nmap <silent> <Leader>set :call <SID>toggle("expandtab", "expandtab")<CR>
nmap <silent> <Leader>sw :call <SID>toggle("wrap", "wrap")<CR>

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

" function! s:closing_brace_mapping()
"   if &filetype == 'vim'
"     inoremap <buffer> {<CR> {<CR>\<Space>}<Esc>O\<Space>
"     inoremap <buffer> (<CR> (<CR>\<Space>)<Esc>O\<Space>
"     inoremap <buffer> [<CR> [<CR>\<Space>]<Esc>O\<Space>
"   " Don't do this for LaTeX documents
"   elseif &filetype !~ 'tex\|plaintex'
"     inoremap <buffer> {<CR> {<CR>}<Esc>O
"     inoremap <buffer> (<CR> (<CR>)<Esc>O
"     inoremap <buffer> [<CR> [<CR>]<Esc>O
"   endif
"   " Otherwise, no mapping is done
" endfun
" autocmd FileType * call <SID>closing_brace_mapping()
