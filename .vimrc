set backup
set backupdir=~/.vim/backup/
set tabstop=4
set expandtab
set shiftwidth=4
set shiftround
set textwidth=78
set number
set wildmode=longest,list
set foldenable
set foldmethod=marker
set history=100000
set viewoptions=cursor,folds
set hlsearch
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932,guess
set backspace=indent,eol,start
set splitright
set splitbelow
set modeline
set completeopt-=preview
let mapleader=","
noremap \ ,
syntax on

" Indentations
autocmd FileType text,vimshell set textwidth=0
autocmd FileType ruby,html,eruby,vim set shiftwidth=2 tabstop=2
autocmd FileType python,scss set shiftwidth=4 tabstop=4
" Hard-tabs
autocmd FileType go set noexpandtab

" Jump to matching keyword
runtime macros/matchit.vim

" Case-sensitive search, case-insensitive command completion  {{{
let b:case_insensitive_cmd = 0
if b:case_insensitive_cmd
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

" Keybindings   {{{
inoremap <C-d> <Del>

" Move by physical/logical line
nnoremap gj j
nnoremap gk k
nnoremap j gj
nnoremap k gk

" More powerful ex command history
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Save as super user
nnoremap <Leader>ws :w sudo:%<CR>
nnoremap <Leader>xs :x sudo:%<CR>

" Reset hilight search by pressing Escape 2 times
nnoremap <silent> <ESC><ESC> :nohlsearch<CR>

" Move pwd to directory of buffer
nnoremap <silent><Leader>cd :cd %:h<CR>
" }}}

" Insert closing braces {{{
fun! CloseBraces()
  " Don't do this for LaTeX documents
  if &filetype !~ 'tex\|plaintex'
    inoremap {<CR> {<CR>}<Esc>O
  endif
endfun
autocmd FileType * call CloseBraces()
" }}}

" C++ snippets {{{
augroup cpp-namespace
  autocmd!
  autocmd FileType cpp inoremap <buffer><expr>; <SID>expand_namespace()
augroup END
function! s:expand_namespace()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<b;$'
    return "\<BS>oost::"
  elseif s =~# '\<s;$'
    return "\<BS>td::"
  elseif s =~# '\<d;$'
    return "\<BS>etail::"
  else
    return ';'
  endif
endfunction
" }}}

" Create backup dir if it doesn't exist {{{
if !isdirectory(expand('~/.vim/backup/'))
  call mkdir(expand("~/.vim/backup/"))
endif
" }}}

" views {{{
" Set viewdir to be ~/Dropbox/vim/view/
if isdirectory(expand('~/Dropbox/vim/view/'))
  set viewdir=~/Dropbox/vim/view/
else
  if !isdirectory(expand('~/.vim/view/'))
    call mkdir(expand("~/.vim/view/"))
  endif

  set viewdir=~/.vim/view/
endif

" Set filetypes to not save/load the view
au FileType gitcommit au! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
let no_view = ['gitcommit']
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

" setup for neobundle.vim {{{
if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim
endif
call neobundle#begin(expand('~/.vim/bundle/'))
" }}}

" neobundle.vim {{{
NeoBundleFetch 'Shougo/neobundle.vim'

NeoBundle 'Shougo/neocomplete.vim'
" {{{
let g:neocomplete#enable_at_startup = 1
let g:neocomplete#enable_smart_case = 1
let g:neocomplete#min_keyword_length = 3

inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<BS>"
inoremap <expr><C-y> neocomplete#close_popup()
inoremap <expr><C-g> neocomplete#cancel_popup()

if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif

let g:neocomplete#force_omni_input_patterns.cpp =
      \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'

" perlomni.vim
let g:neocomplete#force_omni_input_patterns.perl =
      \ '[^. \t]->\%(\h\w*\)\?\|\h\w*::\%(\h\w*\)\?'

" Go
let g:neocomplete#force_omni_input_patterns.go =
      \ '[^.[:digit:] *\t]\.\w*'

" }}}

NeoBundle 'Shougo/vimshell.vim'
" {{{
" VimShell with ,is
" nnoremap <silent> <Leader>is :VimShell<CR>
nnoremap <silent> <Leader>vs :VimShellBuffer -split-command=vsplit<CR>
" Open VimShell vertically
"nnoremap <silent> <Leader>vvs :sp<CR><C-w>j:VimShell<CR>
nnoremap <silent> <Leader>vvs :VimShellBuffer -split-command=split<CR>
" Interactive python with ,ipy
nnoremap <silent> <Leader>ipy :VimShellInteractive python<CR>
" Hit escape twice to exit vimshell
autocmd FileType vimshell imap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
autocmd FileType vimshell nmap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
" Enter in normal mode goes into insertion mode first
autocmd FileType vimshell nmap <silent> <buffer> <CR> A<CR>

" Set user prompt to pwd
let g:vimshell_prompt_expr = 'getcwd()." > "'
let g:vimshell_prompt_pattern = '^\f\+ > '
" }}}

NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \   'windows' : 'make -f make_mingw32.mak',
      \   'cygwin' : 'make -f make_cygwin.mak',
      \   'mac' : 'make -f make_mac.mak',
      \   'unix' : 'make -f make_unix.mak',
      \ },
      \ }

NeoBundle 'thinca/vim-quickrun'
NeoBundle 'osyo-manga/shabadou.vim'
NeoBundle 'rhysd/wandbox-vim'
" {{{
let g:quickrun_config = {
      \ '_': {
      \   'hook/time/enable': 0,
      \   'hook/close_unite_quickfix/enable_hook_loaded' : 1,
      \   'hook/unite_quickfix/enable_failure': 1,
      \   'hook/close_quickfix/enable_exit': 1,
      \   'hook/close_buffer/enable_empty_data': 1,
      \   'hook/close_buffer/enable_failure': 1,
      \   'outputter': 'multi:buffer:quickfix',
      \   'outputter/buffer/close_on_empty': 1,
      \   'hook/quickfix_replate_tempname_to_bufnr/enable_exit' : 1,
      \   'hook/quickfix_replate_tempname_to_bufnr/priority_exit' : -10,
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
      \ 'hook/quickrunex/enable': 1,
      \ }
" Open in browser using previm
let g:quickrun_config.markdown = {
      \ 'runner': 'vimscript',
      \ 'exec': 'PrevimOpen',
      \ 'outputter': 'null',
      \ 'hook/time/enable': 0,
      \ }
" }}}

NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
" {{{
nnoremap <Leader>es :<C-u>NeoSnippetEdit

let g:neosnippet#snippets_directory = '~/.vim/snippets/'

" <TAB>: completion
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_expand_or_jump)" :
      \ pumvisible() ?
      \ "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
      \ "\<Plug>(neosnippet_jump_or_expand)" :
      \ pumvisible() ?
      \ "\<C-n>" : "\<TAB>"

imap <silent> <C-k> <Plug>(neosnippet_jump_or_expand)
smap <silent> <C-k> <Plug>(neosnippet_jump_or_expand)

" }}}

NeoBundle 'Shougo/unite.vim'
NeoBundle 'osyo-manga/unite-quickfix'
" {{{
" Start with insert mode
let g:unite_enable_start_insert=1
" Open Unite vertically
let g:unite_enable_split_vertically=1
let g:unite_split_rule='botright'

" Recently used files
noremap <Leader>uru :Unite file_mru -buffer-name=file_mru<CR>
noremap <Leader>ureg :Unite register -buffer-name=register<CR>
noremap <Leader>ubu :Unite buffer -buffer-name=buffer<CR>

" File in current directory
noremap <Leader>uf :UniteWithBufferDir file -buffer-name=file -create<CR>
noremap <Leader>ure :UniteWithBufferDir file_rec -buffer-name=file_rec -create<CR>

" QuickFix in Unite
noremap <Leader>uq :Unite -horizontal -no-quit -direction=botright quickfix<CR>

" Hit Esc twice to exit Unite
autocmd FileType unite nmap <silent> <buffer> <Esc><Esc> <Plug>(unite_exit)
autocmd FileType unite imap <silent> <buffer> <Esc><Esc> <Plug>(unite_exit)
" Move cursor with j, k in normal mode and do default action with l
" autocmd FileType unite nmap <silent> <buffer> j <Plug>(unite_loop_cursor_down)
" autocmd FileType unite nmap <silent> <buffer> k <Plug>(unite_loop_cursor_up)
autocmd FileType unite nmap <silent> <buffer> l <Plug>(unite_do_default_action)
autocmd FileType unite nmap <silent> <buffer> h <Plug>(unite_delete_backward_path)
" Move down when hitting jj, wrap and go to the bottom when hitting kk
autocmd FileType unite imap <buffer> kk <Esc><Plug>(unite_cursor_bottom)
autocmd FileType unite imap <buffer> jj <Esc><Plug>(unite_loop_cursor_down)
" Move cursor with C-j and C-k in insert mode
autocmd FileType unite imap <silent> <buffer> <C-k> <Esc><Plug>(unite_cursor_bottom)
autocmd FileType unite imap <silent> <buffer> <C-j> <Esc><Plug>(unite_loop_cursor_down)

autocmd FileType unite nmap <silent> <buffer> <C-k> <Esc><Plug>(unite_loop_cursor_up)
autocmd FileType unite nmap <silent> <buffer> <C-j> <Esc><Plug>(unite_loop_cursor_down)
" Open or go up directory with C-l, C-h
autocmd FileType unite nmap <silent> <buffer> <C-h> <Plug>(unite_delete_backward_path) <Plug>(unite_insert_enter)

autocmd FileType unite imap <silent> <buffer> <C-l> <Plug>(unite_do_default_action)
autocmd FileType unite nmap <silent> <buffer> <C-l> <Plug>(unite_do_default_action)

autocmd FileType unite imap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
autocmd FileType unite nmap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
" Go back to insert mode with /
autocmd FileType unite nmap <buffer> / <Plug>(unite_insert_enter)

" }}}

NeoBundle 'Shougo/vimfiler.vim'
" {{{
let g:vimfiler_as_default_explorer=1
let g:vimfiler_time_format="%m/%d/%y %H:%M%S"

nnoremap <silent> <Leader>vf :VimFiler<CR>
" }}}

NeoBundleLazy 'osyo-manga/vim-marching', {
      \ 'depends': ['Shougo/vimproc.vim', 'osyo-manga/vim-reunions'],
      \ 'autoload': {
      \   'filetypes': ['c', 'cpp']}
      \ }
" {{{
let g:marching_enable_neocomplete = 1
let g:marching_include_paths = filter(
      \ split(glob('/usr/lib/gcc/**/include/*/'), '\n') +
      \ split(glob('/usr/include/boost/*'), '\n') +
      \ split(glob('/usr/include/*'), '\n') +
      \ split(glob('/usr/include/c++/*'), '\n') +
      \ split(glob('/usr/local/include/*/'), '\n'),
      \ 'isdirectory(v:val)')

imap <C-x><C-o> <Plug>(marching_start_omni_complete)
imap <C-x><C-o> <Plug>(marching_force_start_omni_complete)
nmap <Leader>mc :MarchingBufferClearCache<CR>
" }}}

NeoBundleLazy 'osyo-manga/vim-stargate', {
      \ 'autoload': {
      \   'filetypes': ['cpp']
      \ },
      \ }
" {{{
let g:stargate#include_paths = {
      \ 'cpp': marching_include_paths,
      \ }
nmap <Leader>sg :StargateInclude<Space>
" }}}

NeoBundle 'kana/vim-altr'
" {{{
nmap <Leader>a <Plug>(altr-forward)
nmap <Leader>A <Plug>(altr-back)
" }}}

NeoBundle 'Shougo/vinarise.vim'
" {{{
let g:vinarise_enable_auto_detect = 1
" }}}

NeoBundle 'goldfeld/vim-seek'
" {{{
let g:seek_ignorecase = 1
" }}}

NeoBundle 'thinca/vim-ref'
" {{{
let g:ref_open='vsplit'
let g:ref_detect_filetype = {
      \ 'sh': 'man',
      \ 'zsh': 'man',
      \ 'bash': 'man',
      \ }
let g:ref_use_vimproc=1
nmap K <Plug>(ref-keyword)
" }}}

" Go programming
" {{{
NeoBundleLazy 'fatih/vim-go', {
      \ 'autoload': {
      \   'filetypes': ['go'],
      \ },
      \ }
" {{{
let s:bundle = neobundle#get('vim-go')
function! s:bundle.hooks.on_source(bundle)
  let g:go_snippet_engine = 'neosnippet'
  let g:go_highlight_trailing_whitespace_error = 0
  let g:go_highlight_operators = 0
endfunction
" }}}
NeoBundleLazy 'rhysd/vim-go-impl', {
      \ 'autoload': {
      \   'filetypes': ['go'],
      \ },
      \ }
" }}}

" Ruby on Rails
" {{{
NeoBundleLazy 'tpope/vim-rails', {
      \ 'autoload': {
      \   'filetypes': ['ruby', 'eruby', 'css', 'scss', 'html', 'javascript', 'yaml'],
      \ },
      \ }
NeoBundle 'scrooloose/nerdtree'
" {{{
command Rtree NERDTreeFind
" Don't use NERDTree when opening directory
let g:NERDTreeHijackNetrw = 0
let g:NERDTreeMinimalUI = 1
let g:NERDTreeDirArrows = 0
" }}}
NeoBundleLazy 'vim-scripts/dbext.vim'
" }}}

NeoBundle 'wesQ3/vim-windowswap'
" {{{
let g:windowswap_map_keys = 0 "prevent default bindings
nnoremap <silent> <leader>yw :call WindowSwap#MarkWindowSwap()<CR>
nnoremap <silent> <leader>pw :call WindowSwap#DoWindowSwap()<CR>
" }}}

NeoBundle 'tpope/vim-fugitive'

NeoBundle 'tyru/open-browser.vim', {
      \ 'autoload' : {
      \   'filetypes' : ['markdown']
      \ },
      \ }

NeoBundle 'rhysd/clever-f.vim'
" {{{
let g:clever_f_fix_key_direction = 1
let g:clever_f_chars_match_any_signs = 0
let g:clever_f_across_no_line = 1
" }}}

" Perl
" {{{
NeoBundleLazy 'c9s/perlomni.vim', {
      \ 'autoload': {
      \   'filetypes': ['perl'],
      \ },
      \ }
NeoBundleLazy 'hotchpotch/perldoc-vim'
NeoBundleLazy 'c9s/perlomni.vim', {
      \ 'autoload': {
      \   'filetypes': ['perl'],
      \ },
      \ }
" }}}

NeoBundle 'Align'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'kannokanno/previm'

NeoBundle 'gcmt/wildfire.vim'
NeoBundle 'tkztmk/vim-vala'
NeoBundle 'vim-scripts/mips.vim'
NeoBundle 'TextFormat'
NeoBundle 'ciaranm/securemodelines'

"NeoBundle 'Shougo/neocomplcache'
" {{{
"let g:neocomplcache_enable_at_startup = 1
"let g:neocomplcache_enable_smart_case = 1
"let g:neocomplcache_enable_camel_case_completion = 1
"let g:neocomplcache_enable_underbar_completion = 1
"let g:neocomplcache_min_syntax_length = 3
"<C-h>, <BS>: close popup and delete backward char
"inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
"inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
"inoremap <expr><C-y>  neocomplcache#close_popup()
"inoremap <expr><C-e> neocomplcache#cancel_popup()
"inoremap <expr><C-g> neocomplcache#undo_completion()
"inoremap <expr><C-l> neocomplcache#complete_common_string()
" }}}

NeoBundle 'osyo-manga/vim-watchdogs', {
      \ 'gui': 1,
      \ }
" {{{
let s:bundle = neobundle#get('vim-watchdogs')
function! s:bundle.hooks.on_source(bundle)
  let g:watchdogs_check_BufWritePost_enable = 1
  call watchdogs#setup(g:quickrun_config)
endfunction
nmap <silent> <Leader>wd :WatchdogsRun<CR>
" }}}
NeoBundle 'jceb/vim-hier', {
      \ 'gui': 1,
      \ }

NeoBundleLazy 'dkasak/manpageview'
" {{{
let g:manpageview_winopen='vsplit='
" }}}

NeoBundleLazy 'tpope/vim-markdown', {
      \ 'autoload' : {
      \   'filetypes' : ['markdown']
      \ },
      \ }
NeoBundleLazy 'vim-scripts/sudo.vim'
" NeoBundleLazy 'davidhalter/jedi-vim'
" {{{
"autocmd FileType python setlocal omnifunc=jedi#completions
"let g:jedi#popup_select_first=0
"let g:jedi#completions_enabled = 0
"let g:jedi#auto_vim_configuration = 0
"let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
" }}}

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
" }}}

" Source local configurations if any
if exists(expand('~/.vimrc_local'))
  source ~/.vimrc_local
endif
