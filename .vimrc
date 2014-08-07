set backup
set backupdir=~/.vim/backup/
set viewdir=~/.vim/view/
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
let mapleader = ','
noremap \ ,
syntax on
" Jump to matching keyword
runtime macros/matchit.vim

" Filetype-specific text properties {{{
autocmd FileType text,vimshell set textwidth=0
autocmd FileType ruby,html,eruby,vim set shiftwidth=2 tabstop=2
autocmd FileType python,scss set shiftwidth=4 tabstop=4
autocmd FileType go set noexpandtab
" }}}
" General keybindings   {{{
" Move by physical/logical line
nnoremap gj j
nnoremap gk k
nnoremap j gj
nnoremap k gk

" More powerful ex command history
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>
" Shell keybindings in ex mode
cnoremap <C-b> <Left>
cnoremap <C-f> <Right>
" NOTE: Meta keys do not work in terminal Vim
cnoremap <M-f> <S-Right>
cnoremap <M-b> <S-Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>

" Save as super user
nnoremap <Leader>ws :w sudo:%<CR>
nnoremap <Leader>xs :x sudo:%<CR>

" Reset hilight search by pressing Escape 2 times
nnoremap <silent> <ESC><ESC> :nohlsearch<CR>

" Move pwd to directory of buffer
nnoremap <silent><Leader>cd :cd %:h<CR>
" }}}
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
" Insert closing braces {{{
fun! CloseBraces()
  if &filetype == 'vim'
    inoremap {<CR> {<CR>\<Space>}<Esc>O\<Space>
  " Don't do this for LaTeX documents
  elseif &filetype !~ 'tex\|plaintex'
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
" Create backup and view directories {{{
if !isdirectory(expand(&backupdir))
  call mkdir(&backupdir)
endif
if !isdirectory(expand(&viewdir))
  call mkdir(&viewdir)
endif
" }}}
" Set filetypes to not save/load the view {{{
autocmd FileType gitcommit autocmd! BufEnter COMMIT_EDITMSG call setpos('.', [0, 1, 1, 0])
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

" Plugins managed by neobundle.vim {{{
" Setup runtimepath on start {{{
if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim
endif
" }}}
call neobundle#begin(expand('~/.vim/bundle/'))

" NeoBundleFetch {{{
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundleFetch 'davidhalter/jedi'
" }}}
" NeoBundle {{{
NeoBundle 'Align'
NeoBundle 'ciaranm/securemodelines'
NeoBundle 'gcmt/wildfire.vim'
NeoBundle 'goldfeld/vim-seek'
NeoBundle 'mattn/disableitalic-vim', {
      \ 'gui': 1,
      \ }
NeoBundle 'mips.vim'
NeoBundle 'osyo-manga/vim-watchdogs', {
      \ 'gui': 1,
      \ 'depends': ['thinca/vim-quickrun', 'jceb/vim-hier'],
      \ }
NeoBundle 'rhysd/clever-f.vim'
NeoBundle 'Shougo/neocomplete.vim', {
      \ 'depends': ['Shougo/vimproc.vim'],
      \ 'disabled': !has('lua'),
      \ 'vim_version': '7.3.885',
      \ }
" neocomplete.vim must come before neocomplcache.vim
NeoBundle 'Shougo/neocomplcache.vim', {
      \ 'depends': 'Shougo/vimproc.vim',
      \ 'disabled': neobundle#is_sourced('neocomplete.vim'),
      \ }
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundleLazy 'Shougo/vimfiler.vim', {
      \ 'autoload': {
      \   'commands': ['VimFiler', 'VimFilerCurrentDir',
      \     'VimFilerBufferDir', 'VimFilerExplorer'],
      \ },
      \ }
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build': {
      \   'windows': 'mingw32-make -f make_mingw32.mak',
      \   'cygwin': 'make -f make_cygwin.mak',
      \   'mac': 'make -f make_mac.mak',
      \   'unix': 'make -f make_unix.mak',
      \ },
      \ }
NeoBundle 'Shougo/vimshell.vim', {
      \ 'depends': ['Shougo/vimproc.vim'],
      \ }
NeoBundle 'Shougo/vinarise.vim'
NeoBundle 'TextFormat'
NeoBundle 'thinca/vim-ref'
NeoBundle 'tkztmk/vim-vala'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'wesQ3/vim-windowswap'
" }}}
" NeoBundleLazy {{{
NeoBundleLazy 'c9s/perlomni.vim', {
      \ 'autoload': {
      \   'filetypes': 'perl',
      \ },
      \ }
NeoBundleLazy 'davidhalter/jedi-vim', {
      \ 'autoload': {
      \   'filetypes': 'python',
      \ },
      \ 'depends': 'davidhalter/jedi',
      \ }
NeoBundleLazy 'dbext.vim'
NeoBundleLazy 'dkasak/manpageview'
NeoBundleLazy 'fatih/vim-go', {
      \ 'autoload': {
      \   'filetypes': 'go',
      \ },
      \ }
NeoBundleLazy 'hotchpotch/perldoc-vim', {
      \ 'autoload': {
      \   'filetypes': 'perl',
      \ },
      \ }
NeoBundleLazy 'jceb/vim-hier', {
      \ 'gui': 1,
      \ }
NeoBundleLazy 'kana/vim-altr', {
      \ 'autoload': {
      \   'filetypes': ['c', 'cpp'],
      \ },
      \ }
NeoBundleLazy 'kana/vim-textobj-user'
NeoBundleLazy 'kannokanno/previm', {
      \ 'autoload': {
      \   'commands': 'PrevimOpen',
      \   'filetypes': 'markdown',
      \ },
      \ }
NeoBundleLazy 'osyo-manga/shabadou.vim', {
      \ 'depends': 'thinca/vim-quickrun',
      \ }
NeoBundleLazy 'osyo-manga/unite-quickfix', {
      \ 'depends': 'Shougo/unite.vim',
      \ }
NeoBundleLazy 'osyo-manga/vim-marching', {
      \ 'depends': ['Shougo/vimproc.vim', 'osyo-manga/vim-reunions'],
      \ 'autoload': {
      \   'filetypes': ['c', 'cpp'],
      \ },
      \ }
NeoBundleLazy 'osyo-manga/vim-precious', {
      \ 'depends': ['Shougo/context_filetype.vim'],
      \ }
NeoBundleLazy 'osyo-manga/vim-stargate', {
      \ 'autoload': {
      \   'filetypes': 'cpp',
      \ },
      \ }
NeoBundleLazy 'rhysd/vim-go-impl', {
      \ 'autoload': {
      \   'filetypes': 'go',
      \ },
      \ }
NeoBundleLazy 'rhysd/wandbox-vim', {
      \ 'depends': 'thinca/vim-quickrun',
      \ }
NeoBundleLazy 'scrooloose/nerdtree'
NeoBundleLazy 'Shougo/context_filetype.vim'
NeoBundleLazy 'Shougo/unite.vim', {
      \ 'autoload': {
      \   'commands': ['Unite', 'UniteWithBufferDir',
      \     'UniteWithCurrentDir', 'UniteWithProjectDir',
      \     'UniteWithInputDirectory', 'UniteWithCursorWord'],
      \ },
      \ }
NeoBundleLazy 'thinca/vim-quickrun', {
      \ 'autoload': {
      \   'commands': 'QuickRun',
      \   'mappings': [
      \     ['nxo', '<Plug>(quickrun)'],
      \   ],
      \ },
      \ }
NeoBundleLazy 'tpope/vim-markdown', {
      \ 'autoload': {
      \   'filetypes': 'markdown',
      \ },
      \ }
NeoBundleLazy 'tpope/vim-rails', {
      \ 'autoload': {
      \   'filetypes': ['ruby', 'eruby', 'css', 'scss', 'html', 'javascript', 'yaml'],
      \ },
      \ }
NeoBundleLazy 'tyru/open-browser.vim', {
      \ 'autoload': {
      \   'filetypes': 'markdown',
      \ },
      \ }
" }}}

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
" }}}
" Configurations for individual plugins {{{
" context_filetype.vim {{{
let s:bundle = neobundle#get('context_filetype.vim')
function! s:bundle.hooks.on_source(bundle)
  call neobundle#source('osyo-manga/vim-precious')
endfunction
" }}}
" clever-f.vim {{{
let g:clever_f_fix_key_direction = 1
let g:clever_f_chars_match_any_signs = ''
let g:clever_f_across_no_line = 1
" }}}
" jedi-vim {{{
let s:bundle = neobundle#get('jedi-vim')
function! s:bundle.hooks.on_source(bundle)
  autocmd FileType python setlocal omnifunc=jedi#completions
  let g:jedi#popup_select_first = 0
  let g:jedi#completions_enabled = 0
  let g:jedi#auto_vim_configuration = 0
  let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
endfunction
" }}}
" manpageview {{{
let g:manpageview_winopen = 'vsplit='
" }}}
" neocomplcache.vim {{{
let s:bundle = neobundle#get('neocomplcache.vim')
function! s:bundle.hooks.on_source(bundle)
  call neocomplcache#initialize()

  let g:neocomplcache#enable_at_startup = 1
  let g:neocomplcache#enable_smart_case = 1
  let g:neocomplcache#min_keyword_length = 3

  inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
  inoremap <expr><BS> neocomplcache#smart_close_popup()."\<BS>"
  inoremap <expr><C-y> neocomplcache#close_popup()
  inoremap <expr><C-g> neocomplcache#cancel_popup()

  if !exists('g:neocomplcache#force_omni_input_patterns')
    let g:neocomplcache#force_omni_input_patterns = {}
  endif

  let g:neocomplcache#force_omni_input_patterns.cpp =
        \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
  let g:neocomplcache#force_omni_input_patterns.perl =
        \ '[^. \t]->\%(\h\w*\)\?\|\h\w*::\%(\h\w*\)\?'
  let g:neocomplcache#force_omni_input_patterns.go =
        \ '[^.[:digit:] *\t]\.\w*'
endfunction
" }}}
" neocomplete.vim {{{
let s:bundle = neobundle#get('neocomplete.vim')
function! s:bundle.hooks.on_source(bundle)
  call neobundle#source('context_filetype.vim')

  call neocomplete#initialize()

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
endfunction

" }}}
" neosnippet.vim {{{
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
" nerdtree {{{
command! Rtree NERDTreeFind
" Don't use NERDTree when opening directory
let g:NERDTreeHijackNetrw = 0
let g:NERDTreeMinimalUI = 1
let g:NERDTreeDirArrows = 0
" }}}
" unite.vim {{{
let s:bundle = neobundle#get('unite.vim')
noremap <Leader>ur :Unite register -buffer-name=register<CR>
"noremap <Leader>uf :UniteWithBufferDir file/async file_rec/async -buffer-name=file -create<CR>
noremap <Leader>uq :Unite -horizontal -no-quit quickfix<CR>
noremap <Leader>uf :UniteWithBufferDir file_rec/async -buffer-name=file -create<CR>

function! s:bundle.hooks.on_source(bundle)
  call unite#custom#profile('default', 'context', {
        \ 'start_insert': 1,
        \ 'direction': 'botright',
        \ })
  autocmd FileType unite imap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
endfunction
" }}}
" vim-altr {{{
nmap <Leader>a <Plug>(altr-forward)
nmap <Leader>A <Plug>(altr-back)
" }}}
" vim-go {{{
let s:bundle = neobundle#get('vim-go')
function! s:bundle.hooks.on_source(bundle)
  let g:go_snippet_engine = 'neosnippet'
  let g:go_highlight_trailing_whitespace_error = 0
  let g:go_highlight_operators = 0
endfunction
" }}}
" vim-marching {{{
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
" vim-quickrun {{{
if neobundle#tap('vim-quickrun')
  nmap <silent> <Leader>r <Plug>(quickrun)
  call neobundle#untap()
endif

let s:bundle = neobundle#get('vim-quickrun')
function! s:bundle.hooks.on_source(bundle)
  nnoremap <Leader>wb :QuickRun -runner wandbox<CR>
  call neobundle#source(['osyo-manga/shabadou.vim',
        \ 'rhysd/wandbox-vim', 'osyo-manga/unite-quickfix'])

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
        \   'hook/quickfix_replate_tempname_to_bufnr/enable_exit': 1,
        \   'hook/quickfix_replate_tempname_to_bufnr/priority_exit': -10,
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

endfunction
" }}}
" vim-rails {{{
let s:bundle = neobundle#get('vim-rails')
function! s:bundle.hooks.on_source(bundle)
  call neobundle#source('vimfiler.vim')
  "call neobundle#source('nerdtree')
endfunction
" }}}
" vim-ref {{{
let g:ref_open = 'vsplit'
let g:ref_detect_filetype = {
      \ 'sh': 'man',
      \ 'zsh': 'man',
      \ 'bash': 'man',
      \ }
let g:ref_use_vimproc = 1
nmap K <Plug>(ref-keyword)
" }}}
" vim-seek {{{
let g:seek_ignorecase = 1
" }}}
" vim-stargate {{{
let g:stargate#include_paths = {
      \ 'cpp': marching_include_paths,
      \ }
nmap <Leader>sg :StargateInclude<Space>
" }}}
" vim-watchdogs {{{
let s:bundle = neobundle#get('vim-watchdogs')
function! s:bundle.hooks.on_source(bundle)
  let g:watchdogs_check_BufWritePost_enable = 1
  call watchdogs#setup(g:quickrun_config)
endfunction
nmap <silent> <Leader>wd :WatchdogsRun<CR>
" }}}
" vim-windowswap {{{
let g:windowswap_map_keys = 0 "prevent default bindings
nnoremap <silent> <leader>yw :call WindowSwap#MarkWindowSwap()<CR>
nnoremap <silent> <leader>pw :call WindowSwap#DoWindowSwap()<CR>
" }}}
" vimfiler.vim {{{
nnoremap <silent> <Leader>f :VimFilerBufferDir -status<CR>

let s:bundle = neobundle#get('vimfiler.vim')
function! s:bundle.hooks.on_source(bundle)
  " Disable netrw.vim
  let g:loaded_netrwPlugin = 1
  let g:vimfiler_as_default_explorer = 1
  let g:vimfiler_time_format = "%m/%d/%y %H:%M%S"
endfunction
" }}}
" vim-precious {{{
let s:bundle = neobundle#get('vim-precious')
function! s:bundle.hooks.on_source(bundle)
  call neobundle#source('kana/vim-textobj-user')
endfunction
" }}}
" vimshell.vim {{{
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
" vinarise.vim {{{
let g:vinarise_enable_auto_detect = 1
" }}}
" }}}

" Source local configurations if any {{{
if exists(expand('~/.vimrc_local'))
  source ~/.vimrc_local
endif
" }}}
