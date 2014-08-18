" Setup runtimepath on start {{{
if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
" }}}
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
autocmd FileType man set nonumber noexpandtab shiftwidth=8 tabstop=8
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
au BufWriteCmd  sudo:*,sudo:*/* SudoWrite <afile>
au FileWriteCmd sudo:*,sudo:*/* SudoWrite <afile>

" Reset hilight search by pressing Escape 2 times
nnoremap <silent> <ESC><ESC> :nohlsearch<CR>

" Move pwd to directory of buffer
nnoremap <Leader>cd :cd %:h<CR>

" EXPERIMENTAL: Q to quit (overrides 'Q', but this is the same as 'gq')
nmap <silent> Q :q<CR>

" Toggle spell check
nmap <Leader>ss :set spell!<CR>
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
call neobundle#begin(expand('~/.vim/bundle/'))

" NeoBundleFetch {{{
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundleFetch 'davidhalter/jedi'
" }}}
" NeoBundle {{{
NeoBundle 'ciaranm/securemodelines'
NeoBundle 'goldfeld/vim-seek'
NeoBundle 'mattn/disableitalic-vim', {
      \ 'gui': 1,
      \ }
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
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build': {
      \   'windows': 'mingw32-make -f make_mingw32.mak',
      \   'cygwin': 'make -f make_cygwin.mak',
      \   'mac': 'make -f make_mac.mak',
      \   'unix': 'make -f make_unix.mak',
      \ },
      \ }
NeoBundle 'Shougo/vinarise.vim'
NeoBundle 'tkztmk/vim-vala'
NeoBundle 'tomtom/tcomment_vim'
" }}}
" NeoBundleLazy {{{
" Plugins with '*' uses neobundle#config() in the individual sections
" *
NeoBundleLazy 'Align', {
      \ 'autoload': {
      \   'commands': ['Align', 'AlignCtrl'],
      \ },
      \ }
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
NeoBundleLazy 'gcmt/wildfire.vim', {
      \ 'autoload': {
      \   'mappings': [
      \     ['nxo', '<Plug>(wildfire-water)'],
      \     ['nxo', '<Plug>(wildfire-fuel)'],
      \   ],
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
      \   'mappings': [
      \     ['nxo', '<Plug>(altr-forward)'],
      \     ['nxo', '<Plug>(altr-back)'],
      \   ],
      \ },
      \ }
NeoBundleLazy 'kana/vim-textobj-user'
NeoBundleLazy 'kannokanno/previm', {
      \ 'autoload': {
      \   'commands': 'PrevimOpen',
      \   'filetypes': 'markdown',
      \ },
      \ }
NeoBundleLazy 'mips.vim', {
      \ 'autoload': {
      \   'filetypes': 'asm',
      \ },
      \ }
NeoBundleLazy 'osyo-manga/shabadou.vim', {
      \ 'depends': 'thinca/vim-quickrun',
      \ }
NeoBundleLazy 'osyo-manga/vim-snowdrop', {
      \ 'autoload': {
      \   'filetypes': 'cpp',
      \ },
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
      \   'commands': 'StargateInclude',
      \   'filetypes': 'cpp',
      \ },
      \ }
NeoBundleLazy 'rhysd/vim-go-impl', {
      \ 'autoload': {
      \   'filetypes': 'go',
      \ },
      \ }
NeoBundleLazy 'rhysd/wandbox-vim', {
      \ 'autoload': {
      \   'commands': ['Wandbox', 'WandboxAsync'],
      \ },
      \ 'depends': ['thinca/vim-quickrun', 'Shougo/vimproc.vim'],
      \ }
NeoBundleLazy 'scrooloose/nerdtree', {
      \ 'augroup': 'NERDTreeHijackNetrw',
      \ 'autoload': {
      \   'commands': [
      \     {
      \       'name': 'NERDTree',
      \       'complete': 'dir',
      \     },
      \     'NERDTreeFind',
      \   ],
      \ },
      \ }
NeoBundleLazy 'Shougo/context_filetype.vim'
NeoBundleLazy 'Shougo/neomru.vim', {
      \ 'depends': 'Shougo/unite.vim',
      \ }
" *
NeoBundleLazy 'Shougo/unite.vim'
" *
NeoBundleLazy 'Shougo/vimfiler.vim'
" *
NeoBundleLazy 'Shougo/vimshell.vim', {
      \ 'depends': ['Shougo/vimproc.vim'],
      \ }
NeoBundleLazy 'sudo.vim', {
      \ 'autoload': {
      \   'commands': ['SudoRead', 'SudoWrite'],
      \ },
      \ }
NeoBundleLazy 'TextFormat', {
      \ 'autoload': {
      \   'commands': ['AlignCenter', 'AlignJustify',
      \     'AlignRight', 'AlignLeft'
      \   ],
      \ },
      \ }
NeoBundleLazy 'thinca/vim-ref', {
      \ 'autoload': {
      \   'commands': {
      \     'name': 'Ref',
      \     'complete': 'customlist,ref#complete',
      \   },
      \   'mappings': [
      \     ['nxo', '<Plug>(ref-keyword)'],
      \   ],
      \ },
      \ }
NeoBundleLazy 'thinca/vim-quickrun', {
      \ 'autoload': {
      \   'commands': {
      \     'name': 'QuickRun',
      \     'complete': 'customlist,quickrun#complete',
      \   },
      \   'mappings': [
      \     ['nxo', '<Plug>(quickrun)'],
      \   ],
      \ },
      \ 'depends': 'Shougo/vimproc.vim',
      \ }
" *
NeoBundleLazy 'tpope/vim-fugitive', {
      \ 'augroup': 'fugitive',
      \ }
NeoBundleLazy 'tpope/vim-markdown', {
      \ 'autoload': {
      \   'filetypes': 'markdown',
      \ },
      \ }
" *
NeoBundleLazy 'tpope/vim-rails', {
      \ 'autoload': {
      \   'filetypes': ['ruby', 'eruby', 'css', 'scss', 'html', 'javascript', 'yaml'],
      \ },
      \ }
NeoBundleLazy 'tyru/open-browser.vim', {
      \ 'autoload': {
      \   'commands': 'OpenBrowser',
      \   'filetypes': 'markdown',
      \ },
      \ }
NeoBundleLazy 'wesQ3/vim-windowswap', {
      \ 'autoload': {
      \   'functions': ['WindowSwap#MarkWindowSwap',
      \     'WindowSwap#DoWindowSwap', 'WindowSwap#EasyWindowSwap',
      \   ],
      \ },
      \ }
" }}}

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
" }}}
" Configurations for individual plugins {{{
" Align.vim {{{
call neobundle#config('Align', {
      \ 'autoload': {
      \   'mappings': [
      \     [ 'nox', '<Plug>AM_tt'],
      \     [ 'nox', '<Plug>AM_tsq'],
      \     [ 'nox', '<Plug>AM_tsp'],
      \     [ 'nox', '<Plug>AM_tml'],
      \     [ 'nox', '<Plug>AM_tab'],
      \     [ 'nox', '<Plug>AM_m='],
      \     [ 'nox', '<Plug>AM_tW@'],
      \     [ 'nox', '<Plug>AM_t@'],
      \     [ 'nox', '<Plug>AM_t~'],
      \     [ 'nox', '<Plug>AM_t?'],
      \     [ 'nox', '<Plug>AM_w='],
      \     [ 'nox', '<Plug>AM_ts='],
      \     [ 'nox', '<Plug>AM_ts<'],
      \     [ 'nox', '<Plug>AM_ts;'],
      \     [ 'nox', '<Plug>AM_ts:'],
      \     [ 'nox', '<Plug>AM_ts,'],
      \     [ 'nox', '<Plug>AM_t='],
      \     [ 'nox', '<Plug>AM_t<'],
      \     [ 'nox', '<Plug>AM_t;'],
      \     [ 'nox', '<Plug>AM_t:'],
      \     [ ''   , '<Plug>AM_t,'],
      \     [ 'nox', '<Plug>AM_t#'],
      \     [ ''   , '<Plug>AM_t|'],
      \     [ 'nox', '<Plug>AM_T~'],
      \     [ 'nox', '<Plug>AM_Tsp'],
      \     [ 'nox', '<Plug>AM_Tab'],
      \     [ 'nox', '<Plug>AM_TW@'],
      \     [ 'nox', '<Plug>AM_T@'],
      \     [ 'nox', '<Plug>AM_T?'],
      \     [ 'nox', '<Plug>AM_T='],
      \     [ 'nox', '<Plug>AM_T<'],
      \     [ 'nox', '<Plug>AM_T;'],
      \     [ 'nox', '<Plug>AM_T:'],
      \     [ 'nox', '<Plug>AM_Ts,'],
      \     [ ''   , '<Plug>AM_T,o'],
      \     [ 'nox', '<Plug>AM_T#'],
      \     [ ''   , '<Plug>AM_T|'],
      \     [ 'nox', '<Plug>AM_Htd'],
      \     [ 'nox', '<Plug>AM_aunum'],
      \     [ 'nox', '<Plug>AM_aenum'],
      \     [ 'nox', '<Plug>AM_aunum'],
      \     [ 'nox', '<Plug>AM_afnc'],
      \     [ 'nox', '<Plug>AM_adef'],
      \     [ 'nox', '<Plug>AM_adec'],
      \     [ 'nox', '<Plug>AM_ascom'],
      \     [ 'nox', '<Plug>AM_aocom'],
      \     [ 'nox', '<Plug>AM_adcom'],
      \     [ 'nox', '<Plug>AM_acom'],
      \     [ 'nox', '<Plug>AM_abox'],
      \     [ 'nox', '<Plug>AM_a('],
      \     [ 'nox', '<Plug>AM_a='],
      \     [ 'nox', '<Plug>AM_a<'],
      \     [ ''   , '<Plug>AM_a,'],
      \     [ 'nox', '<Plug>AM_a?'],
      \   ],
      \ },
      \ }
      \ )
" }}}
" context_filetype.vim {{{
let s:bundle = neobundle#get('context_filetype.vim')
function! s:bundle.hooks.on_source(bundle)
  call neobundle#source('osyo-manga/vim-precious')
endfunction
" }}}
" clever-f.vim {{{
let g:clever_f_fix_key_direction = 0
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
  let g:neocomplete#force_omni_input_patterns.python =
        \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
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

  " Python
  let g:neocomplete#force_omni_input_patterns.python =
        \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
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
let s:bundle = neobundle#get('nerdtree')
function! s:bundle.hooks.on_source(bundle)
  " Don't use nerdtree when opening directory (use vimfiler.vim)
  let g:NERDTreeHijackNetrw = 0
  let g:NERDTreeMinimalUI = 1
  let g:NERDTreeDirArrows = 0
  let g:NERDTreeMapToggleHidden = '.'
  let g:NERDTreeShowHidden = 0
endfunction
" }}}
" unite-quickfix {{{
" }}}
" unite.vim {{{
call neobundle#config('unite.vim', {
      \ 'autoload': {
      \   'commands': [
      \     {
      \       'name': 'Unite',
      \       'complete': 'customlist,unite#complete#source',
      \     },
      \     {
      \       'name': 'UniteWithBufferDir',
      \       'complete': 'customlist,unite#complete#source',
      \     },
      \     'UniteWithCurrentDir', 'UniteWithProjectDir',
      \     'UniteWithInputDirectory', 'UniteWithCursorWord'
      \   ],
      \ },
      \ }
      \ )
let s:bundle = neobundle#get('unite.vim')
nnoremap <Leader>uu :Unite<Space>
nnoremap <silent> <Leader>ur :Unite register -buffer-name=register<CR>
nnoremap <silent> <Leader>uq :Unite -horizontal -no-quit quickfix<CR>
nnoremap <silent> <Leader>uf :Unite file_rec/async -buffer-name=file -create<CR>
nnoremap <silent> <Leader>ul :Unite file/async -buffer-name=file -create<CR>
nnoremap <silent> <Leader>um :Unite neomru/file -buffer-name=mru -create<CR>
nnoremap <silent> <Leader>ug :Unite grep<CR>

function! s:bundle.hooks.on_source(bundle)
  call unite#custom#profile('default', 'context', {
        \ 'start_insert': 1,
        \ 'direction': 'botright',
        \ 'prompt_direction': 'top',
        \ })

  call neobundle#source('neomru.vim')
  autocmd FileType unite imap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
endfunction
" }}}
" vim-altr {{{
nmap <Leader>a <Plug>(altr-forward)
nmap <Leader>A <Plug>(altr-back)
" }}}
" vim-fugitive {{{
call neobundle#config('vim-fugitive', {
      \ 'autoload': {
      \   'commands': [
      \     'Git',
      \     'Git!',
      \     'Gcd',
      \     'Glcd',
      \     'Gstatus',
      \     'Gcommit',
      \     'Gmerge',
      \     'Gpull',
      \     'Gpush',
      \     'Gfetch',
      \     'Ggrep',
      \     'Glgrep',
      \     'Glog',
      \     'Gllog',
      \     'Gedit',
      \     'Gsplit',
      \     'Gvsplit',
      \     'Gtabedit',
      \     'Gpedit',
      \     'Gsplit!',
      \     'Gvsplit!',
      \     'Gtabedit!',
      \     'Gpedit!',
      \     'Gread',
      \     'Gread!',
      \     'Gwrite',
      \     'Gwrite',
      \     'Gwq',
      \     'Gwq!',
      \     'Gdiff',
      \     'Gsdiff',
      \     'Gvdiff',
      \     'Gmove',
      \     'Gremove',
      \     'Gblame',
      \   ],
      \ },
      \ }
      \ )
" }}}
" vim-go {{{
let s:bundle = neobundle#get('vim-go')
function! s:bundle.hooks.on_source(bundle)
  let g:go_snippet_engine = 'neosnippet'
  let g:go_highlight_trailing_whitespace_error = 0
  let g:go_highlight_operators = 0
  let g:go_fmt_autosave = 0

  nmap <silent> <Leader>gf :GoFmt<CR>
  nmap <Leader>gi :GoImport<Space>
endfunction
" }}}
" vim-marching {{{
let g:marching_enable_neocomplete = 1
let g:marching_include_paths = filter(
      \ split(glob('/usr/lib/gcc/x86_64-pc-linux-gnu/*/include/*/'), '\n') +
      \ split(glob('/usr/include/boost/'), '\n') +
      \ split(glob('/usr/include/c++/'), '\n'),
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
" }}}
" vim-rails {{{
let s:bundle = neobundle#get('vim-rails')
function! s:bundle.hooks.on_source(bundle)
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
" }}}
" vim-ref {{{
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
" vim-snowdrop {{{
let s:bundle = neobundle#get('vim-snowdrop')
function! s:bundle.hooks.on_source(bundle)
  let g:snowdrop#libclang_directory = '/usr/lib/'
  let g:snowdrop#include_paths = {
        \ 'cpp': g:marching_include_paths,
        \ }
  let g:neocomplete#sources#snowdrop#enable = 1
  let g:neocomplete#skip_auto_completion_time = ''
endfunction
"}}}
" vim-stargate {{{
nmap <Leader>sg :StargateInclude<Space>
let s:bundle = neobundle#get('vim-stargate')
function! s:bundle.hooks.on_source(bundle)
  let g:stargate#include_paths = {
        \ 'cpp': g:marching_include_paths,
        \ }
endfunction
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
let g:windowswap_map_keys = 0
nnoremap <silent> <Leader>ww :call WindowSwap#EasyWindowSwap()<CR>
nnoremap <silent> <Leader>pw :call WindowSwap#DoWindowSwap()<CR>
nnoremap <silent> <Leader>yw :call WindowSwap#MarkWindowSwap()<CR>
" }}}
" vimfiler.vim {{{
nnoremap <silent> <Leader>f :VimFilerBufferDir -status<CR>

call neobundle#config('vimfiler.vim', {
      \ 'autoload': {
      \   'commands': [
      \     {
      \       'name': 'VimFiler',
      \       'complete': 'customlist,vimfiler#complete',
      \     },
      \     {
      \       'name': 'VimFilerCurrentDir',
      \       'complete': 'customlist,vimfiler#complete',
      \     },
      \     {
      \       'name': 'VimFilerBufferDir',
      \       'complete': 'customlist,vimfiler#complete',
      \     },
      \     {
      \       'name':  'VimFilerExplorer',
      \       'complete': 'customlist,vimfiler#complete',
      \     },
      \   ],
      \ },
      \ }
      \ )

let s:bundle = neobundle#get('vimfiler.vim')
function! s:bundle.hooks.on_source(bundle)
  " Override ':Unite file_rec/async' if in vimfiler.vim
  autocmd FileType vimfiler nmap <silent> <Leader>uf :UniteWithBufferDir file_rec/async<CR>

  " Disable netrw.vim
  let g:loaded_netrwPlugin = 1
  let g:vimfiler_as_default_explorer = 1
  let g:vimfiler_time_format = "%m/%d/%y %H:%M%S"
endfunction
" }}}
" vim-precious {{{
let s:bundle = neobundle#get('vim-precious')
function! s:bundle.hooks.on_source(bundle)
  call neobundle#source('vim-textobj-user')
endfunction
" }}}
" vimshell.vim {{{
call neobundle#config('vimshell.vim', {
      \ 'autoload': {
      \   'commands': [
      \     {
      \       'name': 'VimShell',
      \       'complete': 'customlist,vimshell#complete',
      \     },
      \     {
      \       'name': 'VimShellBufferDir',
      \       'complete': 'customlist,vimshell#complete',
      \     },
      \     'VimShellCreate',
      \     'VimShellTab',
      \     'VimShellPop',
      \     'VimShellCurrentDir',
      \     'VimShellExecute',
      \     'VimShellInteractive',
      \     'VimShellTerminal',
      \     'VimShellSendString',
      \     'VimShellSendBuffer',
      \   ],
      \ },
      \ }
      \ )

" VimShell with ,vs
nnoremap <silent> <Leader>vs :VimShellBuffer -split-command=vsplit<CR>
" Open VimShell vertically
nnoremap <silent> <Leader>vvs :VimShellBuffer -split-command=split<CR>
" Interactive python with ,py
nnoremap <silent> <Leader>py :VimShellInteractive python<CR>
nnoremap <silent> <Leader>irb :VimShellInteractive irb<CR>

let s:bundle = neobundle#get('vimshell.vim')
function! s:bundle.hooks.on_source(bundle)
  " Hit escape twice to exit vimshell
  autocmd FileType vimshell imap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
  autocmd FileType vimshell nmap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
  " Enter in normal mode goes into insertion mode first
  autocmd FileType vimshell nmap <silent> <buffer> <CR> A<CR>

  " Set user prompt to pwd
  let g:vimshell_prompt_expr = 'getcwd()." > "'
  let g:vimshell_prompt_pattern = '^\f\+ > '
endfunction

" }}}
" vinarise.vim {{{
let g:vinarise_enable_auto_detect = 1
" }}}
" wildfire.vim {{{
map <silent> <CR> <Plug>(wildfire-fuel)
vmap <silent> <BS> <Plug>(wildfire-water)
" }}}
" }}}

" Source local configurations if any {{{
if exists(expand('~/.vimrc_local'))
  source ~/.vimrc_local
endif
" }}}
