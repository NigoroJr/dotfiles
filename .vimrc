" Setup runtimepath on start {{{
if has('vim_starting')
  set nocompatible
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif
" }}}
set backup
set backupdir=~/.vim/backup/
set viewdir=~/.vim/view/
set undodir=~/.vim/undo/
set tabstop=4
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
autocmd BufNewFile,BufRead *.tex set filetype=tex
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
nnoremap <silent> <Esc><Esc> :nohlsearch<CR>

" Move pwd to directory of buffer
nnoremap <Leader>cd :cd %:h<CR>

" Q to quit (overrides 'Q', but this is the same as 'gq')
nmap <silent> Q :q<CR>

" Toggle spell check
nmap <silent> <Leader>ss :call ToggleSpellcheck()<CR>
function! ToggleSpellcheck()
  silent setlocal spell!
  if &spell == 0
    echo 'Spellcheck OFF'
  else
    echo 'Spellcheck ON'
  endif
endfunction
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
  autocmd FileType cpp inoremap <buffer><expr>{<CR> <SID>class_declaration()
augroup END
function! s:expand_namespace()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<b;'
    return "\<BS>oost::"
  elseif s =~# '\<s;'
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

" Plugins managed by neobundle.vim {{{
call neobundle#begin(expand('~/.vim/bundle/'))

" NeoBundleFetch {{{
NeoBundleFetch 'Shougo/neobundle.vim'
NeoBundleFetch 'davidhalter/jedi'
" }}}
" NeoBundle {{{
NeoBundle 'ciaranm/securemodelines'
NeoBundle 'goldfeld/vim-seek'
NeoBundle 'mattn/disableitalic-vim'
NeoBundle 'rhysd/clever-f.vim'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/vimproc.vim'
NeoBundle 'Shougo/vinarise.vim'
NeoBundle 'tkztmk/vim-vala'
NeoBundle 'tomtom/tcomment_vim'
" }}}
" NeoBundleLazy {{{
NeoBundleLazy 'Align'
NeoBundleLazy 'c9s/perlomni.vim'
NeoBundleLazy 'davidhalter/jedi-vim'
NeoBundleLazy 'dbext.vim'
NeoBundleLazy 'dkasak/manpageview'
NeoBundleLazy 'fatih/vim-go'
NeoBundleLazy 'gcmt/wildfire.vim'
NeoBundleLazy 'haya14busa/incsearch.vim'
NeoBundleLazy 'hotchpotch/perldoc-vim'
NeoBundleLazy 'jceb/vim-hier'
NeoBundleLazy 'kana/vim-altr'
NeoBundleLazy 'kana/vim-textobj-user'
NeoBundleLazy 'kannokanno/previm'
NeoBundleLazy 'lervag/vim-latex'
NeoBundleLazy 'marcus/rsense'
NeoBundleLazy 'mattn/gist-vim'
NeoBundleLazy 'mattn/webapi-vim'
NeoBundleLazy 'mips.vim'
NeoBundleLazy 'NigoroJr/foofile.vim'
NeoBundleLazy 'osyo-manga/shabadou.vim'
NeoBundleLazy 'osyo-manga/unite-quickfix'
NeoBundleLazy 'osyo-manga/vim-marching'
NeoBundleLazy 'osyo-manga/vim-precious'
NeoBundleLazy 'osyo-manga/vim-snowdrop'
NeoBundleLazy 'osyo-manga/vim-stargate'
NeoBundleLazy 'osyo-manga/vim-watchdogs'
NeoBundleLazy 'rhysd/vim-go-impl'
NeoBundleLazy 'rhysd/wandbox-vim'
NeoBundleLazy 'scrooloose/nerdtree'
NeoBundleLazy 'Shougo/context_filetype.vim'
NeoBundleLazy 'Shougo/neocomplcache-rsense.vim'
NeoBundleLazy 'Shougo/neocomplcache.vim'
NeoBundleLazy 'Shougo/neomru.vim'
NeoBundleLazy 'Shougo/unite.vim'
NeoBundleLazy 'Shougo/vimfiler.vim'
NeoBundleLazy 'Shougo/vimshell.vim'
NeoBundleLazy 'sudo.vim'
NeoBundleLazy 'supermomonga/neocomplete-rsense.vim'
NeoBundleLazy 'TextFormat'
NeoBundleLazy 'thinca/vim-quickrun'
NeoBundleLazy 'thinca/vim-ref'
NeoBundleLazy 'tpope/vim-fugitive'
NeoBundleLazy 'tpope/vim-markdown'
NeoBundleLazy 'tpope/vim-rails'
NeoBundleLazy 'tyru/open-browser.vim'
NeoBundleLazy 'wesQ3/vim-windowswap'
" }}}

call neobundle#end()
filetype plugin indent on
NeoBundleCheck
" }}}

" Configurations for individual plugins {{{
" Align.vim {{{
if neobundle#tap('Align')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': ['Align', 'AlignCtrl'],
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
        \ })
  call neobundle#untap()
endif
" }}}
" context_filetype.vim {{{
if neobundle#tap('context_filetype.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'on_source': ['Shougo/neocomplete.vim'],
        \ },
        \ })

  call neobundle#untap()
endif
" }}}
" clever-f.vim {{{
let g:clever_f_fix_key_direction = 0
let g:clever_f_chars_match_any_signs = ''
let g:clever_f_across_no_line = 1
" }}}
" disableitalic.vim {{{
if neobundle#tap('disableitalic-vim')
  call neobundle#config({
        \ 'gui': 1,
        \ })
  call neobundle#untap()
endif
" }}}
" foofile.vim {{{
if neobundle#tap('foofile.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': [
        \     {
        \       'name': 'FooFile',
        \       'complete': 'customlist,foofile#complete',
        \     },
        \     {
        \       'name': 'FooFileLast',
        \       'complete': 'customlist,foofile#complete',
        \     },
        \     {
        \       'name': 'FooFileDelete',
        \       'complete': 'customlist,foofile#complete',
        \     },
        \     {
        \       'name': 'FooFileSaveAs',
        \       'complete': 'file',
        \     },
        \   ],
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" gist.vim {{{
if neobundle#tap('gist-vim')
  call neobundle#config({
    \ 'autoload': {
    \   'commands': [
    \     'Gist',
    \   ],
    \ },
    \ 'depends': 'mattn/webapi-vim',
    \ })

  call neobundle#untap()
endif
" }}}
" incsearch.vim {{{
if neobundle#tap('incsearch.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'mappings': [
        \     ['nxo', '<Plug>(incsearch-forward)'],
        \     ['nxo', '<Plug>(incsearch-backward)'],
        \     ['nxo', '<Plug>(incsearch-stay)'],
        \   ],
        \ },
        \ })

  map / <Plug>(incsearch-forward)
  map ? <Plug>(incsearch-backward)
  map g/ <Plug>(incsearch-stay)

  function! neobundle#hooks.on_source(bundle)
    let g:incsearch#consistent_n_direction = 1
    let g:incsearch#emacs_like_keymap = 1
    let g:incsearch#auto_no_hlsearch = 1
    map n  <Plug>(incsearch-nohl-n)
    map N  <Plug>(incsearch-nohl-N)
    map *  <Plug>(incsearch-nohl-*)
    map #  <Plug>(incsearch-nohl-#)
    map g* <Plug>(incsearch-nohl-g*)
    map g# <Plug>(incsearch-nohl-g#)
  endfunction

  call neobundle#untap()
endif
" }}}
" jedi-vim {{{
if neobundle#tap('jedi-vim')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'python',
        \ },
        \ 'depends': 'davidhalter/jedi',
        \ })

  let g:jedi#popup_select_first = 0
  let g:jedi#completions_enabled = 0
  let g:jedi#auto_vim_configuration = 0

  function! neobundle#hooks.on_source(bundle)
    autocmd FileType python setlocal omnifunc=jedi#completions
  endfunction

  call neobundle#untap()
endif
" }}}
" manpageview {{{
let g:manpageview_winopen = 'vsplit='
" }}}
" mips.vim {{{
if neobundle#tap('mips.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'asm',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" neocomplcache.vim {{{
if neobundle#tap('neocomplcache.vim')
  call neobundle#config({
        \ 'depends': 'Shougo/vimproc.vim',
        \ 'disabled': neobundle#is_sourced('neocomplete.vim'),
        \ })

  function! neobundle#hooks.on_source(bundle)
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
    let g:neocomplcache#force_omni_input_patterns.python =
          \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
    " Only if RSense is in use
    if neobundle#is_sourced('rsense')
      let g:neocomplcache#force_omni_input_patterns.ruby =
            \ '[^. *\t]\.\w*\|\h\w*::'
      call neobundle#source('neocomplcache-rsense.vim')
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplcache-rsense.vim {{{
if neobundle#tap('neocomplcache-rsense.vim')
  call neobundle#config({
        \ 'depends': ['Shougo/neocomplcache.vim', 'marcus/rsense'],
        \ })

  function! neobundle#hooks.on_source(bundle)
    let g:neocomplcache#sources#rsense#home_directory = g:rsenseHome
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplete.vim {{{
if neobundle#tap('neocomplete.vim')
  call neobundle#config({
        \ 'depends': ['Shougo/vimproc.vim'],
        \ 'disabled': !has('lua'),
        \ 'vim_version': '7.3.885',
        \ })

  function! neobundle#hooks.on_source(bundle)
    inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
    inoremap <expr><BS> neocomplete#smart_close_popup()."\<BS>"
    inoremap <expr><C-y> neocomplete#close_popup()
    inoremap <expr><C-g> neocomplete#cancel_popup()

    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#enable_smart_case = 1
    let g:neocomplete#min_keyword_length = 3

    if !exists('g:neocomplete#force_omni_input_patterns')
      let g:neocomplete#force_omni_input_patterns = {}
    endif

    let g:neocomplete#force_omni_input_patterns.cpp =
          \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'

    " perlomni.vim
    let g:neocomplete#force_omni_input_patterns.perl =
          \ '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?'
          " \ '[^. \t]->\%(\h\w*\)\?\|\h\w*::\%(\h\w*\)\?'

    " Go
    let g:neocomplete#force_omni_input_patterns.go =
          \ '[^.[:digit:] *\t]\.\w*'

    " Python
    let g:neocomplete#force_omni_input_patterns.python =
          \ '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'

    " Ruby (only if RSense is in use)
    " NOTE: Not guaranteed that rsense will be sourced before neocomplete.vim
    if neobundle#is_installed('rsense')
      let g:neocomplete#force_omni_input_patterns.ruby =
            \ '[^. *\t]\.\w*\|\h\w*::'
      call neobundle#source('neocomplete-rsense.vim')
    endif

  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    call neocomplete#initialize()
  endfunction

  call neobundle#untap()
endif
" }}}
" neocomplete-rsense.vim {{{
if neobundle#tap('neocomplete-rsense.vim')
  call neobundle#config({
        \ 'depends': ['Shougo/neocomplete.vim', 'marcus/rsense'],
        \ })

  function! neobundle#hooks.on_source(bundle)
    let g:neocomplete#sources#rsense#home_directory = g:rsenseHome
  endfunction

  call neobundle#untap()
endif
" }}}
" neomru.vim {{{
if neobundle#tap('neomru.vim')
  call neobundle#config({
        \ 'depends': 'Shougo/unite.vim',
        \ })
  call neobundle#untap()
endif
" }}}
" neosnippet.vim {{{
if neobundle#tap('neosnippet.vim')
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

  call neobundle#untap()
endif
" }}}
" nerdtree {{{
if neobundle#tap('nerdtree')
  call neobundle#config({
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
        \ })

  function! neobundle#hooks.on_source(bundle)
    " Don't use nerdtree when opening directory (use vimfiler.vim)
    let g:NERDTreeHijackNetrw = 0
    let g:NERDTreeMinimalUI = 1
    let g:NERDTreeDirArrows = 0
    let g:NERDTreeMapToggleHidden = '.'
    let g:NERDTreeShowHidden = 0
  endfunction

  call neobundle#untap()
endif
" }}}
" open-browser.vim {{{
if neobundle#tap('open-browser.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': 'OpenBrowser',
        \   'filetypes': 'markdown',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" perldoc-vim {{{
if neobundle#tap('perldoc-vim')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'perl',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" perlomni.vim {{{
if neobundle#tap('perlomni.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'perl',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" previm {{{
if neobundle#tap('previm')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': 'PrevimOpen',
        \   'filetypes': 'markdown',
        \ },
        \ })

  function! neobundle#hooks.on_source(bundle)
    if filereadable(expand('~/Dropbox/Vim/previm.css'))
      let g:previm_custom_css_path = expand('~/Dropbox/Vim/previm.css')
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
" rsense {{{
if neobundle#tap('rsense')

  function! neobundle#hooks.on_source(bundle)
    let g:rsenseUseOmniFunc = 1

    " Since it does basically the same thing as :MarchingClearCache
    " TODO: <Leader>cc for both MarchingClearCache and this?
    nmap <silent> <Leader>mc :RSenseClear<CR>
  endfunction

  call neobundle#untap()
endif
" }}}
" shabadou.vim {{{
if neobundle#tap('shabadou.vim')
  call neobundle#config({
        \ 'depends': 'thinca/vim-quickrun',
        \ })
  call neobundle#untap()
endif
" }}}
" sudo.vim {{{
if neobundle#tap('sudo.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': ['SudoRead', 'SudoWrite'],
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" TextFormat {{{
if neobundle#tap('TextFormat')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': ['AlignCenter', 'AlignJustify',
        \     'AlignRight', 'AlignLeft'
        \   ],
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" unite-quickfix {{{
if neobundle#tap('unite-quickfix')
  call neobundle#config({
        \ 'depends': 'Shougo/unite.vim',
        \ })
  call neobundle#untap()
endif
" }}}
" unite.vim {{{
if neobundle#tap('unite.vim')
  call neobundle#config({
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
        \ })

  nnoremap <Leader>uu :Unite<Space>
  nnoremap <silent> <Leader>ur :Unite register -buffer-name=register<CR>
  nnoremap <silent> <Leader>uq :Unite -horizontal -no-quit quickfix<CR>
  nnoremap <silent> <Leader>uf :Unite file_rec/async -buffer-name=file -create<CR>
  nnoremap <silent> <Leader>ul :Unite file/async -buffer-name=file -create<CR>
  nnoremap <silent> <Leader>um :Unite neomru/file -buffer-name=mru -create<CR>
  nnoremap <silent> <Leader>ug :Unite grep<CR>

  function! neobundle#hooks.on_source(bundle)
    call unite#custom#profile('default', 'context', {
          \ 'start_insert': 1,
          \ 'direction': 'botright',
          \ 'prompt_direction': 'top',
          \ })

    call neobundle#source('neomru.vim')
    autocmd FileType unite imap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-altr {{{
if neobundle#tap('vim-altr')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': ['c', 'cpp'],
        \   'mappings': [
        \     ['nxo', '<Plug>(altr-forward)'],
        \     ['nxo', '<Plug>(altr-back)'],
        \   ],
        \ },
        \ })
  call neobundle#untap()
endif
nmap <Leader>a <Plug>(altr-forward)
nmap <Leader>A <Plug>(altr-back)
" }}}
" vim-fugitive {{{
if neobundle#tap('vim-fugitive')
  call neobundle#config({
        \ 'augroup': 'fugitive',
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
        \ })
  call neobundle#untap()
endif
" }}}
" vim-go {{{
if neobundle#tap('vim-go')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'go',
        \ },
        \ })

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
" vim-go-impl {{{
if neobundle#tap('vim-go-impl')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'go',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" vim-hier {{{
if neobundle#tap('vim-hier')
  call neobundle#config({
        \ 'gui': 1,
        \ })
  call neobundle#untap()
endif
" }}}
" vim-latex {{{
if neobundle#tap('vim-latex')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': ['tex'],
        \ },
        \ })

  function! neobundle#hooks.on_source(bundle)
    let g:latex_indent_enabled = 1
    let g:latex_motion_matchparen = 0
  endfunction

  function! neobundle#hooks.on_post_source(bundle)
    " Start auto-compile
    silent call latex#latexmk#compile()

    " Re-set shiftwidth and tapstop
    set shiftwidth=4 tabstop=4
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-marching {{{
if neobundle#tap('vim-marching')
  call neobundle#config({
        \ 'depends': ['Shougo/neocomplete.vim', 'Shougo/vimproc.vim', 'osyo-manga/vim-reunions'],
        \ 'autoload': {
        \   'filetypes': ['c', 'cpp'],
        \   'commands': ['MarchingBufferClearCache'],
        \   'mappings': ['ixo', '<Plug>(marching_force_start_omni_complete)'],
        \ },
        \ })

  let g:marching_include_paths = filter(
        \ split(glob('/usr/lib/gcc/x86_64-pc-linux-gnu/*/include/*/'), '\n') +
        \ split(glob('/usr/include/boost/'), '\n') +
        \ split(glob('/usr/include/c++/*'), '\n'),
        \ 'isdirectory(v:val)')
  let g:marching_enable_neocomplete = 1
  let g:marching_clang_command = 'clang++'
  let g:marching#clang_command#options = {
        \ 'cpp': '-std=c++11',
        \ }

  function! neobundle#hooks.on_source(bundle)
    set updatetime=200
    nmap <Leader>mc :MarchingBufferClearCache<CR>
    imap <C-x><C-o> <Plug>(marching_force_start_omni_complete)
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-markdown {{{
if neobundle#tap('vim-markdown')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'markdown',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" vim-precious {{{
if neobundle#tap('vim-precious')
  call neobundle#config({
        \ 'autoload': {
        \   'on_source': ['context_filetype.vim'],
        \ },
        \ })

  call neobundle#untap()
endif
" }}}
" vim-quickrun {{{
if neobundle#tap('vim-quickrun')
  call neobundle#config({
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
        \ })

  nmap <silent> <Leader>r <Plug>(quickrun)

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
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': ['ruby', 'eruby', 'css', 'scss', 'html', 'javascript', 'yaml'],
        \ },
        \ })

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
    " Use RSense only in normal Ruby codes
    if !exists('b:rails_root') && &filetype == 'ruby'
      call neobundle#source('rsense')
    endif
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-ref {{{
if neobundle#tap('vim-ref')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': {
        \     'name': 'Ref',
        \     'complete': 'customlist,ref#complete',
        \   },
        \   'mappings': [
        \     ['nxo', '<Plug>(ref-keyword)'],
        \   ],
        \ },
        \ })

  let g:ref_detect_filetype = {
        \ 'sh': 'man',
        \ 'zsh': 'man',
        \ 'bash': 'man',
        \ }
  let g:ref_use_vimproc = 1
  nmap K <Plug>(ref-keyword)

  call neobundle#untap()
endif
" }}}
" vim-seek {{{
let g:seek_ignorecase = 1
" }}}
" vim-snowdrop {{{
if neobundle#tap('vim-snowdrop')
  call neobundle#config({
        \ 'autoload': {
        \   'filetypes': 'cpp',
        \ },
        \ })

  function! neobundle#hooks.on_source(bundle)
    let g:snowdrop#libclang_directory = '/usr/lib/'
    let g:snowdrop#include_paths = {
          \ 'cpp': g:marching_include_paths,
          \ }
    let g:neocomplete#sources#snowdrop#enable = 1
    let g:neocomplete#skip_auto_completion_time = ''
  endfunction

  call neobundle#untap()
endif
"}}}
" vim-stargate {{{
if neobundle#tap('vim-stargate')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': 'StargateInclude',
        \   'filetypes': 'cpp',
        \ },
        \ })

  nmap <Leader>sg :StargateInclude<Space>

  function! neobundle#hooks.on_source(bundle)
    let g:stargate#include_paths = {
          \ 'cpp': g:marching_include_paths,
          \ }
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-textobj-user {{{
if neobundle#tap('vim-textobj-user')
  call neobundle#config({
        \ 'autoload': {
        \   'on_source': ['osyo-manga/vim-precious'],
        \ },
        \ })

  call neobundle#untap()
endif
" }}}
" vim-watchdogs {{{
if neobundle#tap('vim-watchdogs')
  call neobundle#config({
        \ 'gui': 1,
        \ 'filetypes': ['c', 'cpp'],
        \ 'depends': ['jceb/vim-hier', 'thinca/vim-quickrun'],
        \ })

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
  call neobundle#config({
        \ 'autoload': {
        \   'functions': ['WindowSwap#MarkWindowSwap',
        \     'WindowSwap#DoWindowSwap', 'WindowSwap#EasyWindowSwap',
        \   ],
        \ },
        \ })

  let g:windowswap_map_keys = 0
  nnoremap <silent> <Leader>ww :call WindowSwap#EasyWindowSwap()<CR>
  nnoremap <silent> <Leader>pw :call WindowSwap#DoWindowSwap()<CR>
  nnoremap <silent> <Leader>yw :call WindowSwap#MarkWindowSwap()<CR>

  call neobundle#untap()
endif
" }}}
" vimfiler.vim {{{
if neobundle#tap('vimfiler.vim')
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
        \ })

  nnoremap <silent> <Leader>f :VimFilerBufferDir -status<CR>

  function! neobundle#hooks.on_source(bundle)
    " vim-rails maps <Leader>uf to file_rec/async:!
    if !neobundle#is_sourced('vim-rails')
      " Otherwise override ':Unite file_rec/async' if in vimfiler.vim
      autocmd FileType vimfiler nmap <silent> <Leader>uf :UniteWithBufferDir file_rec/async<CR>
      autocmd FileType vimfiler nmap <silent> <Leader>ul :UniteWithBufferDir file/async<CR>
    endif

    " Disable netrw.vim
    let g:loaded_netrwPlugin = 1
    let g:vimfiler_as_default_explorer = 1
    let g:vimfiler_time_format = "%m/%d/%y %H:%M%S"
  endfunction

  call neobundle#untap()
endif
" }}}
" vimproc.vim {{{
if neobundle#tap('vimproc.vim')
  call neobundle#config({
        \ 'build': {
        \   'windows': 'mingw32-make -f make_mingw32.mak',
        \   'cygwin': 'make -f make_cygwin.mak',
        \   'mac': 'make -f make_mac.mak',
        \   'unix': 'make -f make_unix.mak',
        \ },
        \ })
  call neobundle#untap()
endif
" }}}
" vimshell.vim {{{
if neobundle#tap('vimshell.vim')
  call neobundle#config({
        \ 'depends': ['Shougo/vimproc.vim'],
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
        \ })

  " VimShell with ,vs
  nnoremap <silent> <Leader>vs :VimShellBuffer -split-command=vsplit<CR>
  " Open VimShell vertically
  nnoremap <silent> <Leader>vvs :VimShellBuffer -split-command=split<CR>
  " Interactive python with ,py
  nnoremap <silent> <Leader>py :VimShellInteractive python<CR>
  nnoremap <silent> <Leader>irb :VimShellInteractive irb<CR>

  function! neobundle#hooks.on_source(bundle)
    " Hit escape twice to exit vimshell
    autocmd FileType vimshell imap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
    autocmd FileType vimshell nmap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
    " Enter in normal mode goes into insertion mode first
    autocmd FileType vimshell nmap <silent> <buffer> <CR> A<CR>

    " Set user prompt to pwd
    let g:vimshell_prompt_expr = 'getcwd()." > "'
    let g:vimshell_prompt_pattern = '^\f\+ > '
  endfunction
endif

" }}}
" vinarise.vim {{{
let g:vinarise_enable_auto_detect = 1
" }}}
" wandbox-vim {{{
if neobundle#tap('wandbox-vim')
  call neobundle#config({
        \ 'autoload': {
        \   'commands': ['Wandbox', 'WandboxAsync'],
        \ },
        \ 'depends': ['thinca/vim-quickrun', 'Shougo/vimproc.vim'],
        \ })
  call neobundle#untap()
endif
" }}}
" wildfire.vim {{{
if neobundle#tap('wildfire.vim')
  call neobundle#config({
        \ 'autoload': {
        \   'mappings': [
        \     ['nxo', '<Plug>(wildfire-water)'],
        \     ['nxo', '<Plug>(wildfire-fuel)'],
        \   ],
        \ },
        \ })

  map <silent> <CR> <Plug>(wildfire-fuel)
  vmap <silent> <BS> <Plug>(wildfire-water)

  call neobundle#untap()
endif
" }}}
" }}}

" Source local configurations if any {{{
if filereadable(expand('~/.vimrc_local'))
  source ~/.vimrc_local
endif
" }}}
