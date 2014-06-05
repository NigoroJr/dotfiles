"set noswapfile      " No swap files
set backup          " We all know backing up is important
set tabstop=4       " An indentation level every four columns
set expandtab       " Convert all tabs typed into spaces
set shiftwidth=4    " Indent/outdent by four columns
set shiftround      " Always indent/outdent to the nearest tabstop
set textwidth=78    " Maximum width of text
set number
set wildmode=longest,list
set foldenable
set foldmethod=marker

set viewoptions=cursor,folds
set hlsearch
set encoding=utf-8
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932,guess
set backspace=indent,eol,start
" Reset hilight search by pressing Escape 2 times
nnoremap <silent> <ESC><ESC> :nohlsearch<CR>
let mapleader=","   " Sets mapleader to ,
noremap \ ,

set splitright      " split to the right
set splitbelow      " split below

" Use modeline
set modeline

" Create backup dir if it doesn't exist
if !isdirectory(expand('~/.vim/backup/'))
    call mkdir(expand("~/.vim/backup/"))
endif
set backupdir=~/.vim/backup/

syntax on

" Set textwidth 0 when using text files
autocmd FileType text,vimshell set textwidth=0

" Set shiftwidth to 2 for particular files
autocmd FileType python,ruby,html set shiftwidth=2 tabstop=2

" setup for neobundle.vim {{{
filetype off
filetype plugin indent off

let s:neobundle_dir = expand('~/.vim/bundle')
if has('vim_starting')
    if isdirectory('neobundle.vim')
        set runtimepath^=neobundle.vim
    elseif finddir('neobundle.vim', '.;') != ''
        execute 'set runtimepath^=' . finddir('neobundle.vim')
    elseif &runtimepath !~ '/neobundle.vim'
        if !isdirectory(s:neobundle_dir.'/neobundle.vim')
            execute printf('get clone %s://github.com/Shougo/neobundle.vim.git',
                        \ (exists('$http_proxy') ? 'https' : 'git'))
                        \ s:neobundle_dir.'/neobundle.vim'
        endif

        execute 'set runtimepath^=' . s:neobundle_dir.'/neobundle.vim'
    endif
endif
call neobundle#rc(s:neobundle_dir)
" }}}

" neobundle.vim {{{
NeoBundleFetch 'Shougo/neobundle.vim'
"NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/vimshell.vim'
NeoBundle 'Shougo/vimproc.vim', {
\    'build' : {
\        'windows' : 'make -f make_mingw32.mak',
\        'cygwin' : 'make -f make_cygwin.mak',
\        'mac' : 'make -f make_mac.mak',
\        'unix' : 'make -f make_unix.mak',
\    },
\}
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'thinca/vim-ref'
NeoBundle 'vim-scripts/sudo.vim'
NeoBundle 'ciaranm/securemodelines'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'taku-o/vim-toggle'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/vimfiler.vim'
NeoBundle 'goldfeld/vim-seek'
NeoBundle 'tpope/vim-rails'
NeoBundle 'scrooloose/nerdtree'
NeoBundle 'vim-scripts/dbext.vim'
NeoBundle 'osyo-manga/vim-marching', {
            \ 'depends' : ['Shougo/vimproc.vim', 'osyo-manga/vim-reunions'],
            \ 'autoload' : {'filetypes' : ['c', 'cpp']}
            \ }
NeoBundle 'osyo-manga/vim-stargate', {
            \ 'autoload' : {'filetypes' : ['cpp']}
            \ }
NeoBundle 'rhysd/wandbox-vim'
NeoBundle 'kana/vim-altr'
NeoBundle 'osyo-manga/unite-quickfix'
NeoBundle 'osyo-manga/shabadou.vim'
NeoBundle 'gcmt/wildfire.vim'
NeoBundle 'wesQ3/vim-windowswap'
NeoBundle 'tkztmk/vim-vala'
NeoBundle 'vim-scripts/mips.vim'
NeoBundle 'davidhalter/jedi-vim'
" NeoBundleLazy 'osyo-manga/vim-watchdogs'
" NeoBundleLazy 'jceb/vim-hier'
NeoBundleLazy 'dkasak/manpageview'
NeoBundleLazy 'Shougo/vinarise.vim'
" NeoBundleLazy 'vim-scripts/DrawIt'
" NeoBundleLazy 'Lokaltog/vim-easymotion'
NeoBundle 'tyru/open-browser.vim', {
            \ 'autoload' : {
            \   'filetypes' : ['markdown']
            \ }}
NeoBundleLazy 'tpope/vim-markdown', {
            \ 'autoload' : {
            \   'filetypes' : ['markdown']
            \ }}

filetype plugin indent on

" }}}

" quickrun {{{
let g:quickrun_config = {
            \ '_' : {
            \ 'hook/time/enable': 0,
            \ 'hook/close_unite_quickfix/enable_hook_loaded' : 1,
            \ 'hook/unite_quickfix/enable_failure': 1,
            \ 'hook/close_quickfix/enable_exit': 1,
            \ 'hook/close_buffer/enable_empty_data': 1,
            \ 'hook/close_buffer/enable_failure': 1,
            \ 'outputter': 'multi:buffer:quickfix',
            \ 'outputter/buffer/close_on_empty': 1,
            \ 'hook/quickfix_replate_tempname_to_bufnr/enable_exit' : 1,
            \ 'hook/quickfix_replate_tempname_to_bufnr/priority_exit' : -10,
            \ 'runmode': 'async:remote:vimproc',
            \ 'runner': 'vimproc',
            \ 'runner/vimproc/updatetime': 60,
            \ },
            \ 'make': {
            \ 'command': 'make',
            \ 'exec': "%c %o",
            \ 'runner': 'vimproc',
            \ },
            \ 'watchdogs_checker/_' : {
            \ 'hook/close_quickfix/enable_exit': 1,
            \ 'hook/unite_quickfix/enable': 0,
            \ 'hook/close_unite_quickfix/enable_exit': 1,
            \ },
            \ }
let g:quickrun_config.cpp = {
            \ 'command': 'g++',
            \ 'cmdopt': '-std=c++11 -Wall -Wextra',
            \ 'hook/quickrunex/enable': 1,
            \ }
let g:quickrun_config.markdown = {
            \ 'outputter': 'browser',
            \ 'hook/time/enable': 0,
            \ }
" }}}

" watchdogs.vim
" let g:watchdogs_check_BufWritePost_enable = 1
" call watchdogs#setup(g:quickrun_config)
" nmap <silent> <Leader>wd :WatchdogsRun<CR>

" vim-seek
let g:seek_ignorecase = 1

" neocomplcache {{{
" let g:neocomplcache_enable_at_startup = 1
" let g:neocomplcache_enable_smart_case = 1
" let g:neocomplcache_enable_camel_case_completion = 1
" let g:neocomplcache_enable_underbar_completion = 1
" let g:neocomplcache_min_syntax_length = 3

" <C-h>, <BS>: close popup and delete backward char
" inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
" inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
" inoremap <expr><C-y>  neocomplcache#close_popup()
"inoremap <expr><C-e> neocomplcache#cancel_popup()
" inoremap <expr><C-g> neocomplcache#undo_completion()
" inoremap <expr><C-l> neocomplcache#complete_common_string()

" }}}

" neocomplete {{{
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

" }}}

" vim-marching
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

" vim-stargate
let g:stargate#include_paths = {
            \ "cpp" : marching_include_paths
            \ }

nmap <Leader>sg :StargateInclude<Space>

" vinarise
let g:vinarise_enable_auto_detect = 1

" unite.vim {{{
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

" vimshell {{{
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

" zsh-like completion
" Figuring out what this does
" autocmd FileType vimshell nmap <silent> <buffer> <C-k> <Plug>(vimshell_zsh_complete)

" }}}

" neosnippet {{{
" Toggle NeoSnippet Editor
nnoremap <silent> <Leader>es :<C-u>NeoSnippetEdit

" neosnippet settings
let g:neosnippet#snippets_directory = '~/.vim/snippets/'

" <TAB>: completion
imap <expr><TAB> neosnippet#expandable() <Bar><bar> neosnippet#jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? "\<C-n>" : "\<TAB>"

imap <silent> <C-k> <Plug>(neosnippet_expand_or_jump)
smap <silent> <C-k> <Plug>(neosnippet_expand_or_jump)

" }}}

" vimfiler {{{
let g:vimfiler_as_default_explorer=1
let g:vimfiler_time_format="%m/%d/%y %H:%M%S"

" VimFiler with ,fl
nnoremap <silent> <Leader>vf :VimFiler<CR>

" }}}

" Disable preview window
set completeopt-=preview

" ref.vim
let g:ref_open='vsplit'
let g:ref_detect_filetype = {
            \ 'sh': 'man',
            \ 'zsh': 'man',
            \ 'bash': 'man',
            \ }
let g:ref_use_vimproc=1

" manpageview.vim
let g:manpageview_winopen='vsplit='

" swap windows
let g:windowswap_map_keys = 0 "prevent default bindings
nnoremap <silent> <leader>yw :call WindowSwap#MarkWindowSwap()<CR>
nnoremap <silent> <leader>pw :call WindowSwap#DoWindowSwap()<CR>

" jedi-vim
autocmd FileType python setlocal omnifunc=jedi#completions
"let g:jedi#popup_select_first=0
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'

" Anywhere SID.
function! s:SID_PREFIX()
    return matchstr(expand('<sfile>'), '<SNR>\d\+_\zeSID_PREFIX$')
endfunction

" Set tabline.
function! s:my_tabline()  "{{{
    let s = ''
    for i in range(1, tabpagenr('$'))
        let bufnrs = tabpagebuflist(i)
        let bufnr = bufnrs[tabpagewinnr(i) - 1]  " first window, first appears
        let no = i  " display 0-origin tabpagenr.
        let mod = getbufvar(bufnr, '&modified') ? '!' : ' '
        let title = fnamemodify(bufname(bufnr), ':t')
        let title = '[' . title . ']'
        let s .= '%'.i.'T'
        let s .= '%#' . (i == tabpagenr() ? 'TabLineSel' : 'TabLine') . '#'
        let s .= no . ':' . title
        let s .= mod
        let s .= '%#TabLineFill# '
    endfor
    let s .= '%#TabLineFill#%T%=%#TabLine#'
    return s
endfunction "}}}
let &tabline = '%!'. s:SID_PREFIX() . 'my_tabline()'
" set showtabline=2 " Always show tabline

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

" C-d is "Delete"
inoremap <C-d> <Del>

" Move by physical/logical line
nnoremap gj j
nnoremap gk k
nnoremap j gj
nnoremap k gk

" More powerful ex command history
cnoremap <C-p> <Up>
cnoremap <C-n> <Down>

" Insert closing braces {{{
fun! CloseBraces()
    " Don't do this for LaTeX documents
    if &filetype !~ 'tex\|plaintex'
        inoremap {<CR> {<CR>}<Esc>O
    endif
endfun
autocmd FileType * call CloseBraces()
" }}}

" Move pwd to directory of buffer
nnoremap <silent><Leader>cd :cd %:h<CR>

" Save as super user
nnoremap <Leader>ws :w sudo:%<CR>
nnoremap <Leader>xs :x sudo:%<CR>
nnoremap <Leader>es :e sudo:%<CR>

" vim-altr
nmap <Leader>a <Plug>(altr-forward)
nmap <Leader>A <Plug>(altr-back)

" rails.vim used to have a Rtree command
command Rtree NERDTreeFind

" Map K to ref.vim
nmap K <Plug>(ref-keyword)

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

" Source local configurations if any
if exists(expand('~/.vimrc_local'))
    source ~/.vimrc_local
endif
