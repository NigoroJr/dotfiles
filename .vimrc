" Source separate files
if filereadable(expand('~/.vimrc.user'))
    source ~/.vimrc.user
endif
if filereadable(expand('~/.vimrc.keybindings'))
    source ~/.vimrc.keybindings
endif
" ----------------------------------

syntax on

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
NeoBundle 'Shougo/neobundle.vim'
"NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neocomplete'
NeoBundle 'Shougo/vimshell'
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
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/unite.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'Shougo/vimfiler'
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
NeoBundleLazy 'dkasak/manpageview'
NeoBundleLazy 'Shougo/vinarise'
NeoBundleLazy 'vim-scripts/DrawIt'
NeoBundleLazy 'Lokaltog/vim-easymotion'
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
            \ 'hook/close_unite/enable_failure': 1,
            \ 'hook/close_quickfix/enable_exit': 1,
            \ 'hook/close_buffer/enable_empty_failure': 1,
            \ 'hook/close_buffer/enable_empty_data': 1,
            \ 'outputter': 'multi:buffer:quickfix',
            \ 'outputter/buffer/close_on_empty': 1,
            \ 'runmode': 'async:remote:vimproc',
            \ 'runner': 'vimproc',
            \ 'runner/vimproc/updatetime': 60,
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
            \ split(glob('/usr/include/c++/*'), '\n') +
            \ split(glob('/usr/include/*/c++/*'), '\n') +
            \ split(glob('/usr/include/*/'), '\n') +
            \ split(glob('/usr/lib/gcc/**/'), '\n') +
            \ split(glob('/usr/local/include/*/'), '\n') +
            \ [
            \ '/usr/local/include/',
            \ ],
            \ 'isdirectory(v:val)')
set updatetime=200

" vim-stargate
let g:stargate#include_paths = {
            \ "cpp" : marching_include_paths
            \ }

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
