set nobackup        " No backup files
set noswapfile      " No swap files
set tabstop=4		" An indentation level every four columns
set expandtab		" Convert all tabs typed into spaces
set shiftwidth=4	" Indent/outdent by four columns
set shiftround		" Always indent/outdent to the nearest tabstop
set textwidth=78    " Maximum width of text
let mapleader=";"   " Sets mapleader to ;
set number
set hlsearch
" Reset hilight search by pressing Escape 2 times
:nnoremap <ESC><ESC> :nohlsearch<CR>

syntax on

" setup for neobundle
filetype off
filetype plugin indent off

if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim/
    call neobundle#rc(expand('~/.vim/bundle'))
endif

" Repositories to sync automatically
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/vimshell'
NeoBundle 'Shougo/vimproc'
NeoBundle 'Shougo/vinarise'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'thinca/vim-ref'
NeoBundle 'vim-scripts/sudo.vim'
NeoBundle 'vim-scripts/DrawIt'
NeoBundle 'Lokaltog/vim-easymotion'
NeoBundle 'tomtom/tcomment_vim'
NeoBundle 'taku-o/vim-toggle'
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/unite.vim'

filetype plugin indent on

" Settings for VimShell
" VimShell with ,is
nnoremap <silent> ,is :VimShell<CR>
" Interactive pythong with ,ipy
nnoremap <silent> ,ipy :VimShellInteractive python<CR>

" Setup for neocomplcache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3

" neosnippet settings
let g:neosnippet#snippets_directory = '~/.vim/bundle/snippets_complete/'

" SuperTab like snippets behavior.
imap <expr><TAB> neosnippet#expandable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For snippet_complete marker
if has('conceal')
    set conceallevel=2 concealcursor=i
endif

" <C-h>, <BS>: close popup and delete backward char
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()

inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()

imap <silent> <C-k> <Plug>(neosnippet_expand_or_jump)
smap <silent> <C-k> <Plug>(neosnippet_expand_or_jump)

" Toggle Snippet Editor
nnoremap <silent> es<Space> :<C-u>NeoSnippetEdit

" Vinarise
let g:vinarise_enable_auto_detect = 1

" unite.vim settings
" Start with insert mode
let g:unite_enable_start_insert=1
" Open Unite vertically
let g:unite_enable_split_vertically=1
