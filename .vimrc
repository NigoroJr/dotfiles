set tabstop=4		"An indentation level every four columns"
set expandtab		"Convert all tabs typed into spaces"
set shiftwidth=4	"Indent/outdent by four columns"
set shiftround		"Always indent/outdent to the nearest tabstop"
set textwidth=78    "Maximum width of text"

set iminsert=0
set imsearch=0

syntax on

"setup for neobundle
filetype off
filetype plugin indent off

if has('vim_starting')
    set runtimepath+=~/.vim/bundle/neobundle.vim/
    call neobundle#rc(expand('~/.vim/bundle'))
endif

"Repositories to sync automatically
NeoBundle 'Shougo/neocomplcache'
NeoBundle 'Shougo/neobundle.vim'
NeoBundle 'thinca/vim-quickrun'
NeoBundle 'vim-scripts/sudo.vim'
NeoBundle 'Lokaltog/vim-easymotion'

filetype plugin indent on

"setup for neocomplcache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_snippets_dir = '~/.vim/snippets'
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3

"<TAB>: completion
imap <expr><TAB> neocomplcache#sources#snippets_complete#expandable() ? "\<Plug>(neocomplcache_snippets_expand)" : pumvisible() ? "\<C-n>" : "\<TAB>"

"<C-h>, <BS>: close popup and delete backward char
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()

inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()

imap <silent> <C-k> <Plug>(neocomplcache_snippets_expand)
smap <silent> <C-k> <Plug>(neocomplcache_snippets_expand)

"Toggle Snippet Editor
nnoremap <silent> <Space>es :<C-u>NeoComplCacheEditSnippets 
nnoremap <silent> es<Space> :<C-u>NeoComplCacheEditSnippets 
