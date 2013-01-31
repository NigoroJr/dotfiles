" ######################## Read Source ########################
if filereadable(expand('~/.vimrc_user'))
    source ~/.vimrc_user
endif
" #############################################################
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
NeoBundle 'Shougo/vimfiler'
NeoBundle 'tyru/open-browser.vim'
NeoBundle 'tpope/vim-markdown'
NeoBundle 'goldfeld/vim-seek'

filetype plugin indent on


" Settings for quickrun
let g:quickrun_config = {}
let g:quickrun_config['markdown'] = {
    \ 'outputter': 'browser'
    \ }


" Autoclose
let g:autoclose_on=0


" neosnippet settings
let g:neosnippet#snippets_directory = '~/.vim/snippets_complete/'


" Setup for neocomplcache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_min_syntax_length = 3

" <TAB>: completion
imap <expr><TAB> neosnippet#expandable() <Bar><bar> neosnippet#jumpable() ? "\<Plug>(neosnippet_expand_or_jump)" : pumvisible() ? "\<C-n>" : "\<TAB>"

" <C-h>, <BS>: close popup and delete backward char
inoremap <expr><C-h> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplcache#smart_close_popup()."\<C-h>"
inoremap <expr><C-y>  neocomplcache#close_popup()
inoremap <expr><C-e> neocomplcache#cancel_popup()

inoremap <expr><C-g> neocomplcache#undo_completion()
inoremap <expr><C-l> neocomplcache#complete_common_string()

imap <silent> <C-k> <Plug>(neosnippet_expand_or_jump)
smap <silent> <C-k> <Plug>(neosnippet_expand_or_jump)


" Vinarise
let g:vinarise_enable_auto_detect = 1


" unite.vim settings
" Start with insert mode
let g:unite_enable_start_insert=1
" Open Unite vertically
let g:unite_enable_split_vertically=1
" Recently used files
noremap <Leader>uru :Unite file_mru -buffer-name=file_mru<CR>
noremap <Leader>ureg :Unite register -buffer-name=register<CR>
noremap <Leader>ubu :Unite buffer -buffer-name=buffer<CR>
" File in current directory
noremap <Leader>uf :UniteWithBufferDir file -buffer-name=file<CR>
noremap <Leader>ure :UniteWithBufferDir file_rec -buffer-name=file_rec<CR>
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

" Settings for VimFiler
let g:vimfiler_as_default_explorer=1
let g:vimfiler_time_format="%m/%d/%y %H:%M%S"

" ##### Settings for VimShell #####
" VimShell with ,is
" nnoremap <silent> <Leader>is :VimShell<CR>
nnoremap <silent> <Leader>vs :VimShellBuffer -split<CR>
" Interactive python with ,ipe
nnoremap <silent> <Leader>ipe :VimShellInteractive perl<CR>
" Interactive python with ,ipy
nnoremap <silent> <Leader>ipy :VimShellInteractive python<CR>
" Hit escape twice to exit vimshell
autocmd FileType vimshell imap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)
autocmd FileType vimshell nmap <silent> <buffer> <Esc><Esc> <Plug>(vimshell_exit)

" ##### Settings for NeoSnippet #####
" Toggle NeoSnippet Editor
nnoremap <silent> es<Space> :<C-u>NeoSnippetEdit

" ##### Settings for VimFiler #####
" VimFiler with ,fl
nnoremap <silent> vf :VimFiler<CR>
