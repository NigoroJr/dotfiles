" vim: foldmethod=marker
"colorscheme ansi_blows
colorscheme molokai
" Favorites
" 'desert'      : 3
" 'gentooish'   : 7
" 'jellybeans'  : 5
" 'molokai'     : 9
" 'moria'       : 8
" 'solarazed'   : 9
" 'wombat'      : 3
" 'zenburn'     : 4

" Font for programming
if has('gui_running')
    if has('gui_mac')
        set guifont=Ricty Discord:h16
    elseif has('gui_gtk')
        set guifont=Ricty\ Discord\ 9
    elseif has('gui_win32')
        set guifont=Ricty\ 13
    endif
endif

set guioptions-=m " Hide menubar
set guioptions-=T " Hide toolbar
set guioptions-=r " Hide right scrollbar
set guioptions-=L " Hide left scrollbar

NeoBundleSource 'vim-watchdogs' 
NeoBundleSource 'vim-hier'
" {{{
let g:watchdogs_check_BufWritePost_enable = 1
call watchdogs#setup(g:quickrun_config)
nmap <silent> <Leader>wd :WatchdogsRun<CR>
" }}}
