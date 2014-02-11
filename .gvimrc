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
        set guifont=Ricty\ Discord\ 10
    elseif has('gui_win32')
        set guifont=Ricty\ 13
    endif
endif
