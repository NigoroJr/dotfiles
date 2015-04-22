if exists(':NeoBundleSource')
      \ && neobundle#is_installed('vim-colorschemes')
  call neobundle#source('vim-colorschemes')

  colorscheme molokai
  " Other good colorschemes
  " desert
  " gentooish
  " jellybeans
  " molokai
  " moria
  " solarazed
  " wombat
  " zenburn
endif

if has('gui_running')
  " Font for programming
  if has('gui_mac')
    set guifont=Ricty Discord:h16
  elseif has('gui_gtk')
    set guifont=Ricty\ Discord\ 9
  elseif has('gui_win32')
    set guifont=Ricty\ 13
  endif

  set guioptions-=m " Hide menubar
  set guioptions-=T " Hide toolbar
  set guioptions-=r " Hide right scrollbar
  set guioptions-=L " Hide left scrollbar

  set iminsert=2
endif
