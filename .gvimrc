if exists('*neobundle#is_installed')
  if neobundle#is_installed('vim-colorschemes')
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
  end

  if neobundle#is_installed('disableitalic-vim')
    call neobundle#source('disableitalic-vim')
    DisableItalic
  end
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

if filereadable(expand('~/.localrc/gvimrc'))
  source ~/.localrc/gvimrc
end
