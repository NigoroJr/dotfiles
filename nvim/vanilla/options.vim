set backup
set backupdir=~/.local/share/nvim/backups/
set viewdir=~/.local/share/nvim/view/
set undodir=~/.local/share/nvim/undo/
set undofile
" Create backup view, and undo directories
if !isdirectory(expand(&backupdir))
  call mkdir(&backupdir)
endif
if !isdirectory(expand(&viewdir))
  call mkdir(&viewdir)
endif
if !isdirectory(expand(&undodir))
  call mkdir(&undodir)
endif
set tabstop=4
set softtabstop=4
set expandtab
set shiftwidth=4
set shiftround
set textwidth=78
set number
set wildmode=longest,list
set foldenable
set foldmethod=marker
set viewoptions=cursor,folds
set fileencodings=ucs-bom,utf-8,iso-2022-jp,sjis,cp932,euc-jp,cp20932,utf-16,utf-16le,guess
set splitright
set splitbelow
set modeline
" set completeopt+=noinsert
" set completeopt+=noselect
set completeopt-=preview
set autoread
set showtabline=0
set formatoptions+=nmB
set updatetime=500
set autoindent
set smartindent
set formatoptions+=j
set formatlistpat=^\\s*\\(\\d\\+[\\]:.)}\\t\ ]\\\|[*+-]\\)\\s*
set matchpairs+=<:>

highlight Pmenu ctermbg=25 ctermfg=249
highlight PmenuSel ctermfg=252
syntax on
