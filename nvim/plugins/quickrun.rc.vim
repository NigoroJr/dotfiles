let g:quickrun_config = {
      \ "_": {
      \   "runner": "neovim_job",
      \ },
      \ }
let g:quickrun_config.cpp = {
      \ "command": "g++",
      \ "cmdopt": "-std=c++11 -Wall -Wextra",
      \ }
let g:quickrun_config.markdown = {
      \ "runner": "vimscript",
      \ "exec": "PrevimOpen",
      \ "outputter": "null",
      \ "hook/time/enable": 0,
      \ }
