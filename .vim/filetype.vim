if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect
  autocmd BufRead,BufNewFile Gemfile.lock setlocal filetype=ruby
  autocmd BufRead,BufNewFile *.toml setlocal filetype=toml
  autocmd BufRead,BufNewFile *.vala setlocal filetype=vala
  autocmd BufRead,BufNewFile *.coffee setlocal filetype=coffee
  autocmd BufRead,BufNewFile *.slim setlocal filetype=slim
  autocmd BufRead,BufNewFile *.r setlocal filetype=r
  autocmd BufRead,BufNewFile *.R setlocal filetype=r
  autocmd BufNewFile,BufRead *.tex setlocal filetype=tex
augroup END
