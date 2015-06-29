if exists('did_load_filetypes')
  finish
endif

augroup filetypedetect
  autocmd BufRead,BufNewFile Gemfile.lock set filetype=ruby
  autocmd BufRead,BufNewFile *.toml set filetype=toml
  autocmd BufRead,BufNewFile *.vala set filetype=vala
  autocmd BufRead,BufNewFile *.coffee set filetype=coffee
augroup END
