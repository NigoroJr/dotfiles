" .zshrc as sh
augroup filetypedetect
    autocmd BufRead,BufNewFile .zshrc* setfiletype sh
    autocmd BufRead,BufNewFile .*alias* setfiletype sh
augroup END
