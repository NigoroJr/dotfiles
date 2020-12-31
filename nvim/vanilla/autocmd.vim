autocmd FileType text,vimshell setlocal textwidth=0
autocmd FileType ruby,html,xhtml,eruby,vim,yaml,toml,xml,cmake setlocal shiftwidth=2 tabstop=2 softtabstop=2
autocmd FileType python,scss setlocal shiftwidth=4 tabstop=4 softtabstop=4
autocmd FileType python setlocal textwidth=88
autocmd FileType go setlocal noexpandtab
autocmd FileType man setlocal nonumber noexpandtab shiftwidth=8 tabstop=8 softtabstop=8

" Make scripts executable if it's a script
function! s:make_executable(filename)
  let real_fname = resolve(expand("%"))
  let dirname = fnamemodify(real_fname, ":h")
  " Don't change permissions in a git repo
  execute "call system('cd " . dirname . " && git status')"
  let exit_status = v:shell_error
  if exit_status == 0
    return
  endif

  let line = getline(1)
  if line =~ "^#!" && line =~ "/bin"
    execute "silent !chmod a+x" real_fname
    filetype detect
  endif
endfunction
autocmd BufWritePost * call s:make_executable(@%)

" Automatically save/load views
let s:no_auto_view = ["gitcommit"]
augroup vimrc
  autocmd!
  autocmd BufWritePost *
        \   if expand("%") != "" &&
        \     &buftype !~ "nofile" &&
        \     index(s:no_auto_view, &filetype) == -1
        \|      mkview!
        \|  endif
  autocmd BufRead *
        \   if expand("%") != "" &&
        \     &buftype !~ "nofile" &&
        \     index(s:no_auto_view, &filetype) == -1
        \|      silent! loadview
        \|  endif
augroup END

augroup filetypedetect
  autocmd BufNewFile,BufRead *.launch setlocal filetype=xml
  autocmd BufNewFile,BufRead *.world setlocal filetype=xml
  autocmd BufNewFile,BufRead *.sdf setlocal filetype=xml
  autocmd BufNewFile,BufRead *.qml setlocal filetype=qml
augroup END
