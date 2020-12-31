" Don't go further if feature size is tiny or small
if 0 | endif

" Measure startup time
function! s:show_elapsed_millisec(start) abort
  if &filetype == "man"
    return
  endif
  let duration = str2float(reltimestr(reltime(a:start)))
  let duration = duration * 1000
  echomsg string(duration) . "ms"
endfunction
let s:start = reltime()
autocmd VimEnter * call s:show_elapsed_millisec(s:start)

let g:config_dir = fnamemodify(expand("<sfile>"), ":h") . "/"

function! s:source_config(fname) abort
  let b:fpath = join([g:config_dir, a:fname], "/")
  if filereadable(expand(b:fpath))
    execute "source" expand(b:fpath)
  endif
endfunction

" Options
call s:source_config("vanilla/options.vim")

" autocmd
call s:source_config("vanilla/autocmd.vim")

" Key bindings
call s:source_config("vanilla/general_mappings.vim")

" Python providers
if executable("/usr/bin/python2.7")
  let g:python_host_prog = "/usr/bin/python2.7"
endif
if executable($HOME . "/.cache/nvim/python3/bin/python3")
  let g:python3_host_prog = $HOME . "/.cache/nvim/python3/bin/python3"
endif

" dein configs
let s:cache_home = empty($XDG_CACHE_HOME) ? expand("~/.cache") : $XDG_CACHE_HOME
let s:dein_dir = s:cache_home . "/dein"
let s:dein_repo_dir = s:dein_dir . "/repos/github.com/Shougo/dein.vim"
if !isdirectory(s:dein_repo_dir)
  call system("git clone https://github.com/Shougo/dein.vim "
              \ . shellescape(s:dein_repo_dir))
endif
let &runtimepath = s:dein_repo_dir .",". &runtimepath
" Load plugins
if dein#load_state(s:dein_dir)
  call dein#begin(s:dein_dir)

  call dein#load_toml(g:config_dir . "plugins/toml/essentials.toml", {"lazy": 0})

  call dein#load_toml(g:config_dir . "plugins/toml/autocomplete.toml", {"lazy": 1})
  call dein#load_toml(g:config_dir . "plugins/toml/format.toml", {"lazy": 1})
  call dein#load_toml(g:config_dir . "plugins/toml/highlight.toml", {"lazy": 1})
  call dein#load_toml(g:config_dir . "plugins/toml/navigation.toml", {"lazy": 1})
  call dein#load_toml(g:config_dir . "plugins/toml/quickrun.toml", {"lazy": 1})

  call dein#end()
  call dein#save_state()
endif

filetype plugin indent on
syntax enable

if has("vim_starting") && dein#check_install()
  let g:dein#install_progress_type = "tabline"
  call dein#install()
endif

" Load locally-defined configs
function! s:load_local(path) abort
  if filereadable(expand(a:path))
    execute "source" expand(a:path)
  endif
endfunction
call s:load_local("~/.localrc/vimrc")
