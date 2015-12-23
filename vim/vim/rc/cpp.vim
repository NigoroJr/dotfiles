" C++ {{{
autocmd FileType cpp setlocal cinoptions+=(0
" Snippets
augroup cpp-namespace
  autocmd!
  autocmd FileType cpp inoremap <buffer> <expr> ; <SID>expand_namespace()
  autocmd FileType cpp inoremap <buffer> <expr> {<CR> <SID>class_declaration()
augroup END
function! s:expand_namespace()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<b;'
    return "\<BS>oost::"
  elseif s =~# '\<s;' && s[col('.')-2] != 's'
    return "\<BS>td::"
  elseif s =~# '\<d;'
    return "\<BS>etail::"
  else
    return ';'
  endif
endfunction

function! s:class_declaration()
  let s = getline('.')[0:col('.')-1]
  if s =~# '\<class\>' ||
        \ (s =~# '\<struct\>' && s !~# '\<typedef\>')
    return "{\<CR>};\<Esc>O"
  endif

  " Otherwise insert closing '}'
  return "{\<CR>}\<Esc>O"
endfunction

" Include paths
let s:mac_include_dir = ''
" Prefer brewed clang over Xcode {{{
if has('mac')
  let dirs = [
        \ '/usr/local/include/c++/v1',
        \ '/usr/include/c++/v1',
        \ '/usr/include/4.2.1',
        \ ]
  call filter(dirs, 'isdirectory(v:val)')
  if len(dirs) != 0
    " Select only the first
    let s:mac_include_dir = dirs[0]
  endif
endif
" }}}
let g:cpp_include_paths = filter(
      \ split(glob('/usr/lib/gcc/x86_64-pc-linux-gnu/*/include/*/'), '\n') +
      \ split(glob('/usr/include/boost/'), '\n') +
      \ [s:mac_include_dir],
      \ 'isdirectory(v:val)')
" Add syntax highlighting to STL header files
augroup cpp-stl-highlight
  autocmd!
  " Boost libraries have .hpp
  let stl_dirs = filter(deepcopy(g:cpp_include_paths), 'v:val !~ "boost"')
  let paths = join(map(stl_dirs, 'v:val . "/*"'), ',')
  " Remove duplicating path separators
  let paths = substitute(paths, '/\+', '/', 'g')
  execute "autocmd BufReadPost " . paths ." if empty(&filetype) | set filetype=cpp | endif"
augroup END
" }}}

" vim-altr {{{
if neobundle#tap('vim-altr')
  nmap <Leader>a <Plug>(altr-forward)
  nmap <Leader>A <Plug>(altr-back)

  call neobundle#untap()
endif
" }}}
" vim-marching {{{
if neobundle#tap('vim-marching')
  function! neobundle#hooks.on_source(bundle)
    let g:marching_include_paths = g:cpp_include_paths
    let g:marching_enable_neocomplete = 1
    let g:marching_clang_command = 'clang++'
    let g:marching#clang_command#options = {
          \ 'cpp': '-std=c++11',
          \ }

    set updatetime=100
    imap <C-x><C-o> <Plug>(marching_force_start_omni_complete)
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-snowdrop {{{
if neobundle#tap('vim-snowdrop')
  function! neobundle#hooks.on_source(bundle)
    let g:snowdrop#libclang_directory = '/usr/lib/'
    let g:snowdrop#include_paths = {
          \ 'cpp': g:cpp_include_paths,
          \ }
    let g:neocomplete#sources#snowdrop#enable = 1
    let g:neocomplete#skip_auto_completion_time = ''
  endfunction

  call neobundle#untap()
endif
"}}}
" vim-stargate {{{
if neobundle#tap('vim-stargate')
  nmap <Leader>sg :StargateInclude<Space>

  function! neobundle#hooks.on_source(bundle)
    let g:stargate#include_paths = {
          \ 'cpp': g:cpp_include_paths,
          \ }
  endfunction

  call neobundle#untap()
endif
" }}}
" vim-watchdogs {{{
if neobundle#tap('vim-watchdogs')
  function! neobundle#hooks.on_source(bundle)
    nmap <silent> <Leader>wd :WatchdogsRun<CR>

    let g:watchdogs_check_BufWritePost_enable = 1
    let g:quickrun_config['watchdogs_checker/_'] = {
          \ 'hook/close_quickfix/enable_exit': 1,
          \ 'hook/unite_quickfix/enable': 0,
          \ 'hook/close_unite_quickfix/enable_exit': 1,
          \ 'runner': 'vimproc',
          \ 'runner/vimproc/updatetime': 100,
          \ }
    " Use g++ (or clang++) rather than the default clang-check
    let checker = (executable('g++')
          \ ? 'watchdogs_checker/g++' : executable('clang++')
          \ ? 'watchdogs_checker/clang++' : '')
    let g:quickrun_config['cpp/watchdogs_checker'] = {
          \ 'type': checker,
          \ 'cmdopt': '-Wall',
          \ }

    call watchdogs#setup(g:quickrun_config)
  endfunction

  call neobundle#untap()
endif
" }}}
