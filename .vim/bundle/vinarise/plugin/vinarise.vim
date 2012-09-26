"=============================================================================
" FILE: vinarise.vim
" AUTHOR:  Shougo Matsushita <Shougo.Matsu@gmail.com>
" Last Modified: 10 Sep 2012.
" License: MIT license  {{{
"     Permission is hereby granted, free of charge, to any person obtaining
"     a copy of this software and associated documentation files (the
"     "Software"), to deal in the Software without restriction, including
"     without limitation the rights to use, copy, modify, merge, publish,
"     distribute, sublicense, and/or sell copies of the Software, and to
"     permit persons to whom the Software is furnished to do so, subject to
"     the following conditions:
"
"     The above copyright notice and this permission notice shall be included
"     in all copies or substantial portions of the Software.
"
"     THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
"     OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
"     MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
"     IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
"     CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
"     TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
"     SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
" }}}
"=============================================================================

if exists('g:loaded_vinarise')
  finish
elseif v:version < 702
  echoerr 'vinarise does not work this version of Vim (' . v:version . ').'
  finish
endif

let s:save_cpo = &cpo
set cpo&vim

" Global options definition."{{{
let g:vinarise_enable_auto_detect =
      \ get(g:, 'vinarise_enable_auto_detect', 0)
let g:vinarise_detect_large_file_size =
      \ get(g:, 'vinarise_detect_large_file_size', 10000000)
let g:vinarise_cursor_ascii_highlight =
      \ get(g:, 'vinarise_cursor_ascii_highlight', 'Search')
"}}}

command! -nargs=* -complete=customlist,vinarise#complete Vinarise
      \ call s:call_vinarise({}, <q-args>)
command! -nargs=? -complete=customlist,vinarise#complete VinariseDump
      \ call vinarise#dump#open(<q-args>, 0)
command! -nargs=* -complete=customlist,vinarise#complete VinariseScript2Hex
      \ call s:call_script2hex({'split' : 1}, <q-args>)

if g:vinarise_enable_auto_detect
  augroup vinarise
    autocmd!
    autocmd BufReadPost,FileReadPost *
          \ call s:browse_check(expand('<amatch>'))
  augroup END
endif

function! s:call_vinarise(default, args)"{{{
  let [args, context] = s:parse_args(a:default, a:args)

  call vinarise#start(join(args), context)
endfunction"}}}
function! s:call_script2hex(default, args)"{{{
  let [args, context] = s:parse_args(a:default, a:args)
  if !get(g:, 'loaded_hexript', 0)
    call vinarise#print_error('hexript plugin is needed.')
    return
  elseif &filetype !=# 'hexript' || !filereadable(expand('%'))
    call vinarise#print_error('hexript file is not found.')
    return
  endif

  " Get hexript data.
  let dict = hexript#file_to_dict(expand('%'))
  let context.bytes = dict.bytes

  call vinarise#start(join(args), context)
endfunction"}}}
function! s:parse_args(default, args)"{{{
  let args = []
  let context = a:default
  for arg in split(a:args, '\%(\\\@<!\s\)\+')
    let arg = substitute(arg, '\\\( \)', '\1', 'g')

    let matched_list = filter(copy(vinarise#get_options()),
          \  'stridx(arg, v:val) == 0')
    for option in matched_list
      let key = substitute(substitute(option, '-', '_', 'g'),
            \ '=$', '', '')[1:]
      let context[key] = (option =~ '=$') ?
            \ arg[len(option) :] : 1
      break
    endfor

    if empty(matched_list)
      call add(args, arg)
    endif
  endfor

  return [args, context]
endfunction"}}}

function! s:browse_check(path)"{{{
  let path = vinarise#util#expand(a:path)
  if fnamemodify(path, ':t') ==# '~'
    let path = vinarise#util#expand('~')
  endif

  if (&filetype ==# 'vinarise' && line('$') != 1)
        \ || !filereadable(path)
        \ || !g:vinarise_enable_auto_detect
        \ || path =~# '/.git/index$'
    " Note: vim-fugitive opens ".git/index" binary file when executed
    " ':Gstatus'.
    return
  endif

  let lines = readfile(path, 'b', 1)
  if empty(lines)
    return
  endif

  if lines[0] =~ '\%(^.ELF\|!<arch>\|^MZ\)'
    call vinarise#dump#open(path, 1)
  elseif lines[0] =~ '[\x00-\x09\x10-\x1a\x1c-\x1f]\{5,}'
        \ || (g:vinarise_detect_large_file_size > 0 &&
        \        getfsize(path) > g:vinarise_detect_large_file_size)
    call s:call_vinarise({'overwrite' : 1}, path)
  endif
endfunction"}}}

let &cpo = s:save_cpo
unlet s:save_cpo

let g:loaded_vinarise = 1

" __END__
" vim: foldmethod=marker
