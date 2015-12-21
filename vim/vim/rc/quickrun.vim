" vim-quickrun {{{
if neobundle#tap('vim-quickrun')
  nmap <silent> <Leader>r <Plug>(quickrun)
  nmap <silent> <Leader>m :QuickRun make<CR>

  function! neobundle#hooks.on_source(bundle)
    nnoremap <Leader>wb :QuickRun -runner wandbox<CR>

    let g:quickrun_config = {
          \ '_': {
          \   'hook/time/enable': 0,
          \   'hook/close_unite_quickfix/enable_hook_loaded': 1,
          \   'hook/unite_quickfix/enable_failure': 1,
          \   'hook/close_quickfix/enable_exit': 1,
          \   'hook/close_buffer/enable_empty_data': 1,
          \   'hook/close_buffer/enable_failure': 1,
          \   'outputter': 'multi:buffer:quickfix',
          \   'outputter/buffer/close_on_empty': 1,
          \   'hook/quickfix_replace_tempname_to_bufnr/enable_exit': 1,
          \   'hook/quickfix_replace_tempname_to_bufnr/priority_exit': -10,
          \   'runmode': 'async:remote:vimproc',
          \   'runner': 'vimproc',
          \   'runner/vimproc/updatetime': 100,
          \ },
          \ 'make': {
          \   'command': 'make',
          \   'exec': "%c %o",
          \   'runner': 'vimproc',
          \ },
          \ }
    let g:quickrun_config.cpp = {
          \ 'command': 'g++',
          \ 'cmdopt': '-std=c++11 -Wall -Wextra',
          \ }
    " Open in browser using previm
    let g:quickrun_config.markdown = {
          \ 'runner': 'vimscript',
          \ 'exec': 'PrevimOpen',
          \ 'outputter': 'null',
          \ 'hook/time/enable': 0,
          \ }
  endfunction

  call neobundle#untap()
endif
" }}}
