" unite.vim {{{
if neobundle#tap('unite.vim')
  nnoremap [unite] <Nop>
  nmap <Leader>u [unite]
  nnoremap [unite]n :Unite<Space>
  nnoremap [unite]u :UniteWithBufferDir<Space>
  nnoremap [unite]gg :UniteWithBufferDir grep<CR>
  nnoremap [unite]gn :Unite grep:
  nnoremap [unite]b :Unite buffer<CR>
  nnoremap [unite]c :Unite command<CR>
  nnoremap [unite]f :Unite file_rec/async<CR>
  nnoremap [unite]l :Unite file/async<CR>
  nnoremap [unite]m :Unite neomru/file -buffer-name=mru -create<CR>
  nnoremap [unite]q :Unite quickfix -horizontal -no-quit<CR>
  nnoremap [unite]r :Unite register -buffer-name=register<CR>
  nnoremap [unite]t :Unite tab:no-current<CR>
  nnoremap [unite]y :Unite history/yank<CR>

  " NeoBundleUpdate using unite.vim interface
  command! NeoBundleUpdateUnite :Unite neobundle/update

  if neobundle#is_installed('unite-outline')
    nnoremap [unite]o :Unite outline<CR>
  endif

  function! neobundle#hooks.on_source(bundle)
    call unite#custom#profile('default',
          \ 'context', {
          \   'start_insert': 1,
          \   'direction': 'botright',
          \   'prompt_direction': 'top',
          \   'auto_resize': 1,
          \ })

    call unite#custom#profile('source/file/async,source/file_rec/async',
          \ 'context', {
          \   'profile_name': 'files',
          \   'buffer_name': 'file',
          \   'create': 1,
          \ })

    " Case-insensitive command search
    call unite#custom#profile('source/command',
          \ 'context', {
          \   'smartcase': 1,
          \ })

    call unite#custom#profile('source/boost-online-doc',
          \ 'context', {
          \   'smartcase': 1,
          \ })

    call unite#custom#source('file_rec/async',
          \ 'matchers', [
          \   'matcher_hide_hidden_files',
          \   'matcher_hide_hidden_directories',
          \   'converter_relative_word',
          \   ])

    call unite#custom#source('command', 'matchers', 'matcher_fuzzy')

    let g:unite_source_alias_aliases = {
          \ 'fra': {
          \   'source': 'file_rec/async',
          \ },
          \ }

    " Use ag if available
    if executable('ag')
      let g:unite_source_grep_command = 'ag'
      let g:unite_source_grep_default_opts = '--nocolor --nogroup --line-numbers '
            \ . '--ignore ".git" --ignore ".svn" --ignore ".hg"'
    endif

    autocmd FileType unite imap <silent> <buffer> <C-w> <Plug>(unite_delete_backward_path)
    " helm-like preview
    autocmd FileType unite imap <silent> <buffer> <C-z> <Esc><Plug>(unite_smart_preview)i
  endfunction

  call neobundle#untap()
endif
" }}}
