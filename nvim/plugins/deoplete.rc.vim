call deoplete#custom#option('omni_patterns', {
			\ 'c': '[^. *\t]\%(\.\|->\)\w*',
			\ 'cpp': '[^. *\t]\%(\.\|->\)\w*\|::\w*',
			\ 'go': '[^.[:digit:] *\t]\.\w*',
			\ 'java': '[^.[:digit:] *\t]\.\w*',
			\ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
			\ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
			\ 'python': '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
			\ 'ruby' : '[^. *\t]\.\w*\|\h\w*::',
			\ 'tex': '\v\\\a*(ref|cite)\a*([^]]*\])?\{([^}]*,)*[^}]*',
			\})

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1]  =~ '\s'
endfunction
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ deoplete#manual_complete()
