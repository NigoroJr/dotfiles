call deoplete#custom#option('omni_patterns', {
      \ 'python': '\%(^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
      \})

" call deoplete#custom#option('omni_patterns', {
" 			\ 'c': '[^. *\t]\%(\.\|->\)\w*',
" 			\ 'cpp': '[^. *\t]\%(\.\|->\)\w*\|::\w*',
" 			\ 'go': '[^.[:digit:] *\t]\.\w*',
" 			\ 'java': '[^.[:digit:] *\t]\.\w*',
" 			\ 'javascript': '[^.[:digit:] *\t]\.\w*\|\<require(',
" 			\ 'perl': '[^. \t]->\%(\h\w*\)\?\|use.*\w*::\%(\h\w*\)\?',
" 			\ 'python': '\%(^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*',
" 			\ 'ruby' : '[^. *\t]\.\w*\|\h\w*::',
" 			\ 'tex': '\v\\\a*(ref|cite)\a*([^]]*\])?\{([^}]*,)*[^}]*',
" 			\})
