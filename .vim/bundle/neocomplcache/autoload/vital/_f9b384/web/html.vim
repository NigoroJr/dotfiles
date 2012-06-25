let s:save_cpo = &cpo
set cpo&vim

let s:utils = V.import('Web.Utils')
let s:xml = V.import('Web.Xml')
let s:http = V.import('Web.Http')

function! s:decodeEntityReference(str)
  let str = a:str
  let str = substitute(str, '&gt;', '>', 'g')
  let str = substitute(str, '&lt;', '<', 'g')
  let str = substitute(str, '&quot;', '"', 'g')
  let str = substitute(str, '&apos;', "'", 'g')
  let str = substitute(str, '&nbsp;', ' ', 'g')
  let str = substitute(str, '&yen;', '\&#65509;', 'g')
  let str = substitute(str, '&#\(\d\+\);', '\=s:utils.nr2enc_char(submatch(1))', 'g')
  let str = substitute(str, '&amp;', '\&', 'g')
  let str = substitute(str, '&raquo;', '>', 'g')
  let str = substitute(str, '&laquo;', '<', 'g')
  return str
endfunction

function! s:encodeEntityReference(str)
  let str = a:str
  let str = substitute(str, '&', '\&amp;', 'g')
  let str = substitute(str, '>', '\&gt;', 'g')
  let str = substitute(str, '<', '\&lt;', 'g')
  let str = substitute(str, "\n", '\&#x0d;', 'g')
  let str = substitute(str, '"', '\&quot;', 'g')
  let str = substitute(str, "'", '\&apos;', 'g')
  let str = substitute(str, ' ', '\&nbsp;', 'g')
  return str
endfunction

function! s:parse(content)
  let content = substitute(a:content, '<\(area\|base\|basefont\|br\|nobr\|col\|frame\|hr\|img\|input\|isindex\|link\|meta\|param\|embed\|keygen\|command\)\([^>]*[^/]\|\)>', '<\1\2/>', 'g')
  return s:xml.parse(content)
endfunction

function! s:parseFile(fname)
  return s:parse(join(readfile(a:fname), "\n"))
endfunction

function! s:parseURL(url)
  return s:parse(s:http.get(a:url).content)
endfunction

let &cpo = s:save_cpo
