let s:save_cpo = &cpo
set cpo&vim

let s:utils = V.import('Web.Utils')

function! s:decode(json)
  let json = iconv(a:json, "utf-8", &encoding)
  let json = substitute(json, '\n', '', 'g')
  let json = substitute(json, '\\u34;', '\\"', 'g')
  let json = substitute(json, '\\u\(\x\x\x\x\)', '\=s:utils.nr2enc_char("0x".submatch(1))', 'g')
  let [null,true,false] = [0,1,0]
  sandbox let ret = eval(json)
  return ret
endfunction

function! s:encode(obj)
  if type(a:val) == 0
    return a:val
  elseif type(a:val) == 1
    let json = '"' . escape(a:val, '"') . '"'
    let json = substitute(json, "\r", '\\r', 'g')
    let json = substitute(json, "\n", '\\n', 'g')
    let json = substitute(json, "\t", '\\t', 'g')
    return iconv(json, &encoding, "utf-8")
  elseif type(a:val) == 3
    return '[' . join(map(copy(a:val), 's:encode(v:val)'), ',') . ']'
  elseif type(a:val) == 4
    return '{' . join(map(keys(a:val), 's:encode(v:val).":".s:encode(a:val[v:val])'), ',') . '}'
  else
    return string(a:val)
  endif
endfunction

let &cpo = s:save_cpo
