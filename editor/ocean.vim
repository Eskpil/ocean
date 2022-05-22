" Vim syntax file
" Language: Porth

" Usage Instructions
" Put this file in .vim/syntax/ocean.vim
" and add in your .vimrc file the next line:
" autocmd BufRead,BufNewFile *.ocean set filetype=ocean

if exists("b:current_syntax")
  finish
endif

set iskeyword=a-z,A-Z,-,*,_,!,@
syntax keyword oceanTodos TODO XXX FIXME NOTE

" Language keywords
syntax keyword oceanKeywords let do end 

" Comments
syntax region oceanCommentLine start="//" end="$"   contains=oceanTodos

" String literals
syntax region oceanString start=/\v"/ skip=/\v\\./ end=/\v"/ contains=oceanEscapes

" Char literals
syntax region oceanChar start=/\v'/ skip=/\v\\./ end=/\v'/ contains=oceanEscapes

" Escape literals \n, \r, ....
syntax match oceanEscapes display contained "\\[nr\"']"

" Number literals
syntax region oceanNumber start=/\s\d/ skip=/\d/ end=/\s/

" Type names the compiler recognizes
syntax keyword oceanTypeNames addr int ptr bool
" Set highlights
highlight default link oceanTodos Todo
highlight default link oceanKeywords Keyword
highlight default link oceanCommentLine Comment
highlight default link oceanString String
highlight default link oceanNumber Number
highlight default link oceanTypeNames Type
highlight default link oceanChar Character
highlight default link oceanEscapes SpecialChar

let b:current_syntax = "ocean"

