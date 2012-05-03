if exists("b:current_syntax")
    finish
endif

syn match lyneConstant "[-\"]"
syn match lyneDirective "^\\\w\+"
syn match lyneInteger "\d\+"
syn match lyneSeparator "|"
syn match lyneStatement "^[&>]"

syn region lyneSubscript start="\[" end="]" transparent contains=lyneInteger

hi def link lyneConstant  Constant
hi def link lyneDirective PreProc
hi def link lyneInteger   Number
hi def link lyneSeparator Operator
hi def link lyneStatement Statement
hi def link lyneSubscript Special

let b:current_syntax = "lyne"
