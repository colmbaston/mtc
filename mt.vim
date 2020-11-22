if exists("b:current_syntax")
  finish
endif

syntax keyword mtKeyword let in var fun if then else while do begin end
syntax keyword mtType    Integer Boolean

syntax match mtOp "+"
syntax match mtOp "-"
syntax match mtOp "*"
syntax match mtOp "/"
syntax match mtOp "!"
syntax match mtOp ":"
syntax match mtOp "?"
syntax match mtOp "<"
syntax match mtOp ">"
syntax match mtOp "="
syntax match mtOp "&&"
syntax match mtOp "||"
syntax match mtOp "=="
syntax match mtOp "!="
syntax match mtOp "<="
syntax match mtOp ">="
syntax match mtOp ":="

syntax match   mtVar  "\v\a(\a|\d)*"
syntax match   mtFun  "\v\a(\a|\d)*\s*\("he=e-1

syntax match   mtInt  "\v\d+"
syntax keyword mtBool false true

highlight link mtKeyword Keyword
highlight link mtType    Type
highlight link mtOp      Operator
highlight link mtVar     Variable
highlight link mtFun     Function
highlight link mtProc    Function
highlight link mtInt     Number
highlight link mtBool    Boolean

let b:current_syntax = "mt"
