module Display (astDisplay, cstDisplay) where

import AST

sameLine :: ShowS
sameLine = showString " ── "

nextLine :: ShowS -> ShowS
nextLine p = showChar '\n' . p . showString "├── "

lastLine :: ShowS -> ShowS
lastLine p = showChar '\n' . p . showString "└── "

sameIndent :: Int -> ShowS -> ShowS
sameIndent n p = p . showString (replicate n ' ')

nextIndent :: ShowS -> ShowS
nextIndent p = p . showString "│   "

lastIndent :: ShowS -> ShowS
lastIndent p = p . showString "    "

node :: String -> ShowS
node s = showString "\ESC[1;38;2;255;128;0m" . showString s . showString "\ESC[0m"

leaf :: String -> ShowS
leaf s = showString "\ESC[1;36m" . showString s . showString "\ESC[0m"

showUnOp :: UnaryOp -> String
showUnOp IntegerNegation = "-"
showUnOp BooleanNegation = "!"

showBinOp :: BinaryOp -> String
showBinOp Addition       = "+"
showBinOp Subtraction    = "-"
showBinOp Multiplication = "*"
showBinOp Division       = "/"
showBinOp Conjunction    = "&&"
showBinOp Disjunction    = "||"
showBinOp Equal          = "=="
showBinOp NotEqual       = "!="
showBinOp Less           = "<"
showBinOp LessEqual      = "<="
showBinOp Greater        = ">"
showBinOp GreaterEqual   = ">="

showTernOp :: TernaryOp -> String
showTernOp Conditional = "?:"

disjunctive :: BinaryOp -> Bool
disjunctive Disjunction = True
disjunctive _           = False

conjunctive :: BinaryOp -> Bool
conjunctive Conjunction = True
conjunctive _           = False

relational :: BinaryOp -> Bool
relational Equal        = True
relational NotEqual     = True
relational Less         = True
relational LessEqual    = True
relational Greater      = True
relational GreaterEqual = True
relational _            = False

additive :: BinaryOp -> Bool
additive Addition    = True
additive Subtraction = True
additive _           = False

multiplicative :: BinaryOp -> Bool
multiplicative Multiplication = True
multiplicative Division       = True
multiplicative _              = False

-- DISPLAY AST AS A TREE

astDisplay :: Program -> String
astDisplay p = astDisplayProg id p ""

astDisplayProg :: ShowS -> Program -> ShowS
astDisplayProg p (Program ds c) = node "Program"
                                . nextLine p . node "Declarations" . astDisplayList 16 astDisplayDecl (nextIndent p) ds
                                . lastLine p . astDisplayComm (lastIndent p) c

astDisplayDecl :: ShowS -> Declaration -> ShowS
astDisplayDecl p (Initialise (Identifier _ i) e) = node "Initialise"
                                                 . nextLine p . leaf i
                                                 . lastLine p . astDisplayExpr (lastIndent p) e

astDisplayComm :: ShowS -> Command -> ShowS
astDisplayComm p (Assign (Identifier _ i) e) = node "Assign"
                                             . nextLine p . leaf i
                                             . lastLine p . astDisplayExpr (lastIndent p) e
astDisplayComm p (If e t f)                  = node "If"
                                             . nextLine p . astDisplayExpr (nextIndent p) e
                                             . nextLine p . astDisplayComm (nextIndent p) t
                                             . lastLine p . astDisplayComm (lastIndent p) f
astDisplayComm p (While e c)                 = node "While"
                                             . nextLine p . astDisplayExpr (nextIndent p) e
                                             . lastLine p . astDisplayComm (lastIndent p) c
astDisplayComm _ (GetInt (Identifier _ i))   = node "GetInt"   . sameLine . leaf i
astDisplayComm p (PrintInt e)                = node "PrintInt" . sameLine . astDisplayExpr (sameIndent 12 p) e
astDisplayComm p (Block cs)                  = node "Block" . astDisplayList 9 astDisplayComm p cs

astDisplayExpr :: ShowS -> Expr -> ShowS
astDisplayExpr _ (Literal n)                 = node "Literal"  . sameLine . leaf (show n)
astDisplayExpr _ (Variable (Identifier _ i)) = node "Variable" . sameLine . leaf i
astDisplayExpr p (UnaryOp op x)              = node "UnaryOp"
                                             . nextLine p . leaf (showUnOp op)
                                             . lastLine p . astDisplayExpr (lastIndent p) x
astDisplayExpr p (BinaryOp op x y)           = node "BinaryOp"
                                             . nextLine p . leaf (showBinOp op)
                                             . nextLine p . astDisplayExpr (nextIndent p) x
                                             . lastLine p . astDisplayExpr (lastIndent p) y
astDisplayExpr p (TernaryOp op x y z)        = node "TernaryOp"
                                             . nextLine p . leaf (showTernOp op)
                                             . nextLine p . astDisplayExpr (nextIndent p) x
                                             . nextLine p . astDisplayExpr (nextIndent p) y
                                             . lastLine p . astDisplayExpr (lastIndent p) z

astDisplayList :: Int -> (ShowS -> a -> ShowS) -> ShowS -> [a] -> ShowS
astDisplayList i f p [x] = sameLine . f (sameIndent i p) x
astDisplayList _ f p xs  = go xs
  where
    go []     = id
    go [y]    = lastLine p . f (lastIndent p) y
    go (y:ys) = nextLine p . f (nextIndent p) y . go ys

-- DISPLAY CST AS A TREE

cstDisplay :: Program -> String
cstDisplay p = cstDisplayProg id p ""

cstDisplayProg :: ShowS -> Program -> ShowS
cstDisplayProg p (Program ds c) = node "PROGRAM"
                                . nextLine p . leaf "let"
                                . nextLine p . cstDisplayDecls (nextIndent p) ds
                                . nextLine p . leaf "in"
                                . lastLine p . cstDisplayComm (lastIndent p) c

cstDisplayDecls :: ShowS -> [Declaration] -> ShowS
cstDisplayDecls p ds = node "DECLARATIONS"
                     . case ds of
                         []     -> sameLine   . leaf "ε"
                         [x]    -> sameLine   . cstDisplayDecl (sameIndent 16 p) x
                         (x:xs) -> nextLine p . cstDisplayDecl (nextIndent p) x
                                 . nextLine p . leaf ";"
                                 . lastLine p . cstDisplayDecls (lastIndent p) xs

cstDisplayDecl :: ShowS -> Declaration -> ShowS
cstDisplayDecl p (Initialise (Identifier _ i) e) = node "DECLARATION"
                                                 . nextLine p . leaf "var"
                                                 . case e of
                                                     Literal 0 -> lastLine p . cstDisplayIdent (lastIndent p) i
                                                     _         -> nextLine p . cstDisplayIdent (nextIndent p) i
                                                                . nextLine p . leaf ":="
                                                                . lastLine p . cstDisplayExpr (lastIndent p) e

cstDisplayComms :: ShowS -> [Command] -> ShowS
cstDisplayComms p cs = node "COMMANDS"
                     . case cs of
                         []     -> sameLine   . leaf "ε"
                         [x]    -> sameLine   . cstDisplayComm (sameIndent 12 p) x
                         (x:xs) -> nextLine p . cstDisplayComm (nextIndent p) x
                                 . nextLine p . leaf ";"
                                 . lastLine p . cstDisplayComms (lastIndent p) xs

cstDisplayComm :: ShowS -> Command -> ShowS
cstDisplayComm p c = node "COMMAND"
                   . case c of
                       Assign (Identifier _ i) e -> nextLine p . cstDisplayIdent (nextIndent p) i
                                                  . nextLine p . leaf ":="
                                                  . lastLine p . cstDisplayExpr (lastIndent p) e
                       If e t f                  -> nextLine p . leaf "if"
                                                  . nextLine p . cstDisplayExpr (nextIndent p) e
                                                  . nextLine p . leaf "then"
                                                  . nextLine p . cstDisplayComm (nextIndent p) t
                                                  . nextLine p . leaf "else"
                                                  . lastLine p . cstDisplayComm (lastIndent p) f
                       While e b                 -> nextLine p . leaf "while"
                                                  . nextLine p . cstDisplayExpr (nextIndent p) e
                                                  . nextLine p . leaf "do"
                                                  . lastLine p . cstDisplayComm (lastIndent p) b
                       GetInt (Identifier _ i)   -> nextLine p . leaf "getint"
                                                  . nextLine p . leaf "("
                                                  . nextLine p . cstDisplayIdent (nextIndent p) i
                                                  . lastLine p . leaf ")"
                       PrintInt e                -> nextLine p . leaf "printint"
                                                  . nextLine p . leaf "("
                                                  . nextLine p . cstDisplayExpr (nextIndent p) e
                                                  . lastLine p . leaf ")"
                       Block cs                  -> nextLine p . leaf "begin"
                                                  . nextLine p . cstDisplayComms (nextIndent p) cs
                                                  . lastLine p . leaf "end"

cstDisplayIdent :: ShowS -> String -> ShowS
cstDisplayIdent _ i = node "IDENTIFIER" . sameLine . leaf i

cstDisplayExpr :: ShowS -> Expr -> ShowS
cstDisplayExpr = cstDisplayExprCond

cstDisplayExprCond :: ShowS -> Expr -> ShowS
cstDisplayExprCond p e = node "EXPR_COND"
                       . case e of
                           TernaryOp Conditional x y z -> nextLine p . cstDisplayExprDisj (nextIndent p) x
                                                        . nextLine p . leaf "?"
                                                        . nextLine p . cstDisplayExprDisj (nextIndent p) y
                                                        . nextLine p . leaf ":"
                                                        . lastLine p . cstDisplayExprDisj (lastIndent p) z
                           _                           -> sameLine   . cstDisplayExprDisj (sameIndent 13 p) e

cstDisplayExprDisj :: ShowS -> Expr -> ShowS
cstDisplayExprDisj p e = node "EXPR_DISJ"
                       . case e of
                           BinaryOp op x y | disjunctive op
                                          -> nextLine p . cstDisplayExprDisj (nextIndent p) x
                                           . nextLine p . leaf (showBinOp op)
                                           . lastLine p . cstDisplayExprConj (lastIndent p) y
                           _              -> sameLine   . cstDisplayExprConj (sameIndent 13 p) e

cstDisplayExprConj :: ShowS -> Expr -> ShowS
cstDisplayExprConj p e = node "EXPR_CONJ"
                       . case e of
                           BinaryOp op x y | conjunctive op
                                          -> nextLine p . cstDisplayExprConj (nextIndent p) x
                                           . nextLine p . leaf (showBinOp op)
                                           . lastLine p . cstDisplayExprRel  (lastIndent p) y
                           _              -> sameLine   . cstDisplayExprRel  (sameIndent 13 p) e

cstDisplayExprRel :: ShowS -> Expr -> ShowS
cstDisplayExprRel p e = node "EXPR_REL"
                      . case e of
                          BinaryOp op x y | relational op
                                         -> nextLine p . cstDisplayExprAdd (nextIndent p) x
                                          . nextLine p . leaf (showBinOp op)
                                          . lastLine p . cstDisplayExprAdd  (lastIndent p) y
                          _              -> sameLine   . cstDisplayExprAdd  (sameIndent 12 p) e

cstDisplayExprAdd :: ShowS -> Expr -> ShowS
cstDisplayExprAdd p e = node "EXPR_ADD"
                      . case e of
                          BinaryOp op x y | additive op
                                         -> nextLine p . cstDisplayExprAdd (nextIndent p) x
                                          . nextLine p . leaf (showBinOp op)
                                          . lastLine p . cstDisplayExprMul (lastIndent p) y
                          _              -> sameLine   . cstDisplayExprMul (sameIndent 12 p) e

cstDisplayExprMul :: ShowS -> Expr -> ShowS
cstDisplayExprMul p e = node "EXPR_MUL"
                      . case e of
                          BinaryOp op x y | multiplicative op
                                         -> nextLine p . cstDisplayExprMul (nextIndent p) x
                                          . nextLine p . leaf (showBinOp op)
                                          . lastLine p . cstDisplayExprAtom (lastIndent p) y
                          _              -> sameLine   . cstDisplayExprAtom (sameIndent 12 p) e

cstDisplayExprAtom :: ShowS -> Expr -> ShowS
cstDisplayExprAtom p e = node "EXPR_ATOM"
                       . case e of
                           Literal n                 -> sameLine   . node "NATURAL" . sameLine . leaf (show n)
                           Variable (Identifier _ i) -> sameLine   . cstDisplayIdent (lastIndent p) i
                           UnaryOp op x              -> nextLine p . leaf (showUnOp op)
                                                      . lastLine p . cstDisplayExprAtom (lastIndent p) x
                           _                         -> nextLine p . leaf "("
                                                      . nextLine p . cstDisplayExpr (nextIndent p) e
                                                      . lastLine p . leaf ")"
