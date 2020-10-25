module TreePrint (astPrint, cstPrint) where

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

-- PRINT AST AS A TREE

astPrint :: Program -> IO ()
astPrint p = putStrLn (astPrintProg id p "")

astPrintProg :: ShowS -> Program -> ShowS
astPrintProg p (Program ds c) = node "Program"
                              . nextLine p . node "Declarations" . astPrintList 16 astPrintDecl (nextIndent p) ds
                              . lastLine p . astPrintComm (lastIndent p) c

astPrintDecl :: ShowS -> Declaration -> ShowS
astPrintDecl p (Initialise i e) = node "Initialise"
                                . nextLine p . leaf i
                                . lastLine p . astPrintExpr (lastIndent p) e

astPrintComm :: ShowS -> Command -> ShowS
astPrintComm p (Assign i e) = node "Assign"
                            . nextLine p . leaf i
                            . lastLine p . astPrintExpr (lastIndent p) e
astPrintComm p (If e t f)   = node "If"
                            . nextLine p . astPrintExpr (nextIndent p) e
                            . nextLine p . astPrintComm (nextIndent p) t
                            . lastLine p . astPrintComm (lastIndent p) f
astPrintComm p (While e c)  = node "While"
                            . nextLine p . astPrintExpr (nextIndent p) e
                            . lastLine p . astPrintComm (lastIndent p) c
astPrintComm _ (GetInt i)   = node "GetInt"   . sameLine . leaf i
astPrintComm p (PrintInt e) = node "PrintInt" . sameLine . astPrintExpr (sameIndent 12 p) e
astPrintComm p (Block cs)   = node "Block" . astPrintList 9 astPrintComm p cs

astPrintExpr :: ShowS -> Expr -> ShowS
astPrintExpr _ (Literal n)          = node "Literal"  . sameLine . leaf (show n)
astPrintExpr _ (Variable i)         = node "Variable" . sameLine . leaf i
astPrintExpr p (UnaryOp op x)       = node "UnaryOp"
                                    . nextLine p . leaf (showUnOp op)
                                    . lastLine p . astPrintExpr (lastIndent p) x
astPrintExpr p (BinaryOp op x y)    = node "BinaryOp"
                                    . nextLine p . leaf (showBinOp op)
                                    . nextLine p . astPrintExpr (nextIndent p) x
                                    . lastLine p . astPrintExpr (lastIndent p) y
astPrintExpr p (TernaryOp op x y z) = node "TernaryOp"
                                    . nextLine p . leaf (showTernOp op)
                                    . nextLine p . astPrintExpr (nextIndent p) x
                                    . nextLine p . astPrintExpr (nextIndent p) y
                                    . lastLine p . astPrintExpr (lastIndent p) z

astPrintList :: Int -> (ShowS -> a -> ShowS) -> ShowS -> [a] -> ShowS
astPrintList i f p [x] = sameLine . f (sameIndent i p) x
astPrintList _ f p xs  = go xs
  where
    go []     = id
    go [y]    = lastLine p . f (lastIndent p) y
    go (y:ys) = nextLine p . f (nextIndent p) y . go ys

-- PRINT CST AS A TREE

cstPrint :: Program -> IO ()
cstPrint p = putStrLn (cstPrintProg id p "")

cstPrintProg :: ShowS -> Program -> ShowS
cstPrintProg p (Program ds c) = node "PROGRAM"
                              . nextLine p . leaf "let"
                              . nextLine p . cstPrintDecls (nextIndent p) ds
                              . nextLine p . leaf "in"
                              . lastLine p . cstPrintComm (lastIndent p) c

cstPrintDecls :: ShowS -> [Declaration] -> ShowS
cstPrintDecls p ds = node "DECLARATIONS"
                   . case ds of
                       []     -> sameLine   . leaf "ε"
                       [x]    -> sameLine   . cstPrintDecl (sameIndent 16 p) x
                       (x:xs) -> nextLine p . cstPrintDecl (nextIndent p) x
                               . nextLine p . leaf ";"
                               . lastLine p . cstPrintDecls (lastIndent p) xs

cstPrintDecl :: ShowS -> Declaration -> ShowS
cstPrintDecl p (Initialise i e) = node "DECLARATION"
                                 . nextLine p . leaf "var"
                                 . case e of
                                     Literal 0 -> lastLine p . cstPrintIdent (lastIndent p) i
                                     _         -> nextLine p . cstPrintIdent (nextIndent p) i
                                                . nextLine p . leaf ":="
                                                . lastLine p . cstPrintExpr (lastIndent p) e

cstPrintComms :: ShowS -> [Command] -> ShowS
cstPrintComms p cs = node "COMMANDS"
                   . case cs of
                       []     -> sameLine   . leaf "ε"
                       [x]    -> sameLine   . cstPrintComm (sameIndent 12 p) x
                       (x:xs) -> nextLine p . cstPrintComm (nextIndent p) x
                               . nextLine p . leaf ";"
                               . lastLine p . cstPrintComms (lastIndent p) xs

cstPrintComm :: ShowS -> Command -> ShowS
cstPrintComm p c = node "COMMAND"
                 . case c of
                     Assign i e -> nextLine p . cstPrintIdent (nextIndent p) i
                                 . nextLine p . leaf ":="
                                 . lastLine p . cstPrintExpr (lastIndent p) e
                     If e t f   -> nextLine p . leaf "if"
                                 . nextLine p . cstPrintExpr (nextIndent p) e
                                 . nextLine p . leaf "then"
                                 . nextLine p . cstPrintComm (nextIndent p) t
                                 . nextLine p . leaf "else"
                                 . lastLine p . cstPrintComm (lastIndent p) f
                     While e b  -> nextLine p . leaf "while"
                                 . nextLine p . cstPrintExpr (nextIndent p) e
                                 . nextLine p . leaf "do"
                                 . lastLine p . cstPrintComm (lastIndent p) b
                     GetInt i   -> nextLine p . leaf "getint"
                                 . nextLine p . leaf "("
                                 . nextLine p . cstPrintIdent (nextIndent p) i
                                 . lastLine p . leaf ")"
                     PrintInt e -> nextLine p . leaf "printint"
                                 . nextLine p . leaf "("
                                 . nextLine p . cstPrintExpr (nextIndent p) e
                                 . lastLine p . leaf ")"
                     Block cs   -> nextLine p . leaf "begin"
                                 . nextLine p . cstPrintComms (nextIndent p) cs
                                 . lastLine p . leaf "end"

cstPrintIdent :: ShowS -> String -> ShowS
cstPrintIdent _ i = node "IDENTIFIER" . sameLine . leaf i

cstPrintExpr :: ShowS -> Expr -> ShowS
cstPrintExpr = cstPrintExprCond

cstPrintExprCond :: ShowS -> Expr -> ShowS
cstPrintExprCond p e = node "EXPR_COND"
                      . case e of
                          TernaryOp Conditional x y z -> nextLine p . cstPrintExprDisj (nextIndent p) x
                                                       . nextLine p . leaf "?"
                                                       . nextLine p . cstPrintExprDisj (nextIndent p) y
                                                       . nextLine p . leaf ":"
                                                       . lastLine p . cstPrintExprDisj (lastIndent p) z
                          _                           -> sameLine   . cstPrintExprDisj (sameIndent 13 p) e

cstPrintExprDisj :: ShowS -> Expr -> ShowS
cstPrintExprDisj p e = node "EXPR_DISJ"
                      . case e of
                          BinaryOp op x y | op `elem` [Disjunction]
                                         -> nextLine p . cstPrintExprDisj (nextIndent p) x
                                          . nextLine p . leaf (showBinOp op)
                                          . lastLine p . cstPrintExprConj (lastIndent p) y
                          _              -> sameLine   . cstPrintExprConj (sameIndent 13 p) e

cstPrintExprConj :: ShowS -> Expr -> ShowS
cstPrintExprConj p e = node "EXPR_CONJ"
                      . case e of
                          BinaryOp op x y | op `elem` [Conjunction]
                                         -> nextLine p . cstPrintExprConj (nextIndent p) x
                                          . nextLine p . leaf (showBinOp op)
                                          . lastLine p . cstPrintExprRel  (lastIndent p) y
                          _              -> sameLine   . cstPrintExprRel  (sameIndent 13 p) e

cstPrintExprRel :: ShowS -> Expr -> ShowS
cstPrintExprRel p e = node "EXPR_REL"
                     . case e of
                         BinaryOp op x y | op `elem` [Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual]
                                       -> nextLine p . cstPrintExprAdd (nextIndent p) x
                                        . nextLine p . leaf (showBinOp op)
                                        . lastLine p . cstPrintExprAdd  (lastIndent p) y
                         _             -> sameLine   . cstPrintExprAdd  (sameIndent 12 p) e

cstPrintExprAdd :: ShowS -> Expr -> ShowS
cstPrintExprAdd p e = node "EXPR_ADD"
                     . case e of
                         BinaryOp op x y | op `elem` [Addition, Subtraction]
                                        -> nextLine p . cstPrintExprAdd (nextIndent p) x
                                         . nextLine p . leaf (showBinOp op)
                                         . lastLine p . cstPrintExprMul (lastIndent p) y
                         _              -> sameLine   . cstPrintExprMul (sameIndent 12 p) e

cstPrintExprMul :: ShowS -> Expr -> ShowS
cstPrintExprMul p e = node "EXPR_MUL"
                     . case e of
                         BinaryOp op x y | op `elem` [Multiplication, Division]
                                        -> nextLine p . cstPrintExprMul (nextIndent p) x
                                         . nextLine p . leaf (showBinOp op)
                                         . lastLine p . cstPrintExprAtom (lastIndent p) y
                         _              -> sameLine   . cstPrintExprAtom (sameIndent 12 p) e

cstPrintExprAtom :: ShowS -> Expr -> ShowS
cstPrintExprAtom p e = node "EXPR_ATOM"
                      . case e of
                          Literal n    -> sameLine   . node "NATURAL" . sameLine . leaf (show n)
                          Variable i   -> sameLine   . cstPrintIdent (lastIndent p) i
                          UnaryOp op x -> nextLine p . leaf (showUnOp op)
                                        . lastLine p . cstPrintExprAtom (lastIndent p) x
                          _            -> nextLine p . leaf "("
                                        . nextLine p . cstPrintExpr (nextIndent p) e
                                        . lastLine p . leaf ")"
