module AST
(
  Identifier(..),
  Program(..),
  Declaration(..),
  TypeMT(..),
  Command(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..),
  TernaryOp(..),
  astDisplay
)
where

import SrcPos
import Data.Char

-- MT AST

data Identifier  = Identifier SrcPos String
data Program     = Program [Declaration] Command
data Declaration = Initialise Identifier TypeMT Expr
data TypeMT      = IntegerMT | BooleanMT

data Command     = Assign Identifier Expr
                 | If Expr Command Command
                 | While Expr Command
                 | GetInt Identifier
                 | PrintInt Expr
                 | Block [Command]

data Expr        = LitInteger Int
                 | LitBoolean Bool
                 | Variable   Identifier
                 | UnaryOp    UnaryOp   Expr
                 | BinaryOp   BinaryOp  Expr Expr
                 | TernaryOp  TernaryOp Expr Expr Expr

data UnaryOp     = IntegerNegation
                 | BooleanNegation

data BinaryOp    = Addition
                 | Subtraction
                 | Multiplication
                 | Division
                 | Conjunction
                 | Disjunction
                 | Equal
                 | NotEqual
                 | Less
                 | Greater
                 | LessEqual
                 | GreaterEqual

data TernaryOp   = Conditional

-- DISPLAYING AST AS A TREE

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

showTypeMT :: TypeMT -> String
showTypeMT IntegerMT = "Integer"
showTypeMT BooleanMT = "Boolean"

astDisplay :: Program -> String
astDisplay p = astDisplayProg id p ""

astDisplayProg :: ShowS -> Program -> ShowS
astDisplayProg p (Program ds c) = node "Program"
                                . nextLine p . node "Declarations" . astDisplayList 16 astDisplayDecl (nextIndent p) ds
                                . lastLine p . astDisplayComm (lastIndent p) c

astDisplayDecl :: ShowS -> Declaration -> ShowS
astDisplayDecl p (Initialise (Identifier _ i) t e) = node "Initialise"
                                                   . nextLine p . leaf i
                                                   . nextLine p . leaf (showTypeMT t)
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
astDisplayExpr _ (LitInteger n)              = node "LitInteger" . sameLine . leaf (show n)
astDisplayExpr _ (LitBoolean b)              = node "LitBoolean" . sameLine . leaf (let c:cs = show b in toLower c : cs)
astDisplayExpr _ (Variable (Identifier _ i)) = node "Variable"   . sameLine . leaf i
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
