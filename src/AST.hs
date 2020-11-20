module AST
(
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

import Data.Char

-- MT AST

data Program     = Program [Declaration] Command
data Declaration = Initialise String TypeMT Expr
data TypeMT      = IntegerMT | BooleanMT deriving Eq

data Command     = Assign String Expr
                 | If Expr Command Command
                 | While Expr Command
                 | GetInt String
                 | PrintInt Expr
                 | Block [Command]

data Expr        = LitInteger Int
                 | LitBoolean Bool
                 | Variable   String
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

instance Show UnaryOp where
  show IntegerNegation = "-"
  show BooleanNegation = "!"

instance Show BinaryOp where
  show Addition       = "+"
  show Subtraction    = "-"
  show Multiplication = "*"
  show Division       = "/"
  show Conjunction    = "&&"
  show Disjunction    = "||"
  show Equal          = "=="
  show NotEqual       = "!="
  show Less           = "<"
  show LessEqual      = "<="
  show Greater        = ">"
  show GreaterEqual   = ">="

instance Show TernaryOp where
  show Conditional = "?:"

instance Show TypeMT where
  show IntegerMT = "Integer"
  show BooleanMT = "Boolean"

astDisplay :: Program -> String
astDisplay p = astDisplayProg id p ""

astDisplayProg :: ShowS -> Program -> ShowS
astDisplayProg p (Program ds c) = node "Program"
                                . nextLine p . node "Declarations" . astDisplayList 16 astDisplayDecl (nextIndent p) ds
                                . lastLine p . astDisplayComm (lastIndent p) c

astDisplayDecl :: ShowS -> Declaration -> ShowS
astDisplayDecl p (Initialise v t e) = node "Initialise"
                                    . nextLine p . leaf v
                                    . nextLine p . leaf (show t)
                                    . lastLine p . astDisplayExpr (lastIndent p) e

astDisplayComm :: ShowS -> Command -> ShowS
astDisplayComm p (Assign v e) = node "Assign"
                              . nextLine p . leaf v
                              . lastLine p . astDisplayExpr (lastIndent p) e
astDisplayComm p (If e t f)   = node "If"
                              . nextLine p . astDisplayExpr (nextIndent p) e
                              . nextLine p . astDisplayComm (nextIndent p) t
                              . lastLine p . astDisplayComm (lastIndent p) f
astDisplayComm p (While e c)  = node "While"
                              . nextLine p . astDisplayExpr (nextIndent p) e
                              . lastLine p . astDisplayComm (lastIndent p) c
astDisplayComm _ (GetInt v)   = node "GetInt"   . sameLine . leaf v
astDisplayComm p (PrintInt e) = node "PrintInt" . sameLine . astDisplayExpr (sameIndent 12 p) e
astDisplayComm p (Block cs)   = node "Block"    . astDisplayList 9 astDisplayComm p cs

astDisplayExpr :: ShowS -> Expr -> ShowS
astDisplayExpr _ (LitInteger n)       = node "LitInteger" . sameLine . leaf (show n)
astDisplayExpr _ (LitBoolean b)       = node "LitBoolean" . sameLine . leaf (let c:cs = show b in toLower c : cs)
astDisplayExpr _ (Variable v)         = node "Variable"   . sameLine . leaf v
astDisplayExpr p (UnaryOp op x)       = node "UnaryOp"
                                      . nextLine p . leaf (show op)
                                      . lastLine p . astDisplayExpr (lastIndent p) x
astDisplayExpr p (BinaryOp op x y)    = node "BinaryOp"
                                      . nextLine p . leaf (show op)
                                      . nextLine p . astDisplayExpr (nextIndent p) x
                                      . lastLine p . astDisplayExpr (lastIndent p) y
astDisplayExpr p (TernaryOp op x y z) = node "TernaryOp"
                                      . nextLine p . leaf (show op)
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
