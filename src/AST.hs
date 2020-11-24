module AST
(
  Program(..),
  Declaration(..),
  Param(..),
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

data Program     = Program [Declaration] Command
data Declaration = Initialise SrcPos String TypeMT Expr
                 | Function   SrcPos String [Param] TypeMT Expr
data Param       = Param SrcPos String TypeMT
data TypeMT      = IntegerMT | BooleanMT deriving Eq

data Command     = Assign SrcPos String Expr
                 | If Expr Command Command
                 | While Expr Command
                 | GetInt SrcPos String
                 | PrintInt Expr
                 | Block [Command]

data Expr        = LitInteger  SrcPos Int
                 | LitBoolean  SrcPos Bool
                 | Variable    SrcPos String
                 | Application SrcPos String   [Expr]
                 | UnaryOp     SrcPos UnaryOp   Expr
                 | BinaryOp    SrcPos BinaryOp  Expr Expr
                 | TernaryOp   SrcPos TernaryOp Expr Expr Expr

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

sameBranch :: ShowS
sameBranch = showString " ── "

nextBranch :: ShowS -> ShowS
nextBranch p = showChar '\n' . p . showString "├── "

lastBranch :: ShowS -> ShowS
lastBranch p = showChar '\n' . p . showString "└── "

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
                                . nextBranch p . node "Declarations" . astDisplayList 16 astDisplayDecl (nextIndent p) ds
                                . lastBranch p . astDisplayComm (lastIndent p) c

astDisplayDecl :: ShowS -> Declaration -> ShowS
astDisplayDecl p (Initialise _ v t e)  = node "Initialise"
                                       . nextBranch p . leaf v
                                       . nextBranch p . leaf (show t)
                                       . lastBranch p . astDisplayExpr (lastIndent p) e
astDisplayDecl p (Function _ f ps t e) = node "Function"
                                       . nextBranch p . leaf f
                                       . nextBranch p . node "Parameters" . astDisplayList 14 astDisplayParam (nextIndent p) ps
                                       . nextBranch p . leaf (show t)
                                       . lastBranch p . astDisplayExpr (lastIndent p) e

astDisplayParam :: ShowS -> Param -> ShowS
astDisplayParam p (Param _ v t) = node "Parameter"
                                . nextBranch p . leaf v
                                . lastBranch p . leaf (show t)

astDisplayComm :: ShowS -> Command -> ShowS
astDisplayComm p (Assign _ v e) = node "Assign"
                                . nextBranch p . leaf v
                                . lastBranch p . astDisplayExpr (lastIndent p) e
astDisplayComm p (If e t f)     = node "If"
                                . nextBranch p . astDisplayExpr (nextIndent p) e
                                . nextBranch p . astDisplayComm (nextIndent p) t
                                . lastBranch p . astDisplayComm (lastIndent p) f
astDisplayComm p (While e c)    = node "While"
                                . nextBranch p . astDisplayExpr (nextIndent p) e
                                . lastBranch p . astDisplayComm (lastIndent p) c
astDisplayComm _ (GetInt _ v)   = node "GetInt"   . sameBranch . leaf v
astDisplayComm p (PrintInt e)   = node "PrintInt" . sameBranch . astDisplayExpr (sameIndent 12 p) e
astDisplayComm p (Block cs)     = node "Block"    . astDisplayList 9 astDisplayComm p cs

astDisplayExpr :: ShowS -> Expr -> ShowS
astDisplayExpr _ (LitInteger _ n)         = node "LitInteger" . sameBranch . leaf (show n)
astDisplayExpr _ (LitBoolean _ b)         = node "LitBoolean" . sameBranch . leaf (let c:cs = show b in toLower c : cs)
astDisplayExpr _ (Variable   _ v)         = node "Variable"   . sameBranch . leaf v
astDisplayExpr p (Application _ f as)     = node "Application"
                                          . nextBranch p . leaf f
                                          . lastBranch p . node "Arguments" . astDisplayList 13 astDisplayExpr (lastIndent p) as
astDisplayExpr p (UnaryOp     _ op x)     = node "UnaryOp"
                                          . nextBranch p . leaf (show op)
                                          . lastBranch p . astDisplayExpr (lastIndent p) x
astDisplayExpr p (BinaryOp    _ op x y)   = node "BinaryOp"
                                          . nextBranch p . leaf (show op)
                                          . nextBranch p . astDisplayExpr (nextIndent p) x
                                          . lastBranch p . astDisplayExpr (lastIndent p) y
astDisplayExpr p (TernaryOp   _ op x y z) = node "TernaryOp"
                                          . nextBranch p . leaf (show op)
                                          . nextBranch p . astDisplayExpr (nextIndent p) x
                                          . nextBranch p . astDisplayExpr (nextIndent p) y
                                          . lastBranch p . astDisplayExpr (lastIndent p) z

astDisplayList :: Int -> (ShowS -> a -> ShowS) -> ShowS -> [a] -> ShowS
astDisplayList i f p [x] = sameBranch . f (sameIndent i p) x
astDisplayList _ f p xs  = go xs
  where
    go []     = id
    go [y]    = lastBranch p . f (lastIndent p) y
    go (y:ys) = nextBranch p . f (nextIndent p) y . go ys
