module AST
(
  Identifier,
  Program(..),
  Declaration(..),
  Command(..),
  Expr(..),
  UnaryOp(..),
  BinaryOp(..),
  TernaryOp(..),
  astPrint,
  parseProgram,
)
where

import Parser
import Data.Functor
import Control.Applicative

type Identifier  = String
data Program     = Program [Declaration] Command
data Declaration = Initialise Identifier Expr
data Command     = Assign     Identifier Expr
                 | If Expr Command Command
                 | While Expr Command
                 | GetInt Identifier
                 | PrintInt Expr
                 | Block [Command]

data Expr = Literal   Int
          | Variable  Identifier
          | UnaryOp   UnaryOp   Expr
          | BinaryOp  BinaryOp  Expr Expr
          | TernaryOp TernaryOp Expr Expr Expr

data UnaryOp = IntegerNegation
             | BooleanNegation
             deriving Show

data BinaryOp = Addition
              | Subtraction
              | Multiplication
              | Division
              | Conjunction
              | Disjunction
              | Equal
              | NotEqual
              | Less
              | LessEqual
              | GreaterEqual
              | Greater
              deriving Show

data TernaryOp = Conditional deriving Show

-- PRINT AST AS A TREE

astPrint :: Program -> IO ()
astPrint p = putStrLn (treePrintProgPrefixed id p "")

treePrintProgPrefixed :: ShowS -> Program -> ShowS
treePrintProgPrefixed p (Program ds c) =     showString "Program\n"
                                       . p . showString "├── Declarations\n" . treePrintListPrefixed treePrintDeclPrefixed (p . showString "│   ") ds . showChar '\n'
                                       . p . showString "└── "               . treePrintCommandPrefixed   (p . showString "    ") c

treePrintListPrefixed :: (ShowS -> a -> ShowS) -> ShowS -> [a] -> ShowS
treePrintListPrefixed _ _ []     = id
treePrintListPrefixed f p [x]    = p . showString "└── " . f (p . showString "    ") x
treePrintListPrefixed f p (x:xs) = p . showString "├── " . f (p . showString "│   ") x . showChar '\n' . treePrintListPrefixed f p xs

treePrintDeclPrefixed :: ShowS -> Declaration -> ShowS
treePrintDeclPrefixed p (Initialise i e) =     showString "Initialise\n"
                                         . p . showString "├── Variable " . shows i . showChar '\n'
                                         . p . showString "└── " . treePrintExprPrefixed (p . showString "    ") e

treePrintCommandPrefixed :: ShowS -> Command -> ShowS
treePrintCommandPrefixed p (Assign i e) =     showString "Assign\n"
                                        . p . showString "├── Variable " . shows i . showChar '\n'
                                        . p . showString "└── " . treePrintExprPrefixed (p . showString "    ") e
treePrintCommandPrefixed p (If e t f)   =     showString "If\n"
                                        . p . showString "├── " . treePrintExprPrefixed    (p . showString "│   ") e . showChar '\n'
                                        . p . showString "├── " . treePrintCommandPrefixed (p . showString "│   ") t . showChar '\n'
                                        . p . showString "└── " . treePrintCommandPrefixed (p . showString "    ") f
treePrintCommandPrefixed p (While e c)  =     showString "While\n"
                                        . p . showString "├── " . treePrintExprPrefixed    (p . showString "│   ") e . showChar '\n'
                                        . p . showString "└── " . treePrintCommandPrefixed (p . showString "    ") c
treePrintCommandPrefixed p (GetInt i)   =     showString "GetInt\n"
                                        . p . showString "└── Variable " . shows i
treePrintCommandPrefixed p (PrintInt e) =     showString "PrintInt\n"
                                        . p . showString "└── " . treePrintExprPrefixed (p . showString "    ") e
treePrintCommandPrefixed p (Block cs)   =     showString "Block\n" . treePrintListPrefixed treePrintCommandPrefixed p cs

treePrintExprPrefixed :: ShowS -> Expr -> ShowS
treePrintExprPrefixed _ (Literal n)          =     showString "Literal "   . shows n
treePrintExprPrefixed _ (Variable i)         =     showString "Variable "  . shows i
treePrintExprPrefixed p (UnaryOp op x)       =     showString "UnaryOp "   . shows op                                  . showChar '\n'
                                             . p . showString "└── " . treePrintExprPrefixed (p . showString "    ") x
treePrintExprPrefixed p (BinaryOp op x y)    =     showString "BinaryOp "  . shows op                                  . showChar '\n'
                                             . p . showString "├── " . treePrintExprPrefixed (p . showString "│   ") x . showChar '\n'
                                             . p . showString "└── " . treePrintExprPrefixed (p . showString "    ") y
treePrintExprPrefixed p (TernaryOp op x y z) =     showString "TernaryOp " . shows op                                  . showChar '\n'
                                             . p . showString "├── " . treePrintExprPrefixed (p . showString "│   ") x . showChar '\n'
                                             . p . showString "├── " . treePrintExprPrefixed (p . showString "│   ") y . showChar '\n'
                                             . p . showString "└── " . treePrintExprPrefixed (p . showString "    ") z

-- PROGRAM PARSER

parseProgram :: String -> Maybe Program
parseProgram src = fst <$> parse (trim prog <* eof) src

prog :: Parser Program
prog = Program <$> (string "let" *> some space *> decls)
               <*> (some space *> string "in" *> some space *> command)

decls :: Parser [Declaration]
decls = (:) <$> decl <*> ((trim (string ";") *> decls) <|> pure [])

decl :: Parser Declaration
decl = Initialise <$> (       string "var" *> some space *> identifier)
                  <*> ((trim (string ":=") *> expr) <|> pure (Literal 0))

command :: Parser Command
command = Assign   <$> identifier <*> (trim (string ":=") *> expr) <|>
          If       <$> (string "if"       *> some space *> expr) <*> (some space *> string "then" *> some space *> command) <*> (some space *> string "else" *> some space *> command) <|>
          While    <$> (string "while"    *> some space *> expr) <*> (some space *> string "do"   *> some space *> command) <|>
          GetInt   <$> (string "getint"   *> many space *> parens (trim identifier)) <|>
          PrintInt <$> (string "printint" *> many space *> parens (trim expr)) <|>
          Block    <$> (string "begin"    *> some space *> commands <* some space <* string "end")

commands :: Parser [Command]
commands = (:) <$> command <*> ((trim (string ";") *> commands) <|> pure [])

-- EXPRESSION PARSER

expr :: Parser Expr
expr = condExpr

condExpr :: Parser Expr
condExpr = do x <- disjExpr
              (TernaryOp Conditional x
                 <$> (trim (string "?") *> disjExpr)
                 <*> (trim (string ":") *> disjExpr)) <|> pure x

disjExpr :: Parser Expr
disjExpr = chainl1 conjExpr (trim (string "||" $> BinaryOp Disjunction))

conjExpr :: Parser Expr
conjExpr = chainl1 equExpr (trim (string "&&" $> BinaryOp Conjunction))

equExpr :: Parser Expr
equExpr = do x <- relExpr
             BinaryOp <$> trim (string "==" $> Equal <|>
                                string "!=" $> NotEqual) <*> pure x <*> relExpr <|> pure x

relExpr :: Parser Expr
relExpr = do x <- addExpr
             BinaryOp <$> trim (string "<" *> (string "=" $> LessEqual    <|> pure Less)     <|>
                                string ">" *> (string "=" $> GreaterEqual <|> pure Greater)) <*> pure x <*> addExpr <|> pure x

addExpr :: Parser Expr
addExpr = chainl1 mulExpr (trim (BinaryOp <$> (string "+" $> Addition <|>
                                               string "-" $> Subtraction)))

mulExpr :: Parser Expr
mulExpr  = chainl1 atomExpr (trim (BinaryOp <$> (string "*" $> Multiplication <|>
                                                 string "/" $> Division)))

atomExpr :: Parser Expr
atomExpr  =   Literal  <$> natural
         <|>  Variable <$> identifier
         <|>  parens (trim expr)
         <|>  UnaryOp <$> trimR (string "-" $> IntegerNegation  <|>
                                 string "!" $> BooleanNegation) <*> atomExpr
