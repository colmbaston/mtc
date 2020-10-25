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
  parseProgram,
)
where

import Parser
import Data.Char
import Data.Functor
import Control.Applicative

import           Data.Set (Set)
import qualified Data.Set as S

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
             deriving (Eq, Show)

data BinaryOp = Addition
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
              deriving (Eq, Show)

data TernaryOp = Conditional deriving (Eq, Show)

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

keywords :: Set Identifier
keywords = S.fromAscList ["begin", "do", "else", "end", "getint", "if", "in", "let", "printint", "then", "var", "while"]

identifier :: Parser Identifier
identifier = sat ((:) <$> sat item isAlpha <*> many (sat item isAlphaNum)) (`S.notMember` keywords)

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
conjExpr = chainl1 relExpr (trim (string "&&" $> BinaryOp Conjunction))

relExpr :: Parser Expr
relExpr = do x <- addExpr
             BinaryOp <$> trim (string "==" $> Equal <|> string "!=" $> NotEqual             <|>
                                string "<" *> (string "=" $> LessEqual    <|> pure Less)     <|>
                                string ">" *> (string "=" $> GreaterEqual <|> pure Greater)) <*> pure x <*> addExpr <|> pure x

addExpr :: Parser Expr
addExpr = chainl1 mulExpr (trim (BinaryOp <$> (string "+" $> Addition <|>
                                               string "-" $> Subtraction)))

mulExpr :: Parser Expr
mulExpr  = chainl1 atomExpr (trim (BinaryOp <$> (string "*" $> Multiplication <|>
                                                 string "/" $> Division)))

atomExpr :: Parser Expr
atomExpr  = Literal  <$> natural
         <|>  Variable <$> identifier
         <|>  parens (trim expr)
         <|>  UnaryOp <$> trimR (string "-" $> IntegerNegation  <|>
                                 string "!" $> BooleanNegation) <*> atomExpr
