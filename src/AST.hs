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
import Lexer

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
data Expr        = Literal   Int
                 | Variable  Identifier
                 | UnaryOp   UnaryOp   Expr
                 | BinaryOp  BinaryOp  Expr Expr
                 | TernaryOp TernaryOp Expr Expr Expr
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

-- PROGRAM PARSER

parseProgram :: String -> Either ParseError Program
parseProgram src = tokenise src >>= fmap fst . parse (prog <* token ETX <* etx <?> "successfully parsed a program, but there are unconsumed tokens")

parens :: Parser Token a -> Parser Token a
parens px = do sp <- srcPos
               token TLeftPar <?> "expected token \"(\"" *> px <*  token TRightPar <?> ("expected token \")\" to close the \"(\" at " ++ show sp)

prog :: Parser Token Program
prog = Program <$> (token TLet <?> "expected token \"let\"" *> decls)
               <*> (token TIn  <?> "expected token \"in\" or a \";\" followed by another declaration" *> command)

decls :: Parser Token [Declaration]
decls = (:) <$> decl <*> do t <- peekToken
                            case t of
                              Just TSemicolon -> nextToken *> decls
                              _               -> pure []

decl :: Parser Token Declaration
decl = Initialise <$> (token TVar <?> "expected token \"var\"" *> identifier)
                  <*> (do t <- peekToken
                          case t of
                            Just TAssign -> nextToken *> expr
                            _            -> pure (Literal 0))

command :: Parser Token Command
command =  Assign   <$> identifier
                    <*> (token TAssign  <?> "expected token \":=\""   *> expr)
       <|> If       <$> (token TIf       *> expr)
                    <*> (token TThen    <?> "expected token \"then\" or an operator (of appropriate precedence) followed by a subexpression" *> command)
                    <*> (token TElse    <?> "expected token \"else\"" *> command)
       <|> While    <$> (token TWhile    *> expr)
                    <*> (token TDo      <?> "expected token \"do\" or an operator (of appropriate precedence) followed by a subexpression"   *> command)
       <|> GetInt   <$> (token TGetInt   *> parens identifier)
       <|> PrintInt <$> (token TPrintInt *> parens expr)
       <|> Block    <$> (token TBegin    *> commands <* token TEnd <?> "expected token \"end\" or a \";\" followed by another command")
       <|> empty    <?> "expected a command"

commands :: Parser Token [Command]
commands = (:) <$> command <*> do t <- peekToken
                                  case t of
                                    Just TSemicolon -> nextToken *> commands
                                    _               -> pure []

identifier :: Parser Token Identifier
identifier = do t <- peekToken
                case t of
                  Just (TIdent i) -> nextToken $> i
                  _               -> empty <?> "expected an identifier"

-- EXPRESSION PARSER

expr :: Parser Token Expr
expr = condExpr

condExpr :: Parser Token Expr
condExpr = do x <- disjExpr
              t <- peekToken
              case t of
                Just TQuestion -> TernaryOp Conditional x <$> (nextToken *> disjExpr) <*> (token TColon <?> "expected token \":\"" *> disjExpr)
                _              -> pure x

disjExpr :: Parser Token Expr
disjExpr = do x <- conjExpr
              t <- peekToken
              case t of
                Just TDisjunction -> BinaryOp Disjunction x <$> (nextToken *> disjExpr)
                _                 -> pure x

conjExpr :: Parser Token Expr
conjExpr = do x <- relExpr
              t <- peekToken
              case t of
                Just TConjunction -> BinaryOp Conjunction x <$> (nextToken *> conjExpr)
                _                 -> pure x

relExpr :: Parser Token Expr
relExpr = do x <- addExpr
             t <- peekToken
             case t of
               Just TEqual        -> BinaryOp Equal        x <$> (nextToken *> addExpr)
               Just TNotEqual     -> BinaryOp NotEqual     x <$> (nextToken *> addExpr)
               Just TLess         -> BinaryOp Less         x <$> (nextToken *> addExpr)
               Just TLessEqual    -> BinaryOp LessEqual    x <$> (nextToken *> addExpr)
               Just TGreater      -> BinaryOp Greater      x <$> (nextToken *> addExpr)
               Just TGreaterEqual -> BinaryOp GreaterEqual x <$> (nextToken *> addExpr)
               _                  -> pure x

addExpr :: Parser Token Expr
addExpr = go id
  where
    go :: (Expr -> Expr) -> Parser Token Expr
    go f = do x <- f <$> mulExpr
              t <- peekToken
              case t of
                Just TAddition    -> nextToken *> go (BinaryOp Addition    x)
                Just TSubtraction -> nextToken *> go (BinaryOp Subtraction x)
                _                 -> pure x

mulExpr :: Parser Token Expr
mulExpr = go id
  where
    go :: (Expr -> Expr) -> Parser Token Expr
    go f = do x <- f <$> atomExpr
              t <- peekToken
              case t of
                Just TMultiplication -> nextToken *> go (BinaryOp Multiplication x)
                Just TDivision       -> nextToken *> go (BinaryOp Division       x)
                _                    -> pure x

atomExpr :: Parser Token Expr
atomExpr = do t <- peekToken
              case t of
                Just (TLiteral n)  -> nextToken $> Literal  n
                Just (TIdent   i)  -> nextToken $> Variable i
                Just  TLeftPar     -> parens expr
                Just  TSubtraction -> UnaryOp IntegerNegation <$> (nextToken *> atomExpr)
                Just  TExclamation -> UnaryOp BooleanNegation <$> (nextToken *> atomExpr)
                _                  -> empty <?> "expected an expression"
