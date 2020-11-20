module Parser (parseProgram) where

import ParserLib
import Lexer
import AST

import Data.Functor
import Control.Applicative

-- PROGRAM PARSER

parseProgram :: String -> Either ParseError Program
parseProgram src = tokenise src >>= fmap fst . parse (prog <* token TkETX <* etx <?> "successfully parsed a program, but there are unconsumed tokens")

parens :: Parser Token a -> Parser Token a
parens px = do sp <- srcPos
               token TkLeftPar <?> "expected token \"(\"" *> px <*  token TkRightPar <?> ("expected token \")\" to close the \"(\" at " ++ show sp)

prog :: Parser Token Program
prog = Program <$> (token TkLet <?> "expected token \"let\"" *> decls)
               <*> (token TkIn  <?> "expected token \"in\" or a \";\" followed by another declaration" *> command)

decls :: Parser Token [Declaration]
decls = (:) <$> decl
            <*> do tk <- peekToken
                   case tk of
                     Just TkSemicolon -> nextToken *> decls
                     _               -> pure []

decl :: Parser Token Declaration
decl = do i  <- token TkVar   <?> "expected token \"var\"" *> identifier
          ty <- token TkColon <?> "expected token \":\""   *> typeMT
          tk <- peekToken
          Initialise i ty <$> case tk of
                                Just TkAssign -> nextToken *> expr
                                _             -> pure (case ty of
                                                         IntegerMT -> LitInteger 0
                                                         BooleanMT -> LitBoolean False)

typeMT :: Parser Token TypeMT
typeMT =  token TkInteger $> IntegerMT
      <|> token TkBoolean $> BooleanMT
      <|> empty <?> "expected a type"

command :: Parser Token Command
command =  Assign   <$>  identifier
                    <*> (token TkAssign  <?> "expected token \":=\""   *> expr)
       <|> If       <$> (token TkIf       *> expr)
                    <*> (token TkThen    <?> "expected token \"then\" or an operator (of appropriate precedence) followed by a subexpression" *> command)
                    <*> (token TkElse    <?> "expected token \"else\"" *> command)
       <|> While    <$> (token TkWhile    *> expr)
                    <*> (token TkDo      <?> "expected token \"do\" or an operator (of appropriate precedence) followed by a subexpression"   *> command)
       <|> GetInt   <$> (token TkGetInt   *> parens identifier)
       <|> PrintInt <$> (token TkPrintInt *> parens expr)
       <|> Block    <$> (token TkBegin    *> commands <* token TkEnd <?> "expected token \"end\" or a \";\" followed by another command")
       <|> empty    <?> "expected a command"

commands :: Parser Token [Command]
commands = (:) <$> command
               <*> do tk <- peekToken
                      case tk of
                        Just TkSemicolon -> nextToken *> commands
                        _                -> pure []

identifier :: Parser Token Identifier
identifier = do tk <- peekToken
                case tk of
                  Just (TkIdent i) -> Identifier <$> srcPos <*> (nextToken $> i)
                  _                -> empty <?> "expected an identifier"

-- EXPRESSION PARSER

expr :: Parser Token Expr
expr = condExpr

condExpr :: Parser Token Expr
condExpr = do x  <- disjExpr
              tk <- peekToken
              case tk of
                Just TkQuestion -> TernaryOp Conditional x <$> (nextToken *> disjExpr) <*> (token TkColon <?> "expected token \":\"" *> disjExpr)
                _               -> pure x

disjExpr :: Parser Token Expr
disjExpr = do x <- conjExpr
              tk <- peekToken
              case tk of
                Just TkDisjunction -> BinaryOp Disjunction x <$> (nextToken *> disjExpr)
                _                  -> pure x

conjExpr :: Parser Token Expr
conjExpr = do x <- relExpr
              tk <- peekToken
              case tk of
                Just TkConjunction -> BinaryOp Conjunction x <$> (nextToken *> conjExpr)
                _                  -> pure x

relExpr :: Parser Token Expr
relExpr = do x  <- addExpr
             tk <- peekToken
             case tk of
               Just TkEqual        -> BinaryOp Equal        x <$> (nextToken *> addExpr)
               Just TkNotEqual     -> BinaryOp NotEqual     x <$> (nextToken *> addExpr)
               Just TkLess         -> BinaryOp Less         x <$> (nextToken *> addExpr)
               Just TkLessEqual    -> BinaryOp LessEqual    x <$> (nextToken *> addExpr)
               Just TkGreater      -> BinaryOp Greater      x <$> (nextToken *> addExpr)
               Just TkGreaterEqual -> BinaryOp GreaterEqual x <$> (nextToken *> addExpr)
               _                   -> pure x

addExpr :: Parser Token Expr
addExpr = go id
  where
    go :: (Expr -> Expr) -> Parser Token Expr
    go f = do x  <- f <$> mulExpr
              tk <- peekToken
              case tk of
                Just TkAddition    -> nextToken *> go (BinaryOp Addition    x)
                Just TkSubtraction -> nextToken *> go (BinaryOp Subtraction x)
                _                  -> pure x

mulExpr :: Parser Token Expr
mulExpr = go id
  where
    go :: (Expr -> Expr) -> Parser Token Expr
    go f = do x  <- f <$> atomExpr
              tk <- peekToken
              case tk of
                Just TkMultiplication -> nextToken *> go (BinaryOp Multiplication x)
                Just TkDivision       -> nextToken *> go (BinaryOp Division       x)
                _                     -> pure x

atomExpr :: Parser Token Expr
atomExpr = do tk <- peekToken
              case tk of
                Just (TkLitInteger n) -> LitInteger <$> (nextToken $> n)
                Just (TkLitBoolean b) -> LitBoolean <$> (nextToken $> b)
                Just (TkIdent      i) -> Variable <$> (Identifier <$> srcPos <*> (nextToken $> i))
                Just  TkLeftPar       -> parens expr
                Just  TkSubtraction   -> UnaryOp IntegerNegation  <$> (nextToken *> atomExpr)
                Just  TkExclamation   -> UnaryOp BooleanNegation  <$> (nextToken *> atomExpr)
                _                     -> empty <?> "expected an expression"
