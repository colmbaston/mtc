module Parser (parseProgram) where

import ParserLib
import Lexer
import AST

import Data.Functor
import Control.Applicative

-- PROGRAM PARSER

parseProgram :: String -> Either ParseError Program
parseProgram src = tokenise src >>= fmap fst . parse (prog <* (token TkETX <* etx) <?> "successfully parsed a program, but there are unconsumed tokens")

parens :: Parser Token a -> Parser Token a
parens px = do sp <- srcPos
               token TkLParen <?> "expected token \"(\"" *> px <*  token TkRParen <?> ("expected token \")\" to close the \"(\" at " ++ show sp)

prog :: Parser Token Program
prog = Program <$> (token TkLet <?> "expected token \"let\"" *> decls)
               <*> (token TkIn  <?> "expected token \"in\" or a \";\" followed by another declaration" *> command)

decls :: Parser Token [Declaration]
decls = (:) <$> decl
            <*> do tk <- peekToken
                   case tk of
                     Just TkSemicolon -> nextToken *> decls
                     _                -> pure []

decl :: Parser Token Declaration
decl = do (sp, i) <- token TkVar  <?> "expected token \"var\"" *> ((,) <$> srcPos <*> identifier)
          ty      <- token TkColon *> typeMT
          tk      <- peekToken
          Initialise sp i ty <$> case tk of
                                   Just TkAssign -> nextToken *> expr
                                   _             -> pure (case ty of
                                                            IntegerMT -> LitInteger sp 0
                                                            BooleanMT -> LitBoolean sp False)

typeMT :: Parser Token TypeMT
typeMT =  token TkInteger $> IntegerMT
      <|> token TkBoolean $> BooleanMT
      <|> empty <?> "expected a type"

command :: Parser Token Command
command =  Assign         <$>  srcPos
                          <*>  identifier
                          <*> (token TkAssign  <?> "expected token \":=\""   *> expr)
       <|> If             <$> (token TkIf       *> expr)
                          <*> (token TkThen    <?> "expected token \"then\" or an operator (of appropriate precedence) followed by a subexpression" *> command)
                          <*> (token TkElse    <?> "expected token \"else\"" *> command)
       <|> While          <$> (token TkWhile    *> expr)
                          <*> (token TkDo      <?> "expected token \"do\" or an operator (of appropriate precedence) followed by a subexpression"   *> command)
       <|> uncurry GetInt <$> (token TkGetInt   *> parens ((,) <$> srcPos <*> identifier))
       <|> PrintInt       <$> (token TkPrintInt *> parens expr)
       <|> Block          <$> (token TkBegin    *> commands <* token TkEnd <?> "expected token \"end\" or a \";\" followed by another command")
       <|> empty          <?> "expected a command"

commands :: Parser Token [Command]
commands = (:) <$> command
               <*> do tk <- peekToken
                      case tk of
                        Just TkSemicolon -> nextToken *> commands
                        _                -> pure []

identifier :: Parser Token String
identifier = do tk <- peekToken
                case tk of
                  Just (TkIdent i) -> nextToken $> i
                  _                -> empty    <?> "expected an identifier"

-- EXPRESSION PARSER

expr :: Parser Token Expr
expr = condExpr

condExpr :: Parser Token Expr
condExpr = do sp <- srcPos
              x  <- disjExpr
              tk <- peekToken
              case tk of
                Just TkQuestion -> TernaryOp sp Conditional x <$> (nextToken *> disjExpr) <*> (token TkColon <?> "expected token \":\"" *> disjExpr)
                _               -> pure x

disjExpr :: Parser Token Expr
disjExpr = do sp <- srcPos
              x  <- conjExpr
              tk <- peekToken
              case tk of
                Just TkDisjunction -> BinaryOp sp Disjunction x <$> (nextToken *> disjExpr)
                _                  -> pure x

conjExpr :: Parser Token Expr
conjExpr = do sp <- srcPos
              x <- relExpr
              tk <- peekToken
              case tk of
                Just TkConjunction -> BinaryOp sp Conjunction x <$> (nextToken *> conjExpr)
                _                  -> pure x

relExpr :: Parser Token Expr
relExpr = do sp <- srcPos
             x  <- addExpr
             tk <- peekToken
             case tk of
               Just TkEqual        -> BinaryOp sp Equal        x <$> (nextToken *> addExpr)
               Just TkNotEqual     -> BinaryOp sp NotEqual     x <$> (nextToken *> addExpr)
               Just TkLess         -> BinaryOp sp Less         x <$> (nextToken *> addExpr)
               Just TkLessEqual    -> BinaryOp sp LessEqual    x <$> (nextToken *> addExpr)
               Just TkGreater      -> BinaryOp sp Greater      x <$> (nextToken *> addExpr)
               Just TkGreaterEqual -> BinaryOp sp GreaterEqual x <$> (nextToken *> addExpr)
               _                   -> pure x

addExpr :: Parser Token Expr
addExpr = go id
  where
    go :: (Expr -> Expr) -> Parser Token Expr
    go f = do sp <- srcPos
              x  <- f <$> mulExpr
              tk <- peekToken
              case tk of
                Just TkAddition    -> nextToken *> go (BinaryOp sp Addition    x)
                Just TkSubtraction -> nextToken *> go (BinaryOp sp Subtraction x)
                _                  -> pure x

mulExpr :: Parser Token Expr
mulExpr = go id
  where
    go :: (Expr -> Expr) -> Parser Token Expr
    go f = do sp <- srcPos
              x  <- f <$> atomExpr
              tk <- peekToken
              case tk of
                Just TkMultiplication -> nextToken *> go (BinaryOp sp Multiplication x)
                Just TkDivision       -> nextToken *> go (BinaryOp sp Division       x)
                _                     -> pure x

atomExpr :: Parser Token Expr
atomExpr = do sp <- srcPos
              tk <- peekToken
              case tk of
                Just (TkLitInteger n) -> LitInteger sp <$> (nextToken $> n)
                Just (TkLitBoolean b) -> LitBoolean sp <$> (nextToken $> b)
                Just (TkIdent      v) -> Variable   sp <$> (nextToken $> v)
                Just  TkLParen        -> parens expr
                Just  TkSubtraction   -> UnaryOp sp IntegerNegation <$> (nextToken *> atomExpr)
                Just  TkExclamation   -> UnaryOp sp BooleanNegation <$> (nextToken *> atomExpr)
                _                     -> empty <?> "expected an expression"
