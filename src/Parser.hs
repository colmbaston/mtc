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
decl = do tk <- nextToken
          case tk of
            TkVar -> varDecl
            TkFun -> funDecl
            _     -> empty <?> "expected token \"var\" or \"fun\""

varDecl :: Parser Token Declaration
varDecl = do sp <- srcPos
             i  <- identifier
             ty <- token TkColon <?> "expected token \":\"" *> typeMT
             Initialise sp i ty <$> do tk <- peekToken
                                       case tk of
                                         Just TkAssign -> nextToken *> expr
                                         _             -> pure (case ty of
                                                                  IntegerMT -> LitInteger sp 0
                                                                  BooleanMT -> LitBoolean sp False)

funDecl :: Parser Token Declaration
funDecl = Function <$>  srcPos
                   <*>  identifier
                   <*>  parens funParams
                   <*> (token TkColon    <?> "expected token \":\"" *> typeMT)
                   <*> (token TkDefEqual <?> "expected token \"=\"" *> expr)

funParams :: Parser Token [Param]
funParams = do tk <- peekToken
               case tk of
                 Just TkRParen -> pure []
                 _             -> nonEmptyFunParams

nonEmptyFunParams :: Parser Token [Param]
nonEmptyFunParams = (:) <$> funParam <*> do tk <- peekToken
                                            case tk of
                                              Just TkComma  -> nextToken *> nonEmptyFunParams
                                              Just TkRParen -> pure []
                                              _             -> empty <?> "expected token \",\" or the end of the parameter list"

funParam :: Parser Token Param
funParam = Param <$>  srcPos
                 <*>  identifier
                 <*> (token TkColon <?> "expected token \":\"" *> typeMT)

typeMT :: Parser Token TypeMT
typeMT =  token TkInteger $> IntegerMT
      <|> token TkBoolean $> BooleanMT
      <|> empty <?> "expected token \"Integer\" or \"Boolean\""

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
                Just TkQuestion -> nextToken *> (TernaryOp sp Conditional x <$> disjExpr <*> (token TkColon <?> "expected token \":\"" *> disjExpr))
                _               -> pure x

disjExpr :: Parser Token Expr
disjExpr = do sp <- srcPos
              x  <- conjExpr
              tk <- peekToken
              case tk of
                Just TkDisjunction -> nextToken *> (BinaryOp sp Disjunction x <$> disjExpr)
                _                  -> pure x

conjExpr :: Parser Token Expr
conjExpr = do sp <- srcPos
              x <- relExpr
              tk <- peekToken
              case tk of
                Just TkConjunction -> nextToken *> (BinaryOp sp Conjunction x <$> conjExpr)
                _                  -> pure x

relExpr :: Parser Token Expr
relExpr = do sp <- srcPos
             x  <- addExpr
             tk <- peekToken
             case tk of
               Just TkEqual        -> nextToken *> (BinaryOp sp Equal        x <$> addExpr)
               Just TkNotEqual     -> nextToken *> (BinaryOp sp NotEqual     x <$> addExpr)
               Just TkLess         -> nextToken *> (BinaryOp sp Less         x <$> addExpr)
               Just TkLessEqual    -> nextToken *> (BinaryOp sp LessEqual    x <$> addExpr)
               Just TkGreater      -> nextToken *> (BinaryOp sp Greater      x <$> addExpr)
               Just TkGreaterEqual -> nextToken *> (BinaryOp sp GreaterEqual x <$> addExpr)
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
                Just (TkLitInteger n) -> nextToken $> LitInteger sp n
                Just (TkLitBoolean b) -> nextToken $> LitBoolean sp b
                Just (TkIdent      i) -> nextToken *> atomIdent  sp i
                Just  TkLParen        -> parens expr
                Just  TkSubtraction   -> nextToken *> (UnaryOp sp IntegerNegation <$> atomExpr)
                Just  TkExclamation   -> nextToken *> (UnaryOp sp BooleanNegation <$> atomExpr)
                _                     -> empty <?> "expected an expression"

atomIdent :: SrcPos -> String -> Parser Token Expr
atomIdent sp i = do tk <- peekToken
                    case tk of
                      Just TkLParen -> Application sp i <$> parens funArgs
                      _             -> pure (Variable sp i)

funArgs :: Parser Token [Expr]
funArgs = do tk <- peekToken
             case tk of
               Just TkRParen -> pure []
               _             -> nonEmptyFunArgs

nonEmptyFunArgs :: Parser Token [Expr]
nonEmptyFunArgs = (:) <$> expr <*> do tk <- peekToken
                                      case tk of
                                        Just TkComma  -> nextToken *> nonEmptyFunArgs
                                        Just TkRParen -> pure []
                                        _             -> empty <?> "expected token \",\" or the end of the argument list"
