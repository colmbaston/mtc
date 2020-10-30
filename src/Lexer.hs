module Lexer (Token(..), tokenise) where

import Parser
import Data.Char
import Data.Functor
import Control.Applicative

data Token = TLet
           | TIn
           | TVar
           | TAssign
           | TIf
           | TThen
           | TElse
           | TWhile
           | TDo
           | TGetInt
           | TPrintInt
           | TBegin
           | TEnd
           | TSemicolon
           | TQuestion
           | TColon
           | TDisjunction
           | TConjunction
           | TEqual
           | TNotEqual
           | TLess
           | TLessEqual
           | TGreater
           | TGreaterEqual
           | TAddition
           | TSubtraction
           | TMultiplication
           | TDivision
           | TExclamation
           | TLeftPar
           | TRightPar
           | TLiteral Int
           | TIdent   String
           deriving Eq

instance Show Token where
  show  TLet               = "let"
  show  TIn                = "in"
  show  TVar               = "var"
  show  TAssign            = ":="
  show  TIf                = "if"
  show  TThen              = "then"
  show  TElse              = "else"
  show  TWhile             = "while"
  show  TDo                = "do"
  show  TGetInt            = "getint"
  show  TPrintInt          = "printint"
  show  TBegin             = "begin"
  show  TEnd               = "end"
  show  TSemicolon         = ";"
  show  TQuestion          = "?"
  show  TColon             = ":"
  show  TDisjunction       = "||"
  show  TConjunction       = "&&"
  show  TEqual             = "=="
  show  TNotEqual          = "!="
  show  TLess              = "<"
  show  TLessEqual         = "<="
  show  TGreater           = ">"
  show  TGreaterEqual      = ">="
  show  TAddition          = "+"
  show  TSubtraction       = "-"
  show  TMultiplication    = "*"
  show  TDivision          = "/"
  show  TExclamation       = "!"
  show  TLeftPar           = "("
  show  TRightPar          = ")"
  show (TLiteral n)        = show n
  show (TIdent   i)        = i

tokenise :: String -> Either ParseError [(SrcPos, Token)]
tokenise = fmap fst . parse (many (many space *> oneToken) <* many space <* eof <?> "unrecognised token") . annotate

oneToken :: Parser Char (SrcPos, Token)
oneToken = (,) <$> srcPos <*> (tokens "let"      *> peekNotAlphaNum $> TLet
                          <|>  tokens "in"       *> peekNotAlphaNum $> TIn
                          <|>  tokens "var"      *> peekNotAlphaNum $> TVar
                          <|>  tokens ":="                          $> TAssign
                          <|>  tokens "if"       *> peekNotAlphaNum $> TIf
                          <|>  tokens "then"     *> peekNotAlphaNum $> TThen
                          <|>  tokens "else"     *> peekNotAlphaNum $> TElse
                          <|>  tokens "while"    *> peekNotAlphaNum $> TWhile
                          <|>  tokens "do"       *> peekNotAlphaNum $> TDo
                          <|>  tokens "getint"   *> peekNotAlphaNum $> TGetInt
                          <|>  tokens "printint" *> peekNotAlphaNum $> TPrintInt
                          <|>  tokens "begin"    *> peekNotAlphaNum $> TBegin
                          <|>  tokens "end"      *> peekNotAlphaNum $> TEnd
                          <|>  tokens ";"                           $> TSemicolon
                          <|>  tokens "?"                           $> TQuestion
                          <|>  tokens ":"                           $> TColon
                          <|>  tokens "||"                          $> TDisjunction
                          <|>  tokens "&&"                          $> TConjunction
                          <|>  tokens "=="                          $> TEqual
                          <|>  tokens "!="                          $> TNotEqual
                          <|>  tokens "<="                          $> TLessEqual
                          <|>  tokens "<"                           $> TLess
                          <|>  tokens ">="                          $> TGreaterEqual
                          <|>  tokens ">"                           $> TGreater
                          <|>  tokens "+"                           $> TAddition
                          <|>  tokens "-"                           $> TSubtraction
                          <|>  tokens "*"                           $> TMultiplication
                          <|>  tokens "/"                           $> TDivision
                          <|>  tokens "!"                           $> TExclamation
                          <|>  tokens "("                           $> TLeftPar
                          <|>  tokens ")"                           $> TRightPar
                          <|>  TLiteral <$> natural <* peekNotAlphaNum
                          <|>  TIdent   <$> ((:) <$> sat nextToken isAlpha <*> many (sat nextToken isAlphaNum)))

peekNotAlphaNum :: Parser Char ()
peekNotAlphaNum = sat peek (maybe True (not . isAlphaNum . snd)) $> ()
