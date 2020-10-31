module Lexer (Token(..), tokenise) where

import Parser
import Data.Char
import Data.Functor
import Control.Applicative

data Token = TLet
           | TIn
           | TVar
           | TIf
           | TThen
           | TElse
           | TWhile
           | TDo
           | TGetInt
           | TPrintInt
           | TBegin
           | TEnd
           | TAssign
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
           | ETX
           deriving Eq

tokenise :: String -> Either ParseError [(SrcPos, Token)]
tokenise = fmap fst . parse (many (many space *> oneToken) <* many space <* etx <?> "unrecognised token") . annotate

oneToken :: Parser Char (SrcPos, Token)
oneToken = (,) <$> srcPos <*> (tokens "let"      *> peekNotAlphaNum $> TLet
                          <|>  tokens "in"       *> peekNotAlphaNum $> TIn
                          <|>  tokens "var"      *> peekNotAlphaNum $> TVar
                          <|>  tokens "if"       *> peekNotAlphaNum $> TIf
                          <|>  tokens "then"     *> peekNotAlphaNum $> TThen
                          <|>  tokens "else"     *> peekNotAlphaNum $> TElse
                          <|>  tokens "while"    *> peekNotAlphaNum $> TWhile
                          <|>  tokens "do"       *> peekNotAlphaNum $> TDo
                          <|>  tokens "getint"   *> peekNotAlphaNum $> TGetInt
                          <|>  tokens "printint" *> peekNotAlphaNum $> TPrintInt
                          <|>  tokens "begin"    *> peekNotAlphaNum $> TBegin
                          <|>  tokens "end"      *> peekNotAlphaNum $> TEnd
                          <|>  tokens ":="                          $> TAssign
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
                          <|>  TIdent   <$> ((:) <$> sat nextToken isAlpha <*> many (sat nextToken isAlphaNum))
                          <|>  tokens "\ETX"                        $> ETX)

peekNotAlphaNum :: Parser Char ()
peekNotAlphaNum = sat peekToken (maybe True (not . isAlphaNum)) $> ()
