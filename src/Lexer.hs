module Lexer (Token(..), tokenise) where

import SrcPos
import ParserLib
import Data.Char
import Data.Functor
import Control.Applicative

data Token = TkLet
           | TkIn
           | TkVar
           | TkIf
           | TkThen
           | TkElse
           | TkWhile
           | TkDo
           | TkGetInt
           | TkPrintInt
           | TkBegin
           | TkEnd
           | TkInteger
           | TkBoolean
           | TkLitInteger Int
           | TkLitBoolean Bool
           | TkIdent      String
           | TkAssign
           | TkSemicolon
           | TkQuestion
           | TkColon
           | TkDisjunction
           | TkConjunction
           | TkEqual
           | TkNotEqual
           | TkLess
           | TkLessEqual
           | TkGreater
           | TkGreaterEqual
           | TkAddition
           | TkSubtraction
           | TkMultiplication
           | TkDivision
           | TkExclamation
           | TkLeftPar
           | TkRightPar
           | TkETX
           deriving (Show, Eq)

tokenise :: String -> Either ParseError [(SrcPos, Token)]
tokenise = fmap fst . parse (many (many space *> oneToken) <* many space <* etx <?> "unrecognised token") . annotate

oneToken :: Parser Char (SrcPos, Token)
oneToken = (,) <$> srcPos <*> (tokens "let"      $> TkLet      <* peekNotAlphaNum
                          <|>  tokens "in"       $> TkIn       <* peekNotAlphaNum
                          <|>  tokens "var"      $> TkVar      <* peekNotAlphaNum
                          <|>  tokens "if"       $> TkIf       <* peekNotAlphaNum
                          <|>  tokens "then"     $> TkThen     <* peekNotAlphaNum
                          <|>  tokens "else"     $> TkElse     <* peekNotAlphaNum
                          <|>  tokens "while"    $> TkWhile    <* peekNotAlphaNum
                          <|>  tokens "do"       $> TkDo       <* peekNotAlphaNum
                          <|>  tokens "getint"   $> TkGetInt   <* peekNotAlphaNum
                          <|>  tokens "printint" $> TkPrintInt <* peekNotAlphaNum
                          <|>  tokens "begin"    $> TkBegin    <* peekNotAlphaNum
                          <|>  tokens "end"      $> TkEnd      <* peekNotAlphaNum
                          <|>  tokens "Integer"  $> TkInteger  <* peekNotAlphaNum
                          <|>  tokens "Boolean"  $> TkBoolean  <* peekNotAlphaNum
                          <|>  TkLitInteger <$>  natural <* peekNotAlphaNum
                          <|>  TkLitBoolean <$> (tokens "false" $> False <|> tokens "true" $> True) <* peekNotAlphaNum
                          <|>  TkIdent      <$> ((:) <$> sat nextToken isAlpha <*> many (sat nextToken isAlphaNum))
                          <|>  tokens ":="       $> TkAssign
                          <|>  tokens ";"        $> TkSemicolon
                          <|>  tokens "?"        $> TkQuestion
                          <|>  tokens ":"        $> TkColon
                          <|>  tokens "||"       $> TkDisjunction
                          <|>  tokens "&&"       $> TkConjunction
                          <|>  tokens "=="       $> TkEqual
                          <|>  tokens "!="       $> TkNotEqual
                          <|>  tokens "<="       $> TkLessEqual
                          <|>  tokens "<"        $> TkLess
                          <|>  tokens ">="       $> TkGreaterEqual
                          <|>  tokens ">"        $> TkGreater
                          <|>  tokens "+"        $> TkAddition
                          <|>  tokens "-"        $> TkSubtraction
                          <|>  tokens "*"        $> TkMultiplication
                          <|>  tokens "/"        $> TkDivision
                          <|>  tokens "!"        $> TkExclamation
                          <|>  tokens "("        $> TkLeftPar
                          <|>  tokens ")"        $> TkRightPar
                          <|>  tokens "\ETX"     $> TkETX)

peekNotAlphaNum :: Parser Char ()
peekNotAlphaNum = sat peekToken (maybe True (not . isAlphaNum)) $> ()
