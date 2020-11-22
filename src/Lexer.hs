module Lexer (Token(..), tokenise) where

import SrcPos
import ParserLib
import Data.Char
import Data.Functor
import Control.Applicative

data Token = TkLet
           | TkIn
           | TkVar
           | TkFun
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
           | TkComma
           | TkDefEqual
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
           | TkLParen
           | TkRParen
           | TkETX
           | TkLitInteger Int
           | TkLitBoolean Bool
           | TkIdent      String
           deriving (Show, Eq)

tokenise :: String -> Either ParseError [(SrcPos, Token)]
tokenise = fmap fst . parse (many (many space *> oneToken) <* many space <* etx <?> "unrecognised token") . annotate

oneToken :: Parser Char (SrcPos, Token)
oneToken = (,) <$> srcPos <*> (TkLet            <$   tokens "let"      <* peekNotAlphaNum
                          <|>  TkIn             <$   tokens "in"       <* peekNotAlphaNum
                          <|>  TkVar            <$   tokens "var"      <* peekNotAlphaNum
                          <|>  TkFun            <$   tokens "fun"      <* peekNotAlphaNum
                          <|>  TkIf             <$   tokens "if"       <* peekNotAlphaNum
                          <|>  TkThen           <$   tokens "then"     <* peekNotAlphaNum
                          <|>  TkElse           <$   tokens "else"     <* peekNotAlphaNum
                          <|>  TkWhile          <$   tokens "while"    <* peekNotAlphaNum
                          <|>  TkDo             <$   tokens "do"       <* peekNotAlphaNum
                          <|>  TkGetInt         <$   tokens "getint"   <* peekNotAlphaNum
                          <|>  TkPrintInt       <$   tokens "printint" <* peekNotAlphaNum
                          <|>  TkBegin          <$   tokens "begin"    <* peekNotAlphaNum
                          <|>  TkEnd            <$   tokens "end"      <* peekNotAlphaNum
                          <|>  TkInteger        <$   tokens "Integer"  <* peekNotAlphaNum
                          <|>  TkBoolean        <$   tokens "Boolean"  <* peekNotAlphaNum
                          <|>  TkComma          <$   tokens ","
                          <|>  TkAssign         <$   tokens ":="
                          <|>  TkSemicolon      <$   tokens ";"
                          <|>  TkQuestion       <$   tokens "?"
                          <|>  TkColon          <$   tokens ":"
                          <|>  TkDisjunction    <$   tokens "||"
                          <|>  TkConjunction    <$   tokens "&&"
                          <|>  TkEqual          <$   tokens "=="
                          <|>  TkDefEqual       <$   tokens "="
                          <|>  TkNotEqual       <$   tokens "!="
                          <|>  TkLessEqual      <$   tokens "<="
                          <|>  TkLess           <$   tokens "<"
                          <|>  TkGreaterEqual   <$   tokens ">="
                          <|>  TkGreater        <$   tokens ">"
                          <|>  TkAddition       <$   tokens "+"
                          <|>  TkSubtraction    <$   tokens "-"
                          <|>  TkMultiplication <$   tokens "*"
                          <|>  TkDivision       <$   tokens "/"
                          <|>  TkExclamation    <$   tokens "!"
                          <|>  TkLParen         <$   tokens "("
                          <|>  TkRParen         <$   tokens ")"
                          <|>  TkETX            <$   tokens "\ETX"
                          <|>  TkLitInteger     <$>  natural <* peekNotAlphaNum
                          <|>  TkLitBoolean     <$> (tokens "false" $> False <|> tokens "true" $> True) <* peekNotAlphaNum
                          <|>  TkIdent          <$> ((:) <$> sat nextToken isAlpha <*> many (sat nextToken isAlphaNum)))

peekNotAlphaNum :: Parser Char ()
peekNotAlphaNum = sat peekToken (maybe True (not . isAlphaNum)) $> ()
