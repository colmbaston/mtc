module Parser where

import Data.Char
import Data.Functor
import Control.Applicative
import Control.Monad.Trans.State

type Parser = StateT String Maybe

parse :: Parser a -> String -> Maybe (a, String)
parse = runStateT

item :: Parser Char
item = do src <- get
          case src of
            []     -> empty
            (x:xs) -> put xs $> x

eof :: Parser ()
eof = do src <- get
         case src of
           [] -> pure ()
           _  -> empty

sat :: Parser a -> (a -> Bool) -> Parser a
sat px f = do x <- px
              if f x
                 then return x
                 else empty

char :: Char -> Parser Char
char = sat item . (==)

string :: String -> Parser String
string = traverse char

space :: Parser Char
space = sat item isSpace

newline :: Parser Char
newline = sat item (== '\n')

trimL :: Parser a -> Parser a
trimL px = many space *> px

trimR :: Parser a -> Parser a
trimR px = px <* many space

trim :: Parser a -> Parser a
trim = trimL . trimR

parens :: Parser a -> Parser a
parens px = char '(' *> px <* char ')'

pair :: Parser a -> Parser b -> Parser (a, b)
pair px py = (,) <$> px <*> py

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 px pf = foldl (\a (f, y) -> a `f` y) <$> px <*> many (pair pf px)

natural :: Parser Int
natural = read <$> some (sat item isDigit)

integer :: Parser Int
integer = ((string "-" $> negate) <|> pure id) <*> natural
