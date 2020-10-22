module Parser where

import Data.Char
import Data.Functor
import Control.Applicative

newtype Parser a = P { parse :: String -> [(a, String)] }

instance Functor Parser where
  fmap f px = P (\src -> [(f x, src') | (x, src') <- parse px src])

instance Applicative Parser where
  pure x    = P (\src -> [(x, src)])
  pf <*> px = P (\src -> [(f x, src'') | (f, src' ) <- parse pf src,
                                         (x, src'') <- parse px src'])

instance Monad Parser where
  px >>= f = P (\src -> [(y, src'') | (x, src' ) <- parse px    src,
                                      (y, src'') <- parse (f x) src'])

instance Alternative Parser where
  empty     = P (const [])
  px <|> py = P (\src -> case parse px src of
                           [] -> parse py src
                           xs -> xs)

item :: Parser Char
item = P (\src -> case src of
                    []     -> []
                    (x:xs) -> [(x, xs)])

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

identifier :: Parser String
identifier = (:) <$> sat item isAlpha <*> many (sat item isAlphaNum)

natural :: Parser Int
natural = read <$> some (sat item isDigit)

integer :: Parser Int
integer = ((string "-" $> negate) <|> pure id) <*> natural
