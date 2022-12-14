module ParserLib
(
  SrcPos(..),
  annotate,
  ParseError(..),
  Parser(..),
  parse,
  (<?>),
  nextToken,
  peekToken,
  srcPos,
  etx,
  sat,
  token,
  tokens,
  space,
  inlineSpace,
  natural,
  integer,
  sign
)
where

import SrcPos

import Data.Char
import Data.Maybe
import Data.Functor
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- ERROR MESSAGES

data ParseError = ParseError (Maybe SrcPos) (Maybe String)

instance Show ParseError where
  showsPrec _ (ParseError sp msg) = showString "parse error" . maybe id ((showString " at " .) . shows) sp . showString ": "
                                  . showString (fromMaybe "no further information available" msg)

instance Semigroup ParseError where
  x@(ParseError spx dx) <> y@(ParseError spy dy) = case (spx, spy) of
                                                     (Nothing, _)     -> y
                                                     (_, Nothing)     -> x
                                                     (Just a, Just b) -> case compare a b of
                                                                           LT -> y
                                                                           EQ -> ParseError spx (dy <|> dx)
                                                                           GT -> x

-- THE PARSER TYPE

newtype Parser t a = Parser { runParser :: StateT [(SrcPos, t)] (Either ParseError) a }

parse :: Parser t a -> [(SrcPos, t)] -> Either ParseError (a, [(SrcPos, t)])
parse = runStateT . runParser

instance Functor (Parser t) where
  fmap f (Parser px) = Parser (fmap f px)

instance Applicative (Parser t) where
  pure x                  = Parser (pure x)
  Parser pf <*> Parser px = Parser (pf <*> px)

instance Monad (Parser t) where
  Parser px >>= f = Parser (px >>= runParser . f)

instance Alternative (Parser t) where
  empty     = do sp <- fmap fst <$> peek
                 Parser (lift (Left (ParseError sp Nothing)))
  px <|> py = do Parser (do src <- get
                            case parse px src of
                              Left ex       -> case parse py src of
                                                 Left ey       -> lift (Left (ex <> ey))
                                                 Right (y, sy) -> put sy *> lift (Right y)
                              Right (x, sx) -> put sx *> lift (Right x))

(<?>) :: Parser t a -> String -> Parser t a
px <?> msg = Parser (do src <- get
                        case parse px src of
                          Left (ParseError spx _) -> lift (Left (ParseError spx (Just msg)))
                          Right (x, sx)           -> put sx *> lift (Right x))

-- PARSER PRIMITIVES

next :: Parser t (SrcPos, t)
next = do src <- Parser get
          case src of
            []     -> empty
            (x:xs) -> Parser (put xs $> x)

nextToken :: Parser t t
nextToken = snd <$> next

peek :: Parser t (Maybe (SrcPos, t))
peek = listToMaybe <$> Parser get

peekToken :: Parser t (Maybe t)
peekToken = fmap snd <$> peek

srcPos :: Parser t SrcPos
srcPos = peek >>= maybe empty (pure . fst)

etx :: Parser t ()
etx = peek >>= maybe (pure ()) (const empty)

sat :: Parser t a -> (a -> Bool) -> Parser t a
sat px f = do x <- px
              if f x
                then pure x
                else empty

token :: Eq t => t -> Parser t t
token t = sat peekToken (== Just t) *> nextToken

tokens :: Eq t => [t] -> Parser t [t]
tokens = traverse token

space :: Parser Char Char
space = sat nextToken isSpace

inlineSpace :: Parser Char Char
inlineSpace = sat space (/= '\n')

natural :: Parser Char Int
natural = read <$> some (sat nextToken isDigit)

integer :: Parser Char Int
integer = (sign <|> pure id) <*> natural

sign :: Parser Char (Int -> Int)
sign = token '+' $> id <|> token '-' $> negate
