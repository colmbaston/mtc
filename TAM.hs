module TAM (TAM(..), Label, formatTAM, exec, parseTAM, optimiseTAM) where

import           Data.Char
import           Data.Maybe
import           Data.Foldable
import           Data.Functor
import           Data.Bifunctor
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import           Data.Array

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe

import Parser
import System.IO

type Label = String
data TAM   = LOADL Int
           | ADD
           | SUB
           | MUL
           | DIV
           | NEG
           | AND
           | OR
           | NOT
           | EQL
           | LSS
           | GTR
           | GETINT
           | PUTINT
           | LABEL   Label
           | JUMP    Label
           | JUMPIFZ Label
           | LOAD    Int
           | STORE   Int
           | HALT
           deriving Eq

-- PRINTING AND PARSING TAM CODE

formatTAM :: [TAM] -> String
formatTAM = unlines . map formatInst

formatInst :: TAM -> String
formatInst (LOADL    n) = "  LOADL   " ++ show n
formatInst  ADD         = "  ADD"
formatInst  SUB         = "  SUB"
formatInst  MUL         = "  MUL"
formatInst  DIV         = "  DIV"
formatInst  NEG         = "  NEG"
formatInst  AND         = "  AND"
formatInst  OR          = "  OR"
formatInst  NOT         = "  NOT"
formatInst  EQL         = "  EQL"
formatInst  LSS         = "  LSS"
formatInst  GTR         = "  GTR"
formatInst  GETINT      = "  GETINT"
formatInst  PUTINT      = "  PUTINT"
formatInst (LABEL    l) = '#' : l ++ ":"
formatInst (JUMP     l) = "  JUMP    " ++ '#' : l
formatInst (JUMPIFZ  l) = "  JUMPIFZ " ++ '#' : l
formatInst (LOAD     a) = "  LOAD    " ++ '[' : show a ++ "]"
formatInst (STORE    a) = "  STORE   " ++ '[' : show a ++ "]"
formatInst  HALT        = "  HALT"

parseTAM :: String -> Maybe [TAM]
parseTAM src = case parse (trim code) src of
                 [(is, "")] -> Just is
                 _          -> Nothing

code :: Parser [TAM]
code = (:) <$> inst <*> (many (sat space (/= '\n')) *> newline *> many space *> code) <|> pure []

inst :: Parser TAM
inst = (string "LOADL"  $> LOADL) <*> (some (sat space (/= '\n')) *> integer) <|>
       (string "ADD"    $> ADD)    <|>
       (string "SUB"    $> SUB)    <|>
       (string "MUL"    $> MUL)    <|>
       (string "DIV"    $> DIV)    <|>
       (string "NEG"    $> NEG)    <|>
       (string "AND"    $> AND)    <|>
       (string "OR"     $> OR )    <|>
       (string "NOT"    $> NOT)    <|>
       (string "EQL"    $> EQL)    <|>
       (string "LSS"    $> LSS)    <|>
       (string "GTR"    $> GTR)    <|>
       (string "GETINT" $> GETINT) <|>
       (string "PUTINT" $> PUTINT) <|>
       (LABEL <$> label <* string ":") <|>
       (string "JUMP"    $> JUMP)    <*> (some (sat space (/= '\n')) *> label)   <|>
       (string "JUMPIFZ" $> JUMPIFZ) <*> (some (sat space (/= '\n')) *> label)   <|>
       (string "LOAD"    $> LOAD)    <*> (some (sat space (/= '\n')) *> address) <|>
       (string "STORE"   $> STORE)   <*> (some (sat space (/= '\n')) *> address) <|>
       (string "HALT"    $> HALT)

address :: Parser Int
address = string "[" *> trim natural <* string "]"

label :: Parser Label
label = string "#" *> some (sat item isAlphaNum)

-- EXECUTING TAM CODE

type Machine m = MaybeT (StateT (Int, S.Seq Int) m)

increment :: Monad m => Machine m ()
increment = lift (modify (first (+1)))

push :: Monad m => Int -> Machine m ()
push x = lift (modify (second (x <|)))

pop :: Monad m => Machine m Int
pop = do xs <- snd <$> lift get
         case xs of
           Empty    -> empty
           y :<| ys -> lift (modify (second (const ys))) $> y

unOp :: Monad m => (Int -> Int) -> Machine m ()
unOp op = do x <- pop
             push (op x)
             increment

binOp :: Monad m => (Int -> Int -> Int) -> Machine m ()
binOp op = do x <- pop
              y <- pop
              push (op y x)
              increment

load :: Monad m => Int -> Machine m ()
load a = do xs <- snd <$> lift get
            let l = S.length xs
            maybe empty push (S.lookup (l - 1 - a) xs)
            increment

store :: Monad m => Int -> Machine m ()
store a = do xs <- snd <$> lift get
             let l = S.length xs
             if 0 <= a && a < l
               then pop >>= lift . modify . second . S.update (l - 2 - a)
               else empty
             increment

getInt :: IO Int
getInt = do putStr "GETINT> "
            hFlush stdout
            xs <- getLine
            case parse (trim integer) xs of
              [(n, "")] -> pure n
              _         -> putStrLn "could not parse as integer" *> getInt

type JumpTable = Map Label Int

jumpTable :: Int -> JumpTable -> [TAM] -> (JumpTable, [TAM])
jumpTable _ js []             = (js, [])
jumpTable n js (LABEL l : is) =              jumpTable  n (M.insert l n js) is
jumpTable n js (      i : is) = second (i:) (jumpTable (n+1)            js  is)

instArray :: [TAM] -> (Int, Array Int TAM)
instArray is = let l = length is in (l, listArray (0, l-1) is)

exec :: [TAM] -> IO (Maybe [Int])
exec is = (\(m, (_, st)) -> m $> toList st) <$> runStateT (runMaybeT run) (0, S.empty)
  where
    jt  :: JumpTable
    ia  :: Array Int TAM
    len :: Int
    (jt, (len, ia)) = second instArray (jumpTable 0 M.empty is)

    run :: MonadIO m => Machine m ()
    run = do pc <- fst <$> lift get
             if pc < 0 || pc >= len
               then empty
               else case ia ! pc of
                      HALT -> pure ()
                      i    -> step i *> run

    step :: MonadIO m => TAM -> Machine m ()
    step (LOADL n)   = push n *> increment
    step  ADD        = binOp (+)
    step  SUB        = binOp (-)
    step  MUL        = binOp (*)
    step  DIV        = do x <- pop
                          y <- pop
                          if x == 0
                            then empty
                            else push (y `div` x)
                          increment
    step  NEG        = unOp negate
    step  AND        = binOp (\a b -> fromEnum (a /= 0 && b /= 0))
    step  OR         = binOp (\a b -> fromEnum (a /= 0 || b /= 0))
    step  NOT        = unOp  (\a   -> fromEnum (a == 0))
    step  EQL        = binOp (\x y -> fromEnum (x == y))
    step  LSS        = binOp (\x y -> fromEnum (x <  y))
    step  GTR        = binOp (\x y -> fromEnum (x >  y))
    step  GETINT     = (liftIO getInt >>= push) *> increment
    step  PUTINT     = (pop >>= liftIO . print) *> increment
    step (JUMP l)    = case M.lookup l jt of
                         Nothing -> empty
                         Just pc -> lift (modify (first (const pc)))
    step (JUMPIFZ l) = do x <- pop
                          if x == 0
                            then step (JUMP l)
                            else increment
    step (LOAD  a)   = load  a
    step (STORE a)   = store a

    step (LABEL _)   = error "step called on LABEL"
    step  HALT       = error "step called on HALT"

-- OPTIMISING TAM CODE

optimiseTAM :: [TAM] -> [TAM]
optimiseTAM = fixedPoint (aliasLabels . peephole)
  where
    fixedPoint :: Eq a => (a -> a) -> a -> a
    fixedPoint f x = let y = f x in if x == y then y else fixedPoint f y

    peephole :: [TAM] -> [TAM]
    peephole []                                     = []
    peephole (LOAD  a : STORE   b : xs) | a == b    =           peephole xs
    peephole (JUMP  a : LABEL   b : xs) | a == b    = LABEL b : peephole xs
    peephole (LOADL a : JUMPIFZ b : xs) | a == 0    = JUMP  b : peephole xs
                                        | otherwise =           peephole xs
    peephole (x:xs)                                 =       x : peephole xs

    aliasLabels :: [TAM] -> [TAM]
    aliasLabels is = foldr (mapMaybe . uncurry relabel) is (labelPairs (zip is (tail is)))
      where
        labelPairs :: [(TAM, TAM)] -> [(Label, Label)]
        labelPairs []                        = []
        labelPairs ((LABEL a, LABEL b) : ls) = (a, b) : labelPairs ls
        labelPairs ((LABEL a, JUMP  b) : ls) = (a, b) : labelPairs ls
        labelPairs (                 _ : ls) =          labelPairs ls

        relabel :: Label -> Label -> TAM -> Maybe TAM
        relabel a _ (LABEL    c) | c == a = Nothing
        relabel a b (JUMP     c) | c == a = Just (JUMP    b)
        relabel a b (JUMPIFZ  c) | c == a = Just (JUMPIFZ b)
        relabel _ _  i                    = Just  i