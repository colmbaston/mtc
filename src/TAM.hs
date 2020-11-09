module TAM (Label, Address, TAM(..), formatTAM, Stack, exec, execWithStack, parseTAM, optimiseTAM) where

import           Data.Char
import           Data.Maybe
import           Data.Functor
import           Data.Bifunctor
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import           Data.Array

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

import Parser
import System.IO

type Label   = String
type Address = Int
data TAM     = LOADL Int
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
             | LOAD    Address
             | STORE   Address
             | HALT
             deriving Eq

-- FORMATTING TAM CODE

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

-- PARSING TAM CODE

parseTAM :: String -> Either ParseError [TAM]
parseTAM = fmap fst . parse ((trim code <* token '\ETX' <* etx) <?> "unrecognised TAM instruction") . annotate

code :: Parser Char [TAM]
code = (:) <$> inst <*> (many (sat space (/= '\n')) *> sat nextToken (== '\n') *> many space *> code) <|> pure []

inst :: Parser Char TAM
inst =  (tokens "LOADL"   $> LOADL) <*> (some (sat space (/= '\n')) *> integer)
    <|> (tokens "ADD"     $> ADD)
    <|> (tokens "SUB"     $> SUB)
    <|> (tokens "MUL"     $> MUL)
    <|> (tokens "DIV"     $> DIV)
    <|> (tokens "NEG"     $> NEG)
    <|> (tokens "AND"     $> AND)
    <|> (tokens "OR"      $> OR )
    <|> (tokens "NOT"     $> NOT)
    <|> (tokens "EQL"     $> EQL)
    <|> (tokens "LSS"     $> LSS)
    <|> (tokens "GTR"     $> GTR)
    <|> (tokens "GETINT"  $> GETINT)
    <|> (tokens "PUTINT"  $> PUTINT)
    <|> (LABEL <$> label <*  token ':')
    <|> (tokens "JUMP"    $> JUMP)    <*> (some (sat space (/= '\n')) *> label)
    <|> (tokens "JUMPIFZ" $> JUMPIFZ) <*> (some (sat space (/= '\n')) *> label)
    <|> (tokens "LOAD"    $> LOAD)    <*> (some (sat space (/= '\n')) *> address)
    <|> (tokens "STORE"   $> STORE)   <*> (some (sat space (/= '\n')) *> address)
    <|> (tokens "HALT"    $> HALT)

address :: Parser Char Address
address = token '[' *> trim natural <* token ']'

label :: Parser Char Label
label = token '#' *> some (sat nextToken isAlphaNum)

-- EXECUTING TAM CODE

data ExecError    = ExecError Int ErrorDetails
data ErrorDetails = InvalidAddress Address
                  | InvalidLabel   Label
                  | BufferOverrun
                  | StackUnderflow
                  | DivZero

instance Show ExecError where
  showsPrec _ (ExecError pc e) = showString "execution error at instruction " . shows pc . showString ": "
                               . case e of
                                   InvalidAddress a -> showString "invalid address [" . shows a . showChar ']'
                                   InvalidLabel   l -> showString "invalid label #" . showString l
                                   BufferOverrun    -> showString "overran the instruction buffer"
                                   StackUnderflow   -> showString "stack underflow"
                                   DivZero          -> showString "division by zero"

type Stack     = Seq Int
type Machine m = StateT (Address, Stack) (ExceptT ExecError m)

emitError :: Monad m => ErrorDetails -> Machine m a
emitError e = do pc <- fst <$> get
                 lift (throwE (ExecError pc e))

increment :: Monad m => Machine m ()
increment = modify (first (+1))

push :: Monad m => Int -> Machine m ()
push x = modify (second (|> x))

pop :: Monad m => Machine m Int
pop = do xs <- snd <$> get
         case xs of
           Empty    -> emitError StackUnderflow
           ys :|> y -> modify (second (const ys)) $> y

unOp :: Monad m => (Int -> Int) -> Machine m ()
unOp op = pop >>= push . op >> increment

binOp :: Monad m => (Int -> Int -> Int) -> Machine m ()
binOp op = flip op <$> pop <*> pop >>= push >> increment

load :: Monad m => Address -> Machine m ()
load a = do xs <- snd <$> get
            case Seq.lookup a xs of
              Nothing -> emitError (InvalidAddress a)
              Just  x -> push x >> increment

store :: Monad m => Address -> Machine m ()
store a = do xs <- snd <$> get
             if 0 <= a && a < Seq.length xs
               then pop >>= modify . second . Seq.update a >> increment
               else emitError (InvalidAddress a)

getInt :: MonadIO m => Machine m Int
getInt = do xs <- liftIO (putStr "GETINT> " >> hFlush stdout >> getLine)
            case fst <$> parse (trim integer <* token '\ETX' <* etx) (annotate xs) of
              Left  _ -> liftIO (putStrLn "could not parse as integer") >> getInt
              Right n -> pure n

type JumpTable = Map Label Int

jumpTable :: Int -> JumpTable -> [TAM] -> (JumpTable, [TAM])
jumpTable _ js []             = (js, [])
jumpTable n js (LABEL l : is) =              jumpTable  n (M.insert l n js) is
jumpTable n js (      i : is) = second (i:) (jumpTable (n+1)            js  is)

instArray :: [TAM] -> (Int, Array Int TAM)
instArray is = let l = length is in (l, listArray (0, l-1) is)

exec :: [TAM] -> IO (Either ExecError Stack)
exec = execWithStack Seq.empty

execWithStack :: Stack -> [TAM] -> IO (Either ExecError Stack)
execWithStack xs is = fmap snd <$> runExceptT (execStateT run (0, xs))
  where
    jt  :: JumpTable
    ia  :: Array Int TAM
    len :: Int
    (jt, (len, ia)) = second instArray (jumpTable 0 M.empty is)

    run :: MonadIO m => Machine m ()
    run = do pc <- fst <$> get
             if 0 <= pc && pc < len
               then case ia ! pc of
                      HALT -> liftIO (putStrLn "HALTED")
                      i    -> step i >> run
               else emitError BufferOverrun

    step :: MonadIO m => TAM -> Machine m ()
    step (LOADL n)   = push n >> increment
    step  ADD        = binOp (+)
    step  SUB        = binOp (-)
    step  MUL        = binOp (*)
    step  DIV        = do x <- pop
                          y <- pop
                          if x == 0
                            then emitError DivZero
                            else push (y `div` x) >> increment
    step  NEG        = unOp negate
    step  AND        = binOp (\a b -> fromEnum (a /= 0 && b /= 0))
    step  OR         = binOp (\a b -> fromEnum (a /= 0 || b /= 0))
    step  NOT        = unOp  (\a   -> fromEnum (a == 0))
    step  EQL        = binOp (\x y -> fromEnum (x == y))
    step  LSS        = binOp (\x y -> fromEnum (x <  y))
    step  GTR        = binOp (\x y -> fromEnum (x >  y))
    step  GETINT     = getInt >>= push >> increment
    step  PUTINT     = pop >>= liftIO . print >> increment
    step (JUMP l)    = maybe (emitError (InvalidLabel l)) (modify . first . const) (M.lookup l jt)
    step (JUMPIFZ l) = pop >>= \x -> if x == 0 then step (JUMP l) else increment
    step (LOAD  a)   = load  a
    step (STORE a)   = store a
    step (LABEL _)   = error "should be unreachable: step called on LABEL"
    step  HALT       = error "should be unreachable: step called on HALT"

-- OPTIMISING TAM CODE

optimiseTAM :: [TAM] -> [TAM]
optimiseTAM = fixedPoint (cullLabels . mergeLabels . peephole)
  where
    fixedPoint :: Eq a => (a -> a) -> a -> a
    fixedPoint f x = let y = f x in if x == y then x else fixedPoint f y

    peephole :: [TAM] -> [TAM]
    peephole []                                               = []
    peephole (LOAD  a : STORE   b : xs)           | a == b    =     peephole            xs
    peephole (JUMP  a : LABEL   b : xs)           | a == b    =     peephole (LABEL b : xs)
    peephole (LOADL a : JUMPIFZ b : xs)           | a == 0    =     peephole (JUMP  b : xs)
                                                  | otherwise =     peephole            xs
    peephole (LOAD a  : JUMPIFZ b : LABEL c : xs) | b == c    =     peephole (LABEL c : xs)
    peephole (x : NEG       : NEG     : xs)                   =     peephole (x       : xs)
    peephole (x : NOT       : NOT     : xs)       | boolOp x  =     peephole (x       : xs)
    peephole (x                       : xs)                   = x : peephole            xs

    mergeLabels :: [TAM] -> [TAM]
    mergeLabels = fixedPoint (\is -> foldr (mapMaybe . uncurry relabel) is (alias (zip is (tail is))))
      where
        alias :: [(TAM, TAM)] -> Maybe (Label, Label)
        alias []                        = Nothing
        alias ((LABEL a, LABEL b) :  _) = Just (a, b)
        alias ((LABEL a, JUMP  b) :  _) = Just (a, b)
        alias                  (_ : ls) = alias ls

        relabel :: Label -> Label -> TAM -> Maybe TAM
        relabel a _ (LABEL    c) | c == a = Nothing
        relabel a b (JUMP     c) | c == a = Just (JUMP    b)
        relabel a b (JUMPIFZ  c) | c == a = Just (JUMPIFZ b)
        relabel _ _  i                    = Just  i

    cullLabels :: [TAM] -> [TAM]
    cullLabels is = let ls = M.keysSet (M.filter not (foldr referenced M.empty is)) in foldr (remove ls) [] is
      where
        referenced :: TAM -> Map Label Bool -> Map Label Bool
        referenced (JUMP    l) m = M.insert          l True  m
        referenced (JUMPIFZ l) m = M.insert          l True  m
        referenced (LABEL   l) m = M.insertWith (||) l False m
        referenced  _          m = m

        remove :: Set Label -> TAM -> [TAM] -> [TAM]
        remove s (LABEL l) js | l `Set.member` s = js
        remove _  i        js                    = i : js

    loadOp :: TAM -> Bool
    loadOp (LOADL _) = True
    loadOp (LOAD  _) = True
    loadOp  _        = False

    boolOp :: TAM -> Bool
    boolOp AND = True
    boolOp OR  = True
    boolOp NOT = True
    boolOp EQL = True
    boolOp LSS = True
    boolOp GTR = True
    boolOp _   = False
