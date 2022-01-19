module TAM
(
  Address(..),
  TAM(..),
  Memory(..),
  formatTAM,
  parseTAM,
  exec,
  execWithMem,
  defaultMem,
  optimiseTAM
)
where

import ParserLib

import           Data.Char
import           Data.Maybe
import           Data.Functor
import           Data.Map      (Map)
import qualified Data.Map      as M
import           Data.Set      (Set)
import qualified Data.Set      as Set
import           Data.Sequence (Seq(..), (|>))
import qualified Data.Sequence as Seq
import           Data.Array

import System.IO
import Control.Monad
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

data Address  = Abs Int
              | SB  Int
              | LB  Int
              deriving (Eq, Show)

data TAM      = LOADL Int
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
              | LABEL   String
              | JUMP    String
              | JUMPIFZ String
              | LOAD    Address
              | STORE   Address
              | CALL    String
              | RETURN  Int Int
              | HALT
              deriving Eq

-- FORMATTING TAM CODE

formatTAM :: [TAM] -> String
formatTAM = unlines . map formatInst

formatInst :: TAM -> String
formatInst (LOADL    n) = "LOADL   " ++ show n
formatInst  ADD         = "ADD"
formatInst  SUB         = "SUB"
formatInst  MUL         = "MUL"
formatInst  DIV         = "DIV"
formatInst  NEG         = "NEG"
formatInst  AND         = "AND"
formatInst  OR          = "OR"
formatInst  NOT         = "NOT"
formatInst  EQL         = "EQL"
formatInst  LSS         = "LSS"
formatInst  GTR         = "GTR"
formatInst  GETINT      = "GETINT"
formatInst  PUTINT      = "PUTINT"
formatInst (LABEL    l) = "Label   " ++ l
formatInst (JUMP     l) = "JUMP    " ++ l
formatInst (JUMPIFZ  l) = "JUMPIFZ " ++ l
formatInst (LOAD     a) = "LOAD    " ++ formatAddr a
formatInst (STORE    a) = "STORE   " ++ formatAddr a
formatInst (CALL     l) = "CALL    " ++ l
formatInst (RETURN m n) = "RETURN  " ++ show m ++ ' ' : show n
formatInst  HALT        = "HALT"

formatAddr :: Address -> String
formatAddr a = '[' : go ++ "]"
  where
    go :: String
    go = case a of
           Abs d ->         show   d
           SB  d -> "SB" ++ signed d
           LB  d -> "LB" ++ signed d

    signed :: Int -> String
    signed n = (if n >= 0 then '+' else '-') : show (abs n)

-- PARSING TAM CODE

parseTAM :: String -> Either ParseError [TAM]
parseTAM = fmap fst . parse ((many space *> code <* many space <* token '\ETX' <* etx) <?> "unrecognised TAM instruction") . annotate

code :: Parser Char [TAM]
code = (:) <$> inst <*> (many inlineSpace *> token '\n' *> many space *> code) <|> pure []

inst :: Parser Char TAM
inst =  (LOADL   <$ tokens "LOADL")   <*> (some inlineSpace *> integer)
    <|> (ADD     <$ tokens "ADD")
    <|> (SUB     <$ tokens "SUB")
    <|> (MUL     <$ tokens "MUL")
    <|> (DIV     <$ tokens "DIV")
    <|> (NEG     <$ tokens "NEG")
    <|> (AND     <$ tokens "AND")
    <|> (OR      <$ tokens "OR")
    <|> (NOT     <$ tokens "NOT")
    <|> (EQL     <$ tokens "EQL")
    <|> (LSS     <$ tokens "LSS")
    <|> (GTR     <$ tokens "GTR")
    <|> (GETINT  <$ tokens "GETINT")
    <|> (PUTINT  <$ tokens "PUTINT")
    <|> (LABEL   <$ tokens "Label")   <*> (some inlineSpace *> label)
    <|> (JUMP    <$ tokens "JUMP")    <*> (some inlineSpace *> label)
    <|> (JUMPIFZ <$ tokens "JUMPIFZ") <*> (some inlineSpace *> label)
    <|> (LOAD    <$ tokens "LOAD")    <*> (some inlineSpace *> address)
    <|> (STORE   <$ tokens "STORE")   <*> (some inlineSpace *> address)
    <|> (CALL    <$ tokens "CALL")    <*> (some inlineSpace *> label)
    <|> (RETURN  <$ tokens "RETURN")  <*> (some inlineSpace *> natural) <*> (some inlineSpace *> natural)
    <|> (HALT    <$ tokens "HALT")

address :: Parser Char Address
address = token '[' *> many inlineSpace *> (register <|> Abs <$> natural) <* many inlineSpace <* token ']'

register :: Parser Char Address
register = (tokens "SB" $> SB <|> tokens "LB" $> LB) <*> (many inlineSpace *> sign <*> (many inlineSpace *> natural))

label :: Parser Char String
label = some (sat nextToken (not . isSpace))

-- EXECUTION MONAD

data ExecError    = ExecError Int ErrorDetails
data ErrorDetails = InvalidAddress Address
                  | InvalidLabel   String
                  | BufferOverrun
                  | StackUnderflow
                  | DivZero

instance Show ExecError where
  showsPrec _ (ExecError pc e) = showString "execution error at instruction " . shows pc . showString ": "
                               . case e of
                                   InvalidAddress a -> showString "invalid address " . showString (formatAddr a)
                                   InvalidLabel   l -> showString "invalid label "   . showString l
                                   BufferOverrun    -> showString "overran the instruction buffer"
                                   StackUnderflow   -> showString "stack underflow"
                                   DivZero          -> showString "division by zero"

type Stack     = Seq Int
data Memory    = Memory { programCounter :: Int, stackBase :: Int, localBase :: Int, stack :: Stack }
type Machine m = StateT Memory (ExceptT ExecError m)

stackPointer :: Memory -> Int
stackPointer = Seq.length . stack

emitError :: Monad m => ErrorDetails -> Machine m a
emitError e = do pc <- programCounter <$> get
                 lift (throwE (ExecError pc e))

increment :: Monad m => Machine m ()
increment = modify (\m -> m { programCounter = programCounter m + 1 })

push :: Monad m => Int -> Machine m ()
push x = modify (\m -> m { stack = stack m |> x })

pop :: Monad m => Machine m Int
pop = do xs <- stack <$> get
         case xs of
           Empty    -> emitError StackUnderflow
           ys :|> y -> modify (\m -> m { stack = ys }) $> y

unOp :: Monad m => (Int -> Int) -> Machine m ()
unOp op = pop >>= push . op >> increment

binOp :: Monad m => (Int -> Int -> Int) -> Machine m ()
binOp op = flip op <$> pop <*> pop >>= push >> increment

absAddress :: Monad m => Address -> Machine m Int
absAddress a = do m <- get
                  let i = case a of
                            Abs d -> d
                            SB  d -> d + stackBase m
                            LB  d -> d + localBase m
                  if 0 <= i && i < stackPointer m
                     then pure i
                     else emitError (InvalidAddress a)

load :: Monad m => Address -> Machine m ()
load a = do i <- absAddress a
            s <- stack <$> get
            case Seq.lookup i s of
              Nothing -> emitError (InvalidAddress a)
              Just  x -> push x
            increment

store :: Monad m => Address -> Machine m ()
store a = do x <- pop
             i <- absAddress a
             modify (\m -> m { stack = Seq.update i x (stack m) })
             increment

jump :: Monad m => String -> JumpTable -> Machine m ()
jump l jt = case M.lookup l jt of
              Nothing -> emitError (InvalidLabel l)
              Just pc -> modify (\m -> m { programCounter = pc })

frameSize :: Monad m => Machine m Int
frameSize = (\m -> stackPointer m - localBase m) <$> get

getInt :: MonadIO m => Machine m Int
getInt = do xs <- liftIO (putStr "input> " *> hFlush stdout *> getLine)
            case fst <$> parse (many space *> integer <* many space <* token '\ETX' <* etx) (annotate xs) of
              Left  _ -> liftIO (putStrLn "could not parse as integer") >> getInt
              Right n -> pure n

-- EXECUTING TAM CODE

type JumpTable = Map String Int

jumpTable :: Int -> JumpTable -> [TAM] -> (JumpTable, [TAM])
jumpTable _ js []             = (js, [])
jumpTable n js (LABEL l : is) =            jumpTable  n (M.insert l n js) is
jumpTable n js (      i : is) = fmap (i:) (jumpTable (n+1)            js  is)

instArray :: [TAM] -> (Int, Array Int TAM)
instArray is = let l = length is in (l, listArray (0, l-1) is)

defaultMem :: Memory
defaultMem = Memory { programCounter = 0, stackBase = 0, localBase = 0, stack = Seq.empty }

exec :: [TAM] -> IO (Either ExecError Stack)
exec = execWithMem defaultMem

execWithMem :: Memory -> [TAM] -> IO (Either ExecError Stack)
execWithMem mem is = fmap stack <$> runExceptT (execStateT run mem)
  where
    jt  :: JumpTable
    ia  :: Array Int TAM
    len :: Int
    (jt, (len, ia)) = instArray <$> jumpTable 0 M.empty is

    run :: MonadIO m => Machine m ()
    run = do pc <- programCounter <$> get
             if 0 <= pc && pc < len
               then case ia ! pc of
                      HALT -> liftIO (putStrLn "TAM halted")
                      i    -> step i >> run
               else emitError BufferOverrun

    step :: MonadIO m => TAM -> Machine m ()
    step (LOADL n)    = push n >> increment
    step  ADD         = binOp (+)
    step  SUB         = binOp (-)
    step  MUL         = binOp (*)
    step  DIV         = do x <- pop
                           y <- pop
                           if x == 0
                             then emitError DivZero
                             else push (y `quot` x) >> increment
    step  NEG         = unOp negate
    step  AND         = binOp (\a b -> fromEnum (a /= 0 && b /= 0))
    step  OR          = binOp (\a b -> fromEnum (a /= 0 || b /= 0))
    step  NOT         = unOp  (\a   -> fromEnum (a == 0))
    step  EQL         = binOp (\x y -> fromEnum (x == y))
    step  LSS         = binOp (\x y -> fromEnum (x <  y))
    step  GTR         = binOp (\x y -> fromEnum (x >  y))
    step  GETINT      = getInt >>= push >> increment
    step  PUTINT      = pop >>= liftIO . print >> increment
    step (JUMP l)     = jump l jt
    step (JUMPIFZ l)  = pop >>= \x -> if x == 0 then jump l jt else increment
    step (LOAD  a)    = load  a
    step (STORE a)    = store a
    step (CALL l)     = do sp <- stackPointer <$> get
                           get >>= push .        localBase
                           get >>= push . (+1) . programCounter
                           modify (\m -> m { localBase = sp })
                           jump l jt
    step (RETURN s a) = do xs <- Seq.replicateA s pop
                           fs <- frameSize
                           replicateM_ (fs - 2) pop
                           ra <- pop
                           lb <- pop
                           replicateM_ a pop
                           mapM_ push (Seq.reverse xs)
                           modify (\m -> m { programCounter = ra, localBase = lb })
    step (LABEL _)    = increment
    step  HALT        = pure ()

-- OPTIMISING TAM CODE

optimiseTAM :: [TAM] -> [TAM]
optimiseTAM = fixedPoint (cullLabels . mergeLabels . peephole)
  where
    fixedPoint :: Eq a => (a -> a) -> a -> a
    fixedPoint f x = let y = f x in if x == y then x else fixedPoint f y

    peephole :: [TAM] -> [TAM]
    peephole []                                               =                         []
    peephole (LOAD  a : STORE   b : xs)           | a == b    =     peephole            xs
    peephole (JUMP  a : LABEL   b : xs)           | a == b    =     peephole (LABEL b : xs)
    peephole (LOADL a : JUMPIFZ b : xs)           | a == 0    =     peephole (JUMP  b : xs)
                                                  | otherwise =     peephole            xs
    peephole (LOAD _  : JUMPIFZ a : LABEL b : xs) | a == b    =     peephole (LABEL b : xs)
    peephole (x       : NEG       : NEG     : xs)             =     peephole (x       : xs)
    peephole (x       : NOT       : NOT     : xs) | boolOp x  =     peephole (x       : xs)
    peephole (x                             : xs)             = x : peephole            xs

    mergeLabels :: [TAM] -> [TAM]
    mergeLabels = fixedPoint (\is -> foldr (mapMaybe . uncurry relabel) is (alias (zip is (tail is))))
      where
        alias :: [(TAM, TAM)] -> Maybe (String, String)
        alias []                        = Nothing
        alias ((LABEL a, LABEL b) :  _) = Just (a, b)
        alias ((LABEL a, JUMP  b) :  _) = Just (a, b)
        alias                  (_ : ls) = alias ls

        relabel :: String -> String -> TAM -> Maybe TAM
        relabel a _ (LABEL    c) | c == a = Nothing
        relabel a b (JUMP     c) | c == a = Just (JUMP    b)
        relabel a b (JUMPIFZ  c) | c == a = Just (JUMPIFZ b)
        relabel _ _  i                    = Just  i

    cullLabels :: [TAM] -> [TAM]
    cullLabels is = let ls = M.keysSet (M.filter not (foldr referenced M.empty is)) in foldr (remove ls) [] is
      where
        referenced :: TAM -> Map String Bool -> Map String Bool
        referenced (JUMP    l) m = M.insert          l True  m
        referenced (JUMPIFZ l) m = M.insert          l True  m
        referenced (CALL    l) m = M.insert          l True  m
        referenced (LABEL   l) m = M.insertWith (||) l False m
        referenced  _          m = m

        remove :: Set String -> TAM -> [TAM] -> [TAM]
        remove s (LABEL l) js | l `Set.member` s =     js
        remove _  i        js                    = i : js

    boolOp :: TAM -> Bool
    boolOp AND = True
    boolOp OR  = True
    boolOp NOT = True
    boolOp EQL = True
    boolOp LSS = True
    boolOp GTR = True
    boolOp _   = False
