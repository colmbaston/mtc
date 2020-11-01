module CodeGen (codeGen) where

import AST
import TAM

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Foldable
import           Data.Bifunctor

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except

data CodeGenError = CodeGenError Identifier ErrorDetails
data ErrorDetails = Undeclared | Multiple

instance Show CodeGenError where
  showsPrec _ (CodeGenError (Identifier sp v) d) = showString "code generation error at " . shows sp
                                                 . showString ": variable " . showString v
                                                 . showString (case d of
                                                                 Undeclared -> " is undeclared"
                                                                 Multiple   -> " is declared multiple times")

newtype DList a = DList { runDList :: [a] -> [a] }

instance Semigroup (DList a) where
  DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
  mempty = DList id

type Environment = Map String Address
type CodeGen m   = WriterT (DList TAM) (StateT (Int, Environment) (ExceptT CodeGenError m))

emitCode :: Monad m => [TAM] -> CodeGen m ()
emitCode is = tell (DList (is ++))

emitError :: Monad m => CodeGenError -> CodeGen m a
emitError = lift . lift . throwE

freshLabel :: Monad m => CodeGen m Label
freshLabel = lift (state (\(l, m) -> (show l, (l+1, m))))

setAddress :: Monad m => Identifier -> Address -> CodeGen m ()
setAddress i@(Identifier _ v) a = do env <- snd <$> lift get
                                     case M.insertLookupWithKey (\_ x _ -> x) v a env of
                                       (Nothing, env') -> lift (modify (second (const env')))
                                       (Just  _,    _) -> emitError (CodeGenError i Multiple)

getAddress :: Monad m => Identifier -> CodeGen m Address
getAddress i@(Identifier _ v) = do env <- snd <$> lift get
                                   case M.lookup v env of
                                     Nothing -> emitError (CodeGenError i Undeclared)
                                     Just  a -> pure a

load :: Monad m => Identifier -> CodeGen m ()
load i = getAddress i >>= emitCode . pure . LOAD

store :: Monad m => Identifier -> CodeGen m ()
store i = getAddress i >>= emitCode . pure . STORE

codeGen :: Program -> Either CodeGenError [TAM]
codeGen p = ($ []) . runDList <$> runExcept (evalStateT (execWriterT (codeGenProg p)) (0, M.empty))

codeGenProg :: Monad m => Program -> CodeGen m ()
codeGenProg (Program ds c) = codeGenDecls ds *> codeGenCommand c *> emitCode [HALT]

codeGenDecls :: Monad m => [Declaration] -> CodeGen m ()
codeGenDecls = traverse_ codeGenDecl . zip [0..]

codeGenDecl :: Monad m => (Address, Declaration) -> CodeGen m ()
codeGenDecl (a, Initialise i e) = codeGenExpr e *> setAddress i a

codeGenCommand :: Monad m => Command -> CodeGen m ()
codeGenCommand (Assign i e) =    codeGenExpr e *> store i
codeGenCommand (If e t f)   = do l1 <- freshLabel
                                 l2 <- freshLabel
                                 codeGenExpr e
                                 emitCode [JUMPIFZ l1]
                                 codeGenCommand t
                                 emitCode [JUMP l2, LABEL l1]
                                 codeGenCommand f
                                 emitCode [LABEL l2]
codeGenCommand (While e t)  = do l1 <- freshLabel
                                 l2 <- freshLabel
                                 emitCode [LABEL l1]
                                 codeGenExpr e
                                 emitCode [JUMPIFZ l2]
                                 codeGenCommand t
                                 emitCode [JUMP l1, LABEL l2]
codeGenCommand (GetInt i)   =    emitCode [GETINT] *> store i
codeGenCommand (PrintInt e) =    codeGenExpr e *> emitCode [PUTINT]
codeGenCommand (Block cs)   =    traverse_ codeGenCommand cs

codeGenExpr :: Monad m => Expr -> CodeGen m ()
codeGenExpr (Literal   n)                 =    emitCode [LOADL n]
codeGenExpr (Variable  i)                 =    load i
codeGenExpr (UnaryOp   op x)              =    codeGenExpr x *>                  codeGenUnOp  op
codeGenExpr (BinaryOp  op x y)            =    codeGenExpr x *> codeGenExpr y *> codeGenBinOp op
codeGenExpr (TernaryOp Conditional x y z) = do x' <- lift (execWriterT (codeGenExpr x))
                                               tell x'
                                               emitCode [NOT, NOT]
                                               codeGenExpr y
                                               emitCode [MUL]
                                               tell x'
                                               emitCode [NOT]
                                               codeGenExpr z
                                               emitCode [MUL, ADD]

codeGenUnOp :: Monad m => UnaryOp -> CodeGen m ()
codeGenUnOp IntegerNegation = emitCode [NEG]
codeGenUnOp BooleanNegation = emitCode [NOT]

codeGenBinOp :: Monad m => BinaryOp -> CodeGen m ()
codeGenBinOp Addition       = emitCode [ADD]
codeGenBinOp Subtraction    = emitCode [SUB]
codeGenBinOp Multiplication = emitCode [MUL]
codeGenBinOp Division       = emitCode [DIV]
codeGenBinOp Conjunction    = emitCode [AND]
codeGenBinOp Disjunction    = emitCode [OR]
codeGenBinOp Equal          = emitCode [EQL]
codeGenBinOp NotEqual       = emitCode [EQL, NOT]
codeGenBinOp Less           = emitCode [LSS]
codeGenBinOp LessEqual      = emitCode [GTR, NOT]
codeGenBinOp Greater        = emitCode [GTR]
codeGenBinOp GreaterEqual   = emitCode [LSS, NOT]
