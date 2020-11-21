module CodeGen (codeGen) where

import AST
import TAM

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Bifunctor

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Except

data CodeGenError = CodeGenError

instance Show CodeGenError where
  show CodeGenError = "code generation error: this indicates that an error was missed by the type-checker"

newtype DList a = DList { runDList :: [a] -> [a] }

instance Semigroup (DList a) where
  DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
  mempty = DList id

-- CODE GENERATION MONAD

type Environment = Map String Address
type CodeGen m   = WriterT (DList TAM) (StateT (Int, Environment) (ExceptT CodeGenError m))

emitCode :: Monad m => [TAM] -> CodeGen m ()
emitCode is = tell (DList (is ++))

emitError :: Monad m =>  CodeGen m a
emitError = lift (lift (throwE CodeGenError))

freshLabel :: Monad m => CodeGen m String
freshLabel = lift (state (\(l, m) -> (show l, (l+1, m))))

setAddress :: Monad m => String -> Address -> CodeGen m ()
setAddress v a = do (m, env) <- M.insertLookupWithKey (\_ x _ -> x) v a . snd <$> lift get
                    maybe (lift (modify (second (const env)))) (const emitError) m

getAddress :: Monad m => String -> CodeGen m Address
getAddress v = maybe emitError pure . M.lookup v . snd =<< lift get

load :: Monad m => String -> CodeGen m ()
load v = getAddress v >>= emitCode . pure . LOAD

store :: Monad m => String -> CodeGen m ()
store v = getAddress v >>= emitCode . pure . STORE

-- CODE GENERATOR

codeGen :: Program -> Either CodeGenError [TAM]
codeGen p = ($ []) . runDList <$> runExcept (evalStateT (execWriterT (codeGenProg p)) (0, M.empty))

codeGenProg :: Monad m => Program -> CodeGen m ()
codeGenProg (Program ds c) = mapM_ (uncurry codeGenDecl) (zip [0..] ds) *> codeGenCommand c *> emitCode [HALT]

codeGenDecl :: Monad m => Int -> Declaration -> CodeGen m ()
codeGenDecl d (Initialise _ v _ e) = codeGenExpr e *> setAddress v (SB d)

codeGenCommand :: Monad m => Command -> CodeGen m ()
codeGenCommand (Assign _ v e) =    codeGenExpr e *> store v
codeGenCommand (If e t f)     = do l1 <- freshLabel
                                   l2 <- freshLabel
                                   codeGenExpr e
                                   emitCode [JUMPIFZ l1]
                                   codeGenCommand t
                                   emitCode [JUMP l2, LABEL l1]
                                   codeGenCommand f
                                   emitCode [LABEL l2]
codeGenCommand (While e t)    = do l1 <- freshLabel
                                   l2 <- freshLabel
                                   emitCode [LABEL l1]
                                   codeGenExpr e
                                   emitCode [JUMPIFZ l2]
                                   codeGenCommand t
                                   emitCode [JUMP l1, LABEL l2]
codeGenCommand (GetInt _ v)   =    emitCode [GETINT] *> store v
codeGenCommand (PrintInt e)   =    codeGenExpr e *> emitCode [PUTINT]
codeGenCommand (Block cs)     =    mapM_ codeGenCommand cs

codeGenExpr :: Monad m => Expr -> CodeGen m ()
codeGenExpr (LitInteger _ n)                 =    emitCode [LOADL n]
codeGenExpr (LitBoolean _ b)                 =    emitCode [LOADL (fromEnum b)]
codeGenExpr (Variable   _ v)                 =    load v
codeGenExpr (UnaryOp    _ op x)              =    codeGenExpr x *>                  codeGenUnOp  op
codeGenExpr (BinaryOp   _ op x y)            =    codeGenExpr x *> codeGenExpr y *> codeGenBinOp op
codeGenExpr (TernaryOp  _ Conditional x y z) = do l1 <- freshLabel
                                                  l2 <- freshLabel
                                                  codeGenExpr x
                                                  emitCode [JUMPIFZ l1]
                                                  codeGenExpr y
                                                  emitCode [JUMP l2, LABEL l1]
                                                  codeGenExpr z
                                                  emitCode [LABEL l2]

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
