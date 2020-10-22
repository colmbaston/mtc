module CodeGen (codeGen) where

import AST
import TAM

import Data.Functor
import Data.Bifunctor
import Control.Applicative
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Foldable

import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer

newtype DList a = DList { runDList :: [a] -> [a] }

instance Semigroup (DList a) where
  DList f <> DList g = DList (f . g)

instance Monoid (DList a) where
  mempty = DList id

type Environment = Map Identifier Address
type CodeGen m   = MaybeT (WriterT (DList TAM) (StateT (Label, Environment) m))

emit :: Monad m => [TAM] -> CodeGen m ()
emit is = lift (tell (DList (is ++)))

nextLabel :: Label -> Label
nextLabel ""       = "a"
nextLabel ('z':xs) = 'a'    : nextLabel xs
nextLabel ( x :xs) = succ x :           xs

freshLabel :: Monad m => CodeGen m Label
freshLabel = lift (lift (state (\(l, m) -> (l, (nextLabel l, m)))))

setAddress :: Monad m => Identifier -> Address -> CodeGen m ()
setAddress i a = lift (lift (modify (second (M.insert i a))))

getAddress :: Monad m => Identifier -> CodeGen m Address
getAddress i = lift (lift get) >>= maybe empty pure . M.lookup i . snd

load :: Monad m => Identifier -> CodeGen m ()
load i = getAddress i >>= \a -> emit [LOAD a]

store :: Monad m => Identifier -> CodeGen m ()
store i = getAddress i >>= \a -> emit [STORE a]

codeGen :: Program -> Maybe [TAM]
codeGen p = let (m, dl) = evalState (runWriterT (runMaybeT (codeGenProg p))) ("a", M.empty)
            in m $> runDList dl []

codeGenProg :: Monad m => Program -> CodeGen m ()
codeGenProg (Program ds c) = codeGenDecls ds *> codeGenCommand c *> emit [HALT]

codeGenDecls :: Monad m => [Declaration] -> CodeGen m ()
codeGenDecls = traverse_ codeGenDecl . zip [0..]

codeGenDecl :: Monad m => (Address, Declaration) -> CodeGen m ()
codeGenDecl (a, Initialise i e) = codeGenExpr e *> setAddress i a

codeGenCommand :: Monad m => Command -> CodeGen m ()
codeGenCommand (Assign i e) =    codeGenExpr e *> store i
codeGenCommand (If e t f)   = do l1 <- freshLabel
                                 l2 <- freshLabel
                                 codeGenExpr e
                                 emit [JUMPIFZ l1]
                                 codeGenCommand t
                                 emit [JUMP l2, LABEL l1]
                                 codeGenCommand f
                                 emit [LABEL l2]
codeGenCommand (While e t)  = do l1 <- freshLabel
                                 l2 <- freshLabel
                                 emit [LABEL l1]
                                 codeGenExpr e
                                 emit [JUMPIFZ l2]
                                 codeGenCommand t
                                 emit [JUMP l1, LABEL l2]
codeGenCommand (GetInt i)   =    emit [GETINT] *> store i
codeGenCommand (PrintInt e) =    codeGenExpr e *> emit [PUTINT]
codeGenCommand (Block cs)   =    traverse_ codeGenCommand cs

codeGenExpr :: Monad m => Expr -> CodeGen m ()
codeGenExpr (Literal   n)                 =    emit [LOADL n]
codeGenExpr (Variable  i)                 =    load i
codeGenExpr (UnaryOp   op x)              =    codeGenExpr x *>                  codeGenUnOp  op
codeGenExpr (BinaryOp  op x y)            =    codeGenExpr x *> codeGenExpr y *> codeGenBinOp op
codeGenExpr (TernaryOp Conditional x y z) = do x' <- lift (lift (execWriterT (runMaybeT (codeGenExpr x))))
                                               lift (tell x')
                                               emit [NOT, NOT]
                                               codeGenExpr y
                                               emit [MUL]
                                               lift (tell x')
                                               emit [NOT]
                                               codeGenExpr z
                                               emit [MUL, ADD]

codeGenUnOp :: Monad m => UnaryOp -> CodeGen m ()
codeGenUnOp IntegerNegation = emit [NEG]
codeGenUnOp BooleanNegation = emit [NOT]

codeGenBinOp :: Monad m => BinaryOp -> CodeGen m ()
codeGenBinOp Addition       = emit [ADD]
codeGenBinOp Subtraction    = emit [SUB]
codeGenBinOp Multiplication = emit [MUL]
codeGenBinOp Division       = emit [DIV]
codeGenBinOp Conjunction    = emit [AND]
codeGenBinOp Disjunction    = emit [OR]
codeGenBinOp Equal          = emit [EQL]
codeGenBinOp NotEqual       = emit [EQL, NOT]
codeGenBinOp Less           = emit [LSS]
codeGenBinOp LessEqual      = emit [GTR, NOT]
codeGenBinOp Greater        = emit [GTR]
codeGenBinOp GreaterEqual   = emit [LSS, NOT]
