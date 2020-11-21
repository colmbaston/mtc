module TypeCheck (typeCheck) where

import SrcPos
import AST

import           Data.Functor
import           Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

data TypeCheckError = TypeCheckError SrcPos ErrorDetails
data ErrorDetails   = DeclNone String
                    | DeclMult String
                    | Expected TypeMT TypeMT

instance Show TypeCheckError where
  showsPrec _ (TypeCheckError sp d) = showString "type-checking error at " . shows sp . showString ": "
                                    . case d of
                                        DeclNone v     -> showString "variable " . showString v . showString " is undeclared"
                                        DeclMult v     -> showString "variable " . showString v . showString " is declared multiple times"
                                        Expected te ta -> showString "expected type " . shows te . showString " but the actual type is " . shows ta

-- TYPE-CHECKING MONAD

type Context     = Map String TypeMT
type TypeCheck m = ReaderT SrcPos (StateT Context (ExceptT TypeCheckError m))

emitError :: Monad m => ErrorDetails -> TypeCheck m a
emitError d = do sp <- ask
                 lift (lift (throwE (TypeCheckError sp d)))

withSrcPos :: Monad m => SrcPos -> TypeCheck m a -> TypeCheck m a
withSrcPos sp = local (const sp)

lookupType :: Monad m => String -> TypeCheck m TypeMT
lookupType v = maybe (emitError (DeclNone v)) pure . M.lookup v =<< lift get

insertType :: Monad m => String -> TypeMT -> TypeCheck m ()
insertType v t = do ctx <- lift get
                    case M.insertLookupWithKey (\_ x _ -> x) v t ctx of
                      (Nothing, ctx') -> lift (put ctx')
                      _               -> emitError (DeclMult v)

matchType :: Monad m => TypeMT -> TypeMT -> TypeCheck m ()
matchType te ta = when (te /= ta) (emitError (Expected te ta))

matchTypeExpr :: Monad m => Expr -> TypeMT -> TypeCheck m ()
matchTypeExpr e t = withSrcPos (srcPosExpr e) (typeCheckExpr e >>= matchType t)
  where
    srcPosExpr :: Expr -> SrcPos
    srcPosExpr (LitInteger sp _)       = sp
    srcPosExpr (LitBoolean sp _)       = sp
    srcPosExpr (Variable   sp _)       = sp
    srcPosExpr (UnaryOp    sp _ _)     = sp
    srcPosExpr (BinaryOp   sp _ _ _)   = sp
    srcPosExpr (TernaryOp  sp _ _ _ _) = sp

-- TYPE-CHECKING THE AST

typeCheck :: Program -> Either TypeCheckError ()
typeCheck p = runExcept (evalStateT (runReaderT (typeCheckProg p) (SrcPos 0 0)) M.empty)

typeCheckProg :: Monad m => Program -> TypeCheck m ()
typeCheckProg (Program ds c) = mapM_ typeCheckDecl ds *> typeCheckComm c

typeCheckDecl :: Monad m => Declaration -> TypeCheck m ()
typeCheckDecl (Initialise sp v t e) = matchTypeExpr e t *> withSrcPos sp (insertType v t)

typeCheckComm :: Monad m => Command -> TypeCheck m ()
typeCheckComm (Assign sp v e) = withSrcPos sp (lookupType v) >>= matchTypeExpr e
typeCheckComm (If e t f)      = matchTypeExpr e BooleanMT    *>  typeCheckComm t *> typeCheckComm f
typeCheckComm (While e c)     = matchTypeExpr e BooleanMT    *>  typeCheckComm c
typeCheckComm (GetInt sp v)   = withSrcPos sp (lookupType v  >>= matchType IntegerMT)
typeCheckComm (PrintInt e)    = matchTypeExpr e IntegerMT
typeCheckComm (Block cs)      = mapM_ typeCheckComm cs

typeCheckExpr :: Monad m => Expr -> TypeCheck m TypeMT
typeCheckExpr (LitInteger _  _)        = pure IntegerMT
typeCheckExpr (LitBoolean _  _)        = pure BooleanMT
typeCheckExpr (Variable   sp v)        = withSrcPos sp (lookupType v)
typeCheckExpr (UnaryOp    _  op x)     = typeCheckUnOp   op x
typeCheckExpr (BinaryOp   _  op x y)   = typeCheckBinOp  op x y
typeCheckExpr (TernaryOp  _  op x y z) = typeCheckTernOp op x y z

typeCheckUnOp :: Monad m => UnaryOp -> Expr -> TypeCheck m TypeMT
typeCheckUnOp IntegerNegation x = matchTypeExpr x IntegerMT $> IntegerMT
typeCheckUnOp BooleanNegation x = matchTypeExpr x BooleanMT $> BooleanMT

typeCheckBinOp :: Monad m => BinaryOp -> Expr -> Expr -> TypeCheck m TypeMT
typeCheckBinOp Addition       x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> IntegerMT
typeCheckBinOp Subtraction    x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> IntegerMT
typeCheckBinOp Multiplication x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> IntegerMT
typeCheckBinOp Division       x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> IntegerMT
typeCheckBinOp Conjunction    x y =  matchTypeExpr x BooleanMT *>  matchTypeExpr y BooleanMT $> BooleanMT
typeCheckBinOp Disjunction    x y =  matchTypeExpr x BooleanMT *>  matchTypeExpr y BooleanMT $> BooleanMT
typeCheckBinOp Equal          x y = (typeCheckExpr x           >>= matchTypeExpr y)          $> BooleanMT
typeCheckBinOp NotEqual       x y = (typeCheckExpr x           >>= matchTypeExpr y)          $> BooleanMT
typeCheckBinOp Less           x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> BooleanMT
typeCheckBinOp Greater        x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> BooleanMT
typeCheckBinOp LessEqual      x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> BooleanMT
typeCheckBinOp GreaterEqual   x y =  matchTypeExpr x IntegerMT *>  matchTypeExpr y IntegerMT $> BooleanMT

typeCheckTernOp :: Monad m => TernaryOp -> Expr -> Expr -> Expr -> TypeCheck m TypeMT
typeCheckTernOp Conditional x y z = matchTypeExpr x BooleanMT *> do ty <- typeCheckExpr y
                                                                    matchTypeExpr z ty $> ty
