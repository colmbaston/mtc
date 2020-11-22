{-# LANGUAGE ScopedTypeVariables #-}

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
                    | DeclVar  String
                    | DeclFun  String
                    | ArgsFew  String
                    | ArgsMany String
                    | Expected TypeMT TypeMT

instance Show TypeCheckError where
  showsPrec _ (TypeCheckError sp d) = showString "type-checking error at " . shows sp . showString ": "
                                    . case d of
                                        DeclNone i     -> showString "identifier " . showString i . showString " is undeclared"
                                        DeclMult i     -> showString "identifier " . showString i . showString " is declared multiple times"
                                        DeclFun  i     -> showString "identifier " . showString i . showString " is declared as a function, not as a variable"
                                        DeclVar  i     -> showString "identifier " . showString i . showString " is declared as a variable, not as a function"
                                        ArgsFew  f     -> showString "function "   . showString f . showString " was applied to too few arguments"
                                        ArgsMany f     -> showString "function "   . showString f . showString " was applied to too many arguments"
                                        Expected te ta -> showString "expected type " . shows te . showString " but the actual type is " . shows ta

-- TYPE-CHECKING MONAD

type Context      = Map String ContextEntry
data ContextEntry = VarEntry TypeMT | FunEntry [TypeMT] TypeMT
type TypeCheck m  = ReaderT SrcPos (StateT Context (ExceptT TypeCheckError m))

emitError :: Monad m => ErrorDetails -> TypeCheck m a
emitError d = do sp <- ask
                 lift (lift (throwE (TypeCheckError sp d)))

withSrcPos :: Monad m => SrcPos -> TypeCheck m a -> TypeCheck m a
withSrcPos sp = local (const sp)

lookupEntry :: Monad m => String -> TypeCheck m ContextEntry
lookupEntry i = lift get >>= maybe (emitError (DeclNone i)) pure . M.lookup i

lookupVarType :: Monad m => String -> TypeCheck m TypeMT
lookupVarType i = do e <- lookupEntry i
                     case e of
                       VarEntry t   -> pure t
                       FunEntry _ _ -> emitError (DeclFun i)

lookupFunType :: Monad m => String -> TypeCheck m ([TypeMT], TypeMT)
lookupFunType i = do e <- lookupEntry i
                     case e of
                       VarEntry _    -> emitError (DeclVar i)
                       FunEntry ps r -> pure (ps, r)

insertEntry :: Monad m => String -> ContextEntry -> TypeCheck m ()
insertEntry i e = do ctx <- lift get
                     case M.insertLookupWithKey (\_ x _ -> x) i e ctx of
                       (Nothing, ctx') -> lift (put ctx')
                       _               -> emitError (DeclMult i)

matchType :: Monad m => TypeMT -> TypeMT -> TypeCheck m ()
matchType te ta = when (te /= ta) (emitError (Expected te ta))

matchTypeExpr :: Monad m => Expr -> TypeMT -> TypeCheck m ()
matchTypeExpr e t = withSrcPos (srcPosExpr e) (typeCheckExpr e >>= matchType t)
  where
    srcPosExpr :: Expr -> SrcPos
    srcPosExpr (LitInteger  sp _)       = sp
    srcPosExpr (LitBoolean  sp _)       = sp
    srcPosExpr (Variable    sp _)       = sp
    srcPosExpr (Application sp _ _)     = sp
    srcPosExpr (UnaryOp     sp _ _)     = sp
    srcPosExpr (BinaryOp    sp _ _ _)   = sp
    srcPosExpr (TernaryOp   sp _ _ _ _) = sp

-- TYPE-CHECKING THE AST

typeCheck :: Program -> Either TypeCheckError ()
typeCheck p = runExcept (evalStateT (runReaderT (typeCheckProg p) (SrcPos 0 0)) M.empty)

typeCheckProg :: Monad m => Program -> TypeCheck m ()
typeCheckProg (Program ds c) = mapM_ typeCheckDecl ds *> typeCheckComm c

typeCheckDecl :: Monad m => Declaration -> TypeCheck m ()
typeCheckDecl (Initialise sp v    t e) = matchTypeExpr e t *> withSrcPos sp (insertEntry v (VarEntry t))
typeCheckDecl (Function   sp f ps t e) = do withSrcPos sp (insertEntry f (FunEntry (map (\(Param _ _ p) -> p) ps) t))
                                            ctx <- lift get
                                            lift (put M.empty)
                                            mapM_ typeCheckParam ps
                                            lift (modify (`M.union` ctx))
                                            matchTypeExpr e t
                                            lift (put ctx)

typeCheckParam :: Monad m => Param -> TypeCheck m ()
typeCheckParam (Param sp v t) = withSrcPos sp (insertEntry v (VarEntry t))

typeCheckComm :: Monad m => Command -> TypeCheck m ()
typeCheckComm (Assign sp v e) = withSrcPos sp (lookupVarType v) >>= matchTypeExpr e
typeCheckComm (If e t f)      = matchTypeExpr e BooleanMT       *>  typeCheckComm t *> typeCheckComm f
typeCheckComm (While e c)     = matchTypeExpr e BooleanMT       *>  typeCheckComm c
typeCheckComm (GetInt sp v)   = withSrcPos sp (lookupVarType v  >>= matchType IntegerMT)
typeCheckComm (PrintInt e)    = matchTypeExpr e IntegerMT
typeCheckComm (Block cs)      = mapM_ typeCheckComm cs

typeCheckExpr :: Monad m => Expr -> TypeCheck m TypeMT
typeCheckExpr (LitInteger  _  _)        = pure IntegerMT
typeCheckExpr (LitBoolean  _  _)        = pure BooleanMT
typeCheckExpr (Variable    sp v)        = withSrcPos sp (lookupVarType v)
typeCheckExpr (Application sp f as)     = withSrcPos sp (lookupFunType f) >>= \(ps, t) -> typeCheckArgs f t as ps
typeCheckExpr (UnaryOp     _  op x)     = typeCheckUnOp   op x
typeCheckExpr (BinaryOp    _  op x y)   = typeCheckBinOp  op x y
typeCheckExpr (TernaryOp   _  op x y z) = typeCheckTernOp op x y z

typeCheckArgs :: forall m. Monad m => String -> TypeMT -> [Expr] -> [TypeMT] -> TypeCheck m TypeMT
typeCheckArgs f t = go
  where
    go :: Monad m => [Expr] -> [TypeMT] -> TypeCheck m TypeMT
    go []     []     = pure t
    go []     _      = emitError (ArgsFew  f)
    go _      []     = emitError (ArgsMany f)
    go (a:as) (p:ps) = matchTypeExpr a p *> go as ps

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
