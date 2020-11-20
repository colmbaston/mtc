module TypeCheck (typeCheck) where

import SrcPos
import AST

import           Data.Functor
import           Data.Map (Map)
import qualified Data.Map as M

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

data TypeCheckError = TypeCheckError ErrorDetails
data ErrorDetails   = DeclMult String
                    | DeclNone String
                    | Expected TypeMT TypeMT

instance Show TypeCheckError where
  showsPrec _ (TypeCheckError d) = showString "type checking error at " . shows (SrcPos 0 0) . showString ": "
                                 . case d of
                                     DeclMult v       -> showString "variable " . showString v . showString " is declared multiple times"
                                     DeclNone v       -> showString "variable " . showString v . showString " is undeclared"
                                     Expected te ta   -> showString "expected type " . shows te . showString " but the actual type is " . shows ta

-- TYPE-CHECKING MONAD

type Context     = Map String TypeMT
type TypeCheck m = StateT Context (ExceptT TypeCheckError m)

emitError :: Monad m => TypeCheckError -> TypeCheck m a
emitError = lift . throwE

lookupType :: Monad m => String -> TypeCheck m TypeMT
lookupType v = do mt <- M.lookup v <$> get
                  case mt of
                    Nothing -> emitError (TypeCheckError (DeclNone v))
                    Just td -> pure td

insertType :: Monad m => String -> TypeMT -> TypeCheck m ()
insertType v t = do ctx <- get
                    case M.insertLookupWithKey (\_ x _ -> x) v t ctx of
                      (Nothing, ctx') -> put ctx'
                      (Just  _,    _) -> emitError (TypeCheckError (DeclMult v))

matchType :: Monad m => TypeMT -> TypeMT -> TypeCheck m ()
matchType te ta = when (te /= ta) (emitError (TypeCheckError (Expected te ta)))

-- TYPE-CHECKING THE AST

typeCheck :: Program -> Either TypeCheckError ()
typeCheck p = runExcept (evalStateT (typeCheckProg p) M.empty)

typeCheckProg :: Monad m => Program -> TypeCheck m ()
typeCheckProg (Program ds c) = mapM_ typeCheckDecl ds *> typeCheckComm c

typeCheckDecl :: Monad m => Declaration -> TypeCheck m ()
typeCheckDecl (Initialise v t e) = insertType v t >> typeCheckExpr e >>= matchType t

typeCheckComm :: Monad m => Command -> TypeCheck m ()
typeCheckComm (Assign v e) = join (matchType <$> lookupType v <*> typeCheckExpr e)
typeCheckComm (If e t f)   = typeCheckExpr e >>= matchType BooleanMT >> typeCheckComm t >> typeCheckComm f
typeCheckComm (While e c)  = typeCheckExpr e >>= matchType BooleanMT >> typeCheckComm c
typeCheckComm (GetInt v)   = lookupType v    >>= matchType IntegerMT
typeCheckComm (PrintInt e) = typeCheckExpr e >>= matchType IntegerMT
typeCheckComm (Block cs)   = mapM_ typeCheckComm cs

typeCheckExpr :: Monad m => Expr -> TypeCheck m TypeMT
typeCheckExpr (LitInteger _)       = pure IntegerMT
typeCheckExpr (LitBoolean _)       = pure BooleanMT
typeCheckExpr (Variable v)         = lookupType v
typeCheckExpr (UnaryOp   op x)     = join (typeCheckUnOp   op <$> typeCheckExpr x)
typeCheckExpr (BinaryOp  op x y)   = join (typeCheckBinOp  op <$> typeCheckExpr x <*> typeCheckExpr y)
typeCheckExpr (TernaryOp op x y z) = join (typeCheckTernOp op <$> typeCheckExpr x <*> typeCheckExpr y <*> typeCheckExpr z)

typeCheckUnOp :: Monad m => UnaryOp -> TypeMT -> TypeCheck m TypeMT
typeCheckUnOp IntegerNegation tx = matchType IntegerMT tx $> IntegerMT
typeCheckUnOp BooleanNegation tx = matchType BooleanMT tx $> BooleanMT

typeCheckBinOp :: Monad m => BinaryOp -> TypeMT -> TypeMT -> TypeCheck m TypeMT
typeCheckBinOp Addition       tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> IntegerMT
typeCheckBinOp Subtraction    tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> IntegerMT
typeCheckBinOp Multiplication tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> IntegerMT
typeCheckBinOp Division       tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> IntegerMT
typeCheckBinOp Conjunction    tx ty = matchType BooleanMT tx *> matchType BooleanMT ty $> BooleanMT
typeCheckBinOp Disjunction    tx ty = matchType BooleanMT tx *> matchType BooleanMT ty $> BooleanMT
typeCheckBinOp Equal          tx ty = matchType tx ty                                  $> BooleanMT
typeCheckBinOp NotEqual       tx ty = matchType tx ty                                  $> BooleanMT
typeCheckBinOp Less           tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> BooleanMT
typeCheckBinOp Greater        tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> BooleanMT
typeCheckBinOp LessEqual      tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> BooleanMT
typeCheckBinOp GreaterEqual   tx ty = matchType IntegerMT tx *> matchType IntegerMT ty $> BooleanMT

typeCheckTernOp :: Monad m => TernaryOp -> TypeMT -> TypeMT -> TypeMT -> TypeCheck m TypeMT
typeCheckTernOp Conditional tx ty tz = matchType BooleanMT tx *> matchType ty tz $> ty
