module Semant.Semant where

import Control.Lens hiding (op)
import Control.Lens.Extras (is)
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.HashMap.Strict qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Prelude

import Semant.Checkable

import qualified SAST.Module as SAST
import qualified SAST.Expr as SAST
import qualified SAST.Type as SAST
import qualified SAST.Type
import qualified SAST.Literal as SAST
import qualified SAST.Literal
import qualified SAST.Var as SAST

import qualified AST.Module as AST
import qualified AST.Statement as AST
import qualified AST.Var as AST
import qualified AST.Expr as AST
import qualified AST.BinOp as AST
import AST.Literal qualified as AST

import Utils

import GHC.Records
import Data.String
import GHC.IO (unsafePerformIO)
import GHC.Generics (Generic)
import Data.Default
import Data.Maybe


type Vars = M.HashMap T.Text SAST.Type
-- type Functions = M.HashMap T.Text Type.Type
data SemantError
  = UnknownError
  | NotImplementedError
  | MultipleVarDefinitionsError AST.Var
  | UnknownIdentifier T.Text AST.Expr
  | TypeMismatch SAST.Type SAST.Type
  deriving (Show)

data Globals = Globals
  { _vars :: Vars
  } deriving (Eq, Show)

instance Default Globals where
  def = Globals M.empty

data Env = Env
  { _moduleName :: T.Text
  , _glob       :: Globals
  } deriving (Eq, Show)

instance Default Env where
  def = Env "" def

makeFieldsNoPrefix ''Globals
makeFieldsNoPrefix ''Env

type Semant = ExceptT SemantError (State Env)

checkModule :: AST.Module -> Semant ()
checkModule m = do
  moduleName .= m.name
  forM_ m.definitions checkDefinitions

checkDefinitions :: AST.Statement -> Semant ()
checkDefinitions ds = do
  case ds of
    (AST.Var v) -> void $ checkVar v
    _ -> pure () -- throwError NotImplementedError

-- FIXME: Currently every var is global
checkVar :: AST.Var -> Semant SAST.Var
checkVar v = do
  vs <- use $ glob . vars
  when (isJust (vs M.!? v.name)) $
    throwError $ MultipleVarDefinitionsError v

  e <- checkExpr v.value
  let inferredType = typeOf e

  when ((AST._Decl `is` v) && (v.type' /= inferredType)) $
    throwError $ TypeMismatch v.type' inferredType
  
  let checkedVar = SAST.Var inferredType v.name e
  glob . vars .= checkedVar `insertVar` vs
  
  pure checkedVar
  where
    insertVar v' = M.insert v'.name v'.type'

checkExpr :: AST.Expr -> Semant SAST.Expr
checkExpr e = do
  case e of
    (AST.Literal (AST.Int i)) -> pure $ SAST.Literal (SAST.Prim SAST.Int32) (SAST.Int i)
    (AST.Literal (AST.Float f)) -> pure $ SAST.Literal (SAST.Prim SAST.Float64) (SAST.Float f)
    (AST.Literal (AST.Bool b)) -> pure $ SAST.Literal (SAST.Prim SAST.Type.Bool) (SAST.Literal.Bool b)
    (AST.Id n) -> do
      vs <- use $ glob . vars
      let t = vs M.!? n
      when (isNothing t) $
        throwError $ UnknownIdentifier n e
      
      pure $ SAST.Id (fromJust t) n
    (AST.BinOp op l r) -> checkBinOpTypes op l r

checkBinOpTypes :: AST.BinOp -> AST.Expr -> AST.Expr -> Semant SAST.Expr
checkBinOpTypes op' l' r' = do
  l <- checkExpr l'
  r <- checkExpr r'
  check op' l r
  where
    check op l r | op == AST.IntPow = checkIntPow l r
                 | op `elem` [AST.BitAnd, AST.BitOr, AST.BitXor] = checkBitwise op l r
                 | otherwise = checkGeneric op l r

checkIntPow :: SAST.Expr -> SAST.Expr -> Semant SAST.Expr
checkIntPow l r = do
  let (tl, tr) = (typeOf l, typeOf r)
  -- Check for right arg of IntPow
  -- a ** b -> typeOf b == Int32
  when (tr /= SAST.Prim SAST.Type.Int32) $
    throwError $ TypeMismatch (typeOf l) (typeOf r)
  pure $ SAST.BinOp tl AST.IntPow l r

checkBitwise :: AST.BinOp -> SAST.Expr -> SAST.Expr -> Semant SAST.Expr
checkBitwise op l r = do
  let (tl, tr) = (typeOf l, typeOf r)
  let i32 = SAST.Prim SAST.Type.Int32
  -- Check for bitwise operations
  -- Bitwise ops permitted on integer types only
  when ((tl, tr) /= (i32, i32)) $
    throwError $ TypeMismatch tl tr
  pure $ SAST.BinOp tl op l r

checkGeneric :: AST.BinOp -> SAST.Expr -> SAST.Expr -> Semant SAST.Expr
checkGeneric op l r = do
  let (tl, tr) = (typeOf l, typeOf r)
  -- Generic operators must have operands of equal types
  when (tl /= tr) $
    throwError $ TypeMismatch tl tr
  pure $ SAST.BinOp tl op l r