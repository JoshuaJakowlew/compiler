module SAST.Expr where

import Data.Text

import AST.Type
import AST.Literal qualified as AST
import AST.BinOp
import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Prelude
import qualified AST.Expr as AST

import Semant.Checkable (Typeable(..))

data Expr
  = Literal Type AST.Literal
  | Id      Type Text
  | BinOp   Type BinOp Expr Expr
  deriving (Show, Eq, Generic, Hashable)

instance Typeable Expr where
  typeOf (Literal t _)   = t
  typeOf (Id t _)        = t
  typeOf (BinOp t _ _ _) = t
