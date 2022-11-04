module AST.BinOp where

import Data.Text
import Prettyprinter
import Data.String (fromString)
import Prelude

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  | IntPow
  | BitAnd
  | BitOr
  | BitXor
  deriving (Show, Eq)

instance Pretty BinOp where
  pretty = \case
    Add    -> "+"
    Sub    -> "-"
    Mul    -> "*"
    Div    -> "/"
    Pow    -> "^"
    IntPow -> "**"
    BitAnd -> "&"
    BitOr  -> "|"
    BitXor -> "(+)"

