module AST.BinOp where

import Data.Hashable ( Hashable )
import Data.String (fromString)
import Data.Text
import GHC.Generics (Generic)
import Prelude
import Prettyprinter

import AST.Internal.Parsable
import Lexer.Lexer

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
  deriving (Show, Eq, Generic, Hashable)

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
