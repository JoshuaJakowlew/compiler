module AST.Expr where

import Prettyprinter

import AST.Literal qualified as Literal
import AST.BinOp   qualified as BinOp

data Expr
  = Literal Literal.Literal
  | BinOp BinOp.BinOp Expr Expr
  deriving (Show, Eq)

instance Pretty Expr where
  pretty = \case
    Literal l        -> pretty l
    BinOp op lhs rhs -> "(" <> hsep [pretty lhs, pretty op, pretty rhs] <> ")"