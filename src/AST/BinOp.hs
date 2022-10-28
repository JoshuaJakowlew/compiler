module AST.BinOp where

import Prettyprinter

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Pow
  deriving (Show, Eq)

instance Pretty BinOp where
  pretty = \case
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Pow -> "**"
