module AST.Literal where

import Prettyprinter

data Literal
  = Int Int
  | Float Double
  | Bool Bool
  deriving (Show, Eq)

instance Pretty Literal where
  pretty = \case
    Int   i -> pretty i
    Float f -> pretty f
    Bool  b -> pretty b