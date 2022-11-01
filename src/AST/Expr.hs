module AST.Expr where

import Prettyprinter
import Data.Text ( Text )

import AST.Literal qualified as Literal
import AST.BinOp   qualified as BinOp
import Prelude
import Data.String (fromString)

data Expr
  = Literal Literal.Literal
  | Id Text
  | BinOp BinOp.BinOp Expr Expr
  deriving (Show, Eq)

instance Pretty Expr where
  pretty = \case
    Literal l        -> pretty l
    Id t             -> pretty t
    BinOp op lhs rhs -> "(" <> hsep [pretty lhs, pretty op, pretty rhs] <> ")"