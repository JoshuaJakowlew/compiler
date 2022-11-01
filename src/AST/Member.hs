module AST.Member where

import Data.Text
import Prettyprinter

import AST.Type qualified as Type
import Prelude

data Member
  = Var Type.Type Text
  deriving (Show, Eq)

instance Pretty Member where
  pretty = \case
    Var t n -> hsep [pretty t, pretty n]