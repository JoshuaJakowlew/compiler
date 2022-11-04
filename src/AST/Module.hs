module AST.Module where

import Data.Text qualified as T
import Prettyprinter
import Prelude

import AST.Statement qualified as Statement
import Data.String
import GHC.Records

data Module = Module
  { name        :: T.Text
  , definitions :: [Statement.Statement]
  }
  deriving (Show, Eq)

instance Pretty Module where
  pretty m = name
    where
      name = "module" <+> pretty m.name <+> "\n" <> definitions
      definitions = vsep $ map pretty m.definitions