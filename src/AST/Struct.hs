module AST.Struct where

import Data.Text qualified as T

import AST.Member qualified as Member
import Prettyprinter
import Prelude
import Data.String
import GHC.Records

data Struct = Struct
  { name    :: T.Text
  , members :: [Member.Member]
  }
  deriving (Show, Eq)

instance Pretty Struct where
  pretty s = name <+> "{\n" <> members <> "\n}"
    where
      name = "struct" <+> pretty s.name
      members = vcat $ map (indent 2 . pretty) s.members
