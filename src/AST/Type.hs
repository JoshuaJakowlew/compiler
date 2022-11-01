module AST.Type where

import Data.Text
import Prettyprinter
import Prelude
import Data.String (fromString)

data Type
  = Prim   Primitive
  | Struct Text
  deriving (Show, Eq)

data Primitive
  = Int32
  | Float64
  | Bool
  deriving (Show, Eq)

instance Pretty Primitive where
  pretty = \case
    Int32   -> pretty @Text "i32"
    Float64 -> pretty @Text "f32"
    Bool    -> pretty @Text "bool"

instance Pretty Type where
  pretty = \case
    Prim   p -> pretty p
    Struct s -> pretty s
