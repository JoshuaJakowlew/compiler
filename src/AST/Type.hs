module AST.Type where

import Control.Applicative ((<|>))
import Data.Functor (($>))
import Data.Hashable (Hashable)
import Data.String (fromString)
import Data.Text ( Text )
import GHC.Generics (Generic)
import Prettyprinter ( Pretty(pretty) )
import Prelude

import AST.Internal.Parsable ( Parsable(..) )
import Lexer.Lexer ( rword, identifier )

data Type
  = Prim   Primitive
  | Struct Text
  deriving (Show, Eq, Generic, Hashable)

data Primitive
  = Int32
  | Float64
  | Bool
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Primitive where
  pretty = \case
    Int32   -> pretty @Text "i32"
    Float64 -> pretty @Text "f32"
    Bool    -> pretty @Text "bool"

instance Pretty Type where
  pretty = \case
    Prim   p -> pretty p
    Struct s -> pretty s

instance Parsable Primitive where
  parse =  rword "i32"  $> Int32
       <|> rword "f64"  $> Float64
       <|> rword "bool" $> Bool

instance Parsable Type where
  parse =  Prim   <$> parse @Primitive
       <|> Struct <$> identifier