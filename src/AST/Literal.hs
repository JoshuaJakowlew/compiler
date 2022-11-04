module AST.Literal where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Prelude
import Prettyprinter
import Text.Megaparsec ( (<|>), MonadParsec(try) )

import AST.Internal.Parsable
import Lexer.Lexer

data Literal
  = Int   Int
  | Float Double
  | Bool  Bool
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Literal where
  pretty = \case
    Int   i -> pretty i
    Float f -> pretty f
    Bool  b -> pretty b

instance Parsable Literal where
  parse =  Float <$> try float
       <|> Int   <$> try int
       <|> Bool  <$> bool