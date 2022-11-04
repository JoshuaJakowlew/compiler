module AST.Pattern where
import Data.Hashable (Hashable)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Prelude
import Prettyprinter

import AST.Internal.Parsable
import Lexer.Lexer

data Pattern
  = Id T.Text
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Pattern where
  pretty = \case
    Id name -> pretty name

instance Parsable Pattern where
  parse = Id <$> identifier