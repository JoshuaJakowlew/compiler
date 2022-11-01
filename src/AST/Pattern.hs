module AST.Pattern where
import qualified Data.Text as T
import Prelude
import Prettyprinter

data Pattern
  = Id T.Text
  deriving (Show, Eq)

instance Pretty Pattern where
  pretty = \case
    Id name -> pretty name