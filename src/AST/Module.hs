module AST.Module where

import Data.Hashable (Hashable)
import Data.String
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Records
import Prelude
import Prettyprinter
import Text.Megaparsec (many)

import AST.Internal.Parsable
import AST.Statement qualified as Statement
import Lexer.Lexer

data Module = Module
  { name        :: T.Text
  , definitions :: [Statement.Statement]
  }
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Module where
  pretty m = name
    where
      name = "module" <+> pretty m.name <+> "\n" <> definitions
      definitions = vsep $ map pretty m.definitions

instance Parsable Module where
  parse = do 
    n <- toplevel name
    ds <- definitions
    eof
    return $ Module n ds
    where
      name        = rword "module" *> identifier
      definitions = many $ toplevel (parse @Statement.Statement)