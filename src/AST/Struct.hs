module AST.Struct where

import Data.Hashable (Hashable)
import Data.String ( IsString(fromString) )
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Records ( HasField(getField) )
import Prelude
import Prettyprinter
import Text.Megaparsec.Char.Lexer qualified as L

import AST.Internal.Parsable ( Parsable(..) )
import AST.Type qualified as Type
import Lexer.Lexer

data Struct = Struct
  { name    :: T.Text
  , members :: [Member]
  }
  deriving (Show, Eq, Generic, Hashable)

data Member
  = Var Type.Type T.Text
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Struct where
  pretty s = name <+> "{\n" <> members <> "\n}"
    where
      name = "struct" <+> pretty s.name
      members = vcat $ map (indent 2 . pretty) s.members

instance Pretty Member where
  pretty = \case
    Var t n -> hsep [pretty t, pretty n]

instance Parsable Struct where
  parse = indentBlock $ do
    name <- rword "struct" *> identifier
    return $ result name (parse @Member)
    where
      result name = L.IndentSome Nothing (declaration name)
      declaration name = return . Struct name

instance Parsable Member where
  parse = Var <$> parse @Type.Type
              <*> identifier