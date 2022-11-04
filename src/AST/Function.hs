module AST.Function where

import Data.Hashable (Hashable)
import Data.String
import Data.Text qualified as T
import GHC.Generics (Generic)
import GHC.Records
import Prelude
import Prettyprinter ( (<+>), hsep, Pretty(pretty) )
import Text.Megaparsec hiding (parse)

import AST.Internal.Parsable
import AST.Expr qualified as Expr
import AST.Type qualified as Type
import AST.Pattern qualified as Pattern
import Lexer.Lexer


data Decl = Decl
  { name :: T.Text
  , args :: [Type.Type]
  , ret  :: Type.Type
  }
  deriving (Show, Eq, Generic, Hashable)

-- TODO: Add Block for multiple statements
data Body = Body
  { name :: T.Text
  , args :: [Pattern.Pattern]
  , expr :: Expr.Expr
  }
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Decl where
  pretty f = pretty f.name <+> ":" <+> types
    where
      types = hsep args <+> pretty f.ret
      args = map ((<+> "->") . pretty) f.args

instance Pretty Body where
  pretty b = name <+> args <+> "=" <+> expr
    where
      name = pretty b.name
      args = hsep . map pretty $ b.args
      expr = pretty b.expr

instance Parsable Decl where
  parse = Decl <$> name <*> args <*> type'
    where
      name = identifier <* colon
      args = many . try $ (type' <* arrow)
      type' = parse @Type.Type

instance Parsable Body where
  parse = Body <$> identifier <*> patterns <*> (parse @Expr.Expr)
    where
      patterns = many (parse @Pattern.Pattern) <* symbol "="