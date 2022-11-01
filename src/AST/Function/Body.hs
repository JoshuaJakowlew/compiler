module AST.Function.Body where

import Data.Text qualified as T
import Prelude
import Data.String
import Prettyprinter
import GHC.Records

import AST.Expr qualified as Expr
import AST.Pattern qualified as Pattern

-- TODO: Add Block for multiple statements
data Body = Body
  { name :: T.Text
  , args :: [Pattern.Pattern]
  , expr :: Expr.Expr
  }
  deriving (Show, Eq)

instance Pretty Body where
  pretty b = name <+> args <+> "=" <+> expr
    where
      name = pretty b.name
      args = hsep . map pretty $ b.args
      expr = pretty b.expr