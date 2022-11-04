module AST.Var where
  
import Data.Text qualified as T
import Prettyprinter
import Prelude

import AST.Type qualified as Type
import AST.Expr qualified as Expr
import Data.String (fromString)

data Var
  = Decl         Type.Type T.Text Expr.Expr
  | InferredDecl           T.Text Expr.Expr
  deriving (Show, Eq)

instance Pretty Var where
  pretty  = \case
    Decl t n e       -> pretty t <+> pretty n <+> "=" <+> pretty e
    InferredDecl n e -> pretty @T.Text "let" <+> pretty n <+> "=" <+> pretty e