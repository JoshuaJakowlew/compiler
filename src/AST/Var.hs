module AST.Var where
  
import Data.Hashable (Hashable)
import Data.String (fromString)
import Data.Text
import GHC.Generics (Generic)
import Prelude
import Prettyprinter
import Text.Megaparsec ( (<|>), MonadParsec(try) )

import AST.Internal.Parsable
import AST.Type qualified as Type
import AST.Expr qualified as Expr
import Lexer.Lexer

data Var
  = Decl         Type.Type Text Expr.Expr
  | InferredDecl           Text Expr.Expr
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Var where
  pretty  = \case
    Decl t n e       -> pretty t <+> pretty n <+> "=" <+> pretty e
    InferredDecl n e -> pretty @Text "let" <+> pretty n <+> "=" <+> pretty e

instance Parsable Var where
  parse =  InferredDecl <$> try (let' *> identifier <* eq) <*> try expr
       <|> Decl         <$> type' <*> (identifier <* eq) <*> expr
    where
      let'  = rword "let"
      eq    = symbol "="
      type' = parse @Type.Type
      expr  = parse @Expr.Expr