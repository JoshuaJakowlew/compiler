module AST.Statement where

import Data.Hashable (Hashable)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prelude
import Prettyprinter
import Text.Megaparsec ( (<|>), try )

import AST.Internal.Parsable
import AST.Function qualified as Function
import AST.Struct qualified as Struct
import AST.Var qualified as Var
import Lexer.Lexer

data Statement
  = Struct   Struct.Struct
  | FuncDecl Function.Decl
  | FuncBody Function.Body
  | Var      Var.Var
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Statement where
  pretty = \case
    Struct   s -> pretty s
    FuncDecl d -> pretty d
    FuncBody b -> pretty b
    Var      v -> pretty v

instance Parsable Statement where
  parse =  Struct   <$> try (parse @Struct.Struct)
       <|> FuncDecl <$> try (parse @Function.Decl)
       <|> FuncBody <$> try (parse @Function.Body)
       <|> Var      <$> (parse @Var.Var)