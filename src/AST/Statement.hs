module AST.Statement where

import Data.Text qualified as T
import Prettyprinter
import Prelude

import AST.Struct qualified as Struct
import AST.Function qualified as Function
import AST.Var qualified as Var
import AST.Var (Var)

data Statement
  = Struct   Struct.Struct
  | FuncDecl Function.Decl
  | FuncBody Function.Body
  | Var      Var.Var
  deriving (Show, Eq)

instance Pretty Statement where
  pretty = \case
    Struct   s -> pretty s
    FuncDecl d -> pretty d
    FuncBody b -> pretty b
    Var      v -> pretty v