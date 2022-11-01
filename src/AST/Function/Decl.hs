module AST.Function.Decl where

import Data.Text qualified as T
import Prelude
import Data.String
import Prettyprinter

import AST.Type qualified as Type
import GHC.Records
import qualified AST.Type as Type

data Decl = Decl
  { name :: T.Text
  , args :: [Type.Type]
  , ret  :: Type.Type
  }
  deriving (Show, Eq)

instance Pretty Decl where
  pretty f = pretty f.name <+> ":" <+> types
    where
      types = hsep args <+> pretty f.ret
      args = map ((<+> "->") . pretty) f.args

testF = Decl
  { name = "add"
  , args = [Type.Prim Type.Int32, Type.Prim Type.Int32]
  , ret = Type.Prim Type.Int32
  }