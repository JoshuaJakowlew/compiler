module SAST.Var where

import Data.Text
import GHC.Records (HasField(..))
import Prelude

import SAST.Type ( Type )
import SAST.Expr ( Expr )

import Semant.Checkable (Typeable(..))

data Var = Var
  { type' :: Type
  , name  :: Text
  , value :: Expr
  } deriving (Show, Eq)

instance Typeable Var where
  typeOf v = v.type'