module SAST.Type 
  ( Type(..)
  , Primitive(..)
  ) where

import Prelude
import Data.Text qualified as T
import Prettyprinter
import GHC.Generics (Generic)
import Data.Hashable (Hashable)

import AST.Type