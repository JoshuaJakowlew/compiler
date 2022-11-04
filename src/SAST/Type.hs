module SAST.Type 
  ( Type(..)
  , Primitive(..)
  ) where

import Prelude
import Data.Text qualified as T
import Prettyprinter

import AST.Type