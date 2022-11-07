module Semant.Checkable where

import SAST.Type

class Typeable a where
  typeOf :: a -> Type