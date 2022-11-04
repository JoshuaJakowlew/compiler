module AST.Internal.Parsable where

import Utils ( Parser )

class Parsable a where
  parse :: Parser a