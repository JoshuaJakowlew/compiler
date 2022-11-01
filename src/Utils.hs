module Utils where

import Text.Pretty.Simple
import Prettyprinter ( Pretty(pretty) )
import Prettyprinter.Render.Text ( putDoc )
import GHC.Records
import Prelude
import Data.String (fromString)

pprint :: (Show a) => a -> IO ()
pprint = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg
    { outputOptionsIndentAmount = 2
    , outputOptionsCompactParens = True
    }

pprint' :: (Pretty a) => a -> IO ()
pprint' x = putDoc (pretty x) >> putStrLn ""