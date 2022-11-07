module Utils where

import Text.Pretty.Simple
import Prettyprinter ( Pretty(pretty) )
import Prettyprinter.Render.Text ( putDoc )
import GHC.Records
import Prelude
import Data.String (fromString)
import Text.Megaparsec
import Data.Void (Void)
import Data.Text
import Data.Bool

type Parser = Parsec Void Text

pprint :: (Show a) => a -> IO ()
pprint = pPrintOpt CheckColorTty defaultOutputOptionsDarkBg
    { outputOptionsIndentAmount = 2
    , outputOptionsCompactParens = True
    }

pprint' :: (Pretty a) => a -> IO ()
pprint' x = putDoc (pretty x) >> putStrLn ""

ifThenElse :: Bool -> a -> a -> a
ifThenElse c x y = Data.Bool.bool y x c