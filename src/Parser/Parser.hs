module Parser.Parser where

import Data.Text ( Text )
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import Text.Megaparsec ( parseMaybe, parseTest )

import AST.Internal.Parsable ( Parsable(..) )

parseFileTest :: forall a . (Show a, Parsable a) => String -> IO ()
parseFileTest path = readFile path >>= parseTest (parse @a)

parseFileMaybe :: forall a . (Show a, Parsable a) => String -> IO (Maybe a)
parseFileMaybe path = do
  text <- readFile path
  return $ parseMaybe (parse @a) text
