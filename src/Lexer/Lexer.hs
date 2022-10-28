module Lexer.Lexer where

import Data.Functor ( ($>) )
import           Data.Void
import           Data.Char
import           Text.Megaparsec hiding (between)
import           Text.Megaparsec qualified as M
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer    as L
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Monad                  ( void )
import           Data.String.Conversions
import Data.Functor.Identity (Identity)

type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

sc :: Parser ()
sc = L.space space1 lineComment blockComment

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

between :: Text -> Text -> Parser a -> Parser a
between open close = M.between (symbol open) (symbol close)

parens :: Parser a -> Parser a
parens = between "(" ")"

braces :: Parser a -> Parser a
braces = between "{" "}"

dquotes :: Parser a -> Parser a
dquotes = between "\"" "\""

squotes :: Parser a -> Parser a
squotes = between "'" "'"

semicolon :: Parser()
semicolon = void $ char ';'

comma :: Parser ()
comma = void $ char ','

star :: Parser ()
star = void $ char '*'

noSpace :: Parser ()
noSpace = pure ()

signed :: (Num a) => Parser a -> Parser a
signed = L.signed noSpace

decimal :: (Integral a) => Parser a
decimal = lexeme L.decimal

hexadecimal :: (Integral a) => Parser a
hexadecimal = lexeme (char '0' *> char' 'x' *> L.hexadecimal)

binary :: (Integral a) => Parser a
binary = lexeme (char '0' *> char' 'b' *> L.binary)

int :: Parser Int
int =  try binary
   <|> try hexadecimal
   <|> decimal

float :: (RealFloat a) => Parser a
float = lexeme L.float

bool :: Parser Bool
bool =  rword "true"  $> True
    <|> rword "false" $> False

rword :: Text -> Parser ()
rword w = try $ word >>= check >> pure ()
  where
    word = string w <* notFollowedBy alphaNumChar
    check x | x `elem` rws = pure x
            | otherwise    = fail . T.unpack $ w <> " is not a keyword"

rws :: [Text]
rws =
  [ "true"
  , "false"
  ]