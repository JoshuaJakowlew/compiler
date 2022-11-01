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
import Data.String
import Prelude


type Parser = Parsec Void Text

lineComment :: Parser ()
lineComment = L.skipLineComment "//"

blockComment :: Parser ()
blockComment = L.skipBlockCommentNested "/*" "*/"

scn :: Parser ()
scn = L.space space1 lineComment blockComment

sc :: Parser ()
sc = L.space indent lineComment blockComment
  where
    indent = void $ some (char ' ' <|> char '\t')

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc

toplevel :: Parser a -> Parser a
toplevel = L.nonIndented scn

indentBlock :: Parser (L.IndentOpt Parser a b) -> Parser a
indentBlock = L.indentBlock scn

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

colon :: Parser()
colon = void $ symbol ":"

semicolon :: Parser()
semicolon = void $ char ';'

comma :: Parser ()
comma = void $ char ','

star :: Parser ()
star = void $ char '*'

arrow :: Parser ()
arrow = void $ symbol "->"

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
rword w = (lexeme . try) $ word >>= check >> pure ()
  where
    word = string w <* notFollowedBy (alphaNumChar <|> char '_')
    check = failOnNonRW (<> " is not a keyword")

identifier :: Parser Text
identifier = (lexeme . try) (packedName >>= check)
  where
    packedName = T.pack <$> name
    name = (:) <$>      (letterChar   <|> char '_')
               <*> many (alphaNumChar <|> char '_')
    check = failOnRW \x -> "keyword " <> x <> " cannot be a keyword"

rws :: [Text]
rws =
  [ "true"
  , "false"
  , "i32"
  , "f64"
  , "bool"
  , "struct"
  ]

isRW :: Text -> Bool
isRW = (`elem` rws)

failOn :: (MonadFail m) => (Text -> Bool) -> (Text -> Text) -> Text -> m Text
failOn p err x | p x       = pure x
               | otherwise = fail . T.unpack $ err x

failOnRW :: (MonadFail m) => (Text -> Text) -> Text -> m Text
failOnRW = failOn $ not . isRW

failOnNonRW :: (MonadFail m) => (Text -> Text) -> Text -> m Text
failOnNonRW = failOn isRW
