module AST.Expr where

import Data.Functor (($>))
import Data.Hashable (Hashable)
import Control.Monad.Combinators.Expr
  ( makeExprParser, Operator(InfixR, InfixL) )
import Data.String (fromString)
import Data.Text ( Text )
import GHC.Generics (Generic)
import Prelude
import Prettyprinter ( hsep, Pretty(pretty) )
import Text.Megaparsec ( (<|>), MonadParsec(try) )

import AST.Internal.Parsable
import AST.BinOp   qualified as BinOp
import AST.Literal qualified as Literal
import Lexer.Lexer
import Utils

data Expr
  = Literal Literal.Literal
  | Id Text
  | BinOp BinOp.BinOp Expr Expr
  deriving (Show, Eq, Generic, Hashable)

instance Pretty Expr where
  pretty = \case
    Literal l        -> pretty l
    Id t             -> pretty t
    BinOp op lhs rhs -> "(" <> hsep [pretty lhs, pretty op, pretty rhs] <> ")"

instance Parsable Expr where
  parse = makeExprParser term opTable

term :: Parser Expr
term =  Literal <$> try (parse @Literal.Literal)
    <|> Id      <$> try identifier
    <|> parens (parse @Expr)

opTable :: [[Operator Parser Expr]]
opTable =
  [ [ infixR BinOp.Pow    "^"
    , infixR BinOp.IntPow "**"
    ]
  , [ infixL BinOp.Mul "*"
    , infixL BinOp.Div "/"
    ]  
  , [ infixL BinOp.Add "+"
    , infixL BinOp.Sub "-"
    ]
  , [ infixL BinOp.BitAnd "&"
    ]
  , [ infixL BinOp.BitOr "|"
    ]
  , [ infixL BinOp.BitXor "(+)"
    ]
  ]

infixL :: BinOp.BinOp -> Text -> Operator Parser Expr
infixL op sym = InfixL $ symbol sym $> BinOp op

infixR :: BinOp.BinOp -> Text -> Operator Parser Expr
infixR op sym = InfixR $ symbol sym $> BinOp op
