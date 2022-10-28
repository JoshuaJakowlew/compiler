module Parser.Parser where

import Data.Text
import Data.Functor ( ($>) )
import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Lexer.Lexer
import AST.Expr  qualified as Expr
import AST.BinOp qualified as BinOp
import AST.Literal qualified as Literal

opTable :: [[Operator Parser Expr.Expr]]
opTable = arithmOpTable

arithmOpTable :: [[Operator Parser Expr.Expr]]
arithmOpTable =
  [ [ infixR BinOp.Pow "**"
    ]
  , [ infixL BinOp.Mul "*"
    , infixL BinOp.Div "/"
    ]  
  , [ infixL BinOp.Add "+"
    , infixL BinOp.Sub "-"
    ]
  ]

infixL :: BinOp.BinOp -> Text -> Operator Parser Expr.Expr
infixL op sym = InfixL $ symbol sym $> Expr.BinOp op

infixR :: BinOp.BinOp -> Text -> Operator Parser Expr.Expr
infixR op sym = InfixR $ symbol sym $> Expr.BinOp op

expr :: Parser Expr.Expr
expr = makeExprParser term opTable

term :: Parser Expr.Expr
term =  Expr.Literal <$> literal
    <|> parens expr

literal :: Parser Literal.Literal
literal =  Literal.Float <$> try float
       <|> Literal.Int   <$> try int
       <|> Literal.Bool  <$> bool
