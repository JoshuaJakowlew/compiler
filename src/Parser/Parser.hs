module Parser.Parser where

import Data.Text ( Text )
import Data.Functor ( ($>), (<&>) )
import Data.Text.IO (readFile)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad.Combinators.Expr

import Lexer.Lexer
import AST.Expr  qualified as Expr
import AST.BinOp qualified as BinOp
import AST.Literal qualified as Literal
import AST.Type qualified as Type
import AST.Member qualified as Member
import AST.Struct qualified as Struct
import AST.Function qualified as Function
import AST.Pattern qualified as Pattern
import AST.Statement qualified as Statement


import Data.String (fromString)
import Prelude hiding (readFile)

parseFile :: Parser a -> String -> IO (Maybe a)
parseFile parser path = do
  text <- readFile path
  return $ parseMaybe parser text

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
term =  Expr.Literal <$> try literal
    <|> Expr.Id      <$> try identifier
    <|> parens expr

literal :: Parser Literal.Literal
literal =  Literal.Float <$> try float
       <|> Literal.Int   <$> try int
       <|> Literal.Bool  <$> bool

type' :: Parser Type.Type
type' =  rword "i32"  $> Type.Prim Type.Int32
     <|> rword "f64"  $> Type.Prim Type.Float64
     <|> rword "bool" $> Type.Prim Type.Bool
     <|> (identifier <&> Type.Struct)

member :: Parser Member.Member
member = Member.Var <$> type' <*> identifier

struct :: Parser Struct.Struct
struct = indentBlock $ do
  name <- rword "struct" *> identifier
  return $ result name member
  where
    result name = L.IndentSome Nothing (declaration name)
    declaration name = return . Struct.Struct name

funcDecl :: Parser Function.Decl
funcDecl = Function.Decl <$> name <*> args <*> ret
  where
    name = identifier <* colon
    args = many . try $ (type' <* arrow)
    ret = type'

funcBody :: Parser Function.Body
funcBody = Function.Body <$> identifier <*> patterns <*> expr
  where
    patterns = many pattern' <* symbol "="

pattern' :: Parser Pattern.Pattern
pattern' = Pattern.Id <$> identifier

statement :: Parser Statement.Statement
statement =  Statement.Struct   <$> try struct
         <|> Statement.FuncDecl <$> try funcDecl
         <|> Statement.FuncBody <$> funcBody