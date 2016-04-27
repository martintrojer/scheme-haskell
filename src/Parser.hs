module Parser (parseValue, parseExpr) where

import Data.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text

import Types

num :: Parser Double
num = read <$> many1 (oneOf "0123456789.") <* spaces

name :: Parser String
name = many1 (oneOf "abcdefghijklmnopqrstuvwxyz+-*/=<>") <* spaces

str :: Parser Value
str = do
  _ <- char '"'
  val <- name
  _ <- char '"'
  spaces
  return $ VStr val

true :: Parser Value
true = do
  _ <- string "true"
  spaces
  return $ VBool True

false :: Parser Value
false = do
  _ <- string "false"
  spaces
  return $ VBool False

value :: Parser Value
value =
  VNum <$> num <|> str <|> try true <|> try false

comb :: Parser Expr
comb = do
  _ <- char '('
  spaces
  exprs <- many1 expr
  spaces
  _ <- char ')'
  spaces
  return $ EComb exprs

expr :: Parser Expr
expr =
  EValue <$> value
  <|> ESymbol <$> name
  <|> comb

parseValue :: String -> Maybe Value
parseValue s =
  case parse value "" $ pack s of
    Left err -> error $ show err
    Right val -> Just val

parseExpr :: String -> Maybe Expr
parseExpr s =
  case parse expr "" $ pack s of
    Left err -> error $ show err
    Right val -> Just val
