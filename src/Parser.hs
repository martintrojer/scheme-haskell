{-# LANGUAGE OverloadedStrings #-}

module Parser (Expr(..), Value(..), parseValue, parseExpr) where

import Data.Text
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.Prim
import Text.Parsec.Text

data Value
  = VNum Double
  | VStr String
  deriving (Show, Eq)

data Expr
  = EValue Value
  | ESymbol String
  | EComb [Expr]
  deriving (Show, Eq)

num :: Parser Double
num = read <$> many1 (oneOf "0123456789.") <* spaces

name :: Parser String
name = many1 (oneOf "abcdefghijklmnopqrstuvwxyz") <* spaces

-- a = parseTest num $ pack "12"
-- a' = parseTest name $ pack "foo"

str :: Parser Value
str = do
  _ <- char '"'
  val <- name
  _ <- char '"'
  spaces
  return $ VStr val

value :: Parser Value
value =
  VNum <$> num <|> str

-- b = parseTest value $ pack "42.42"
-- b'' = parseTest value $ pack "\"barf\""
-- b''' = parseTest value $ pack "barf"

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

-- c = parseTest expr $ pack "42"
-- c' = parseTest expr $ pack "\"bar\""
-- c'' = parseTest expr $ pack "foo"
-- c''' = parseTest expr $ pack "(1 two \"three\" (1))"

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
