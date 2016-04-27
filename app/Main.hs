module Main where

import BuiltIns
import Interpreter
import Parser
import Types

evalStr :: String -> Expr
evalStr str =
  case parseExpr str of
    Just expr -> fst . eval baseEnv $ expr
    Nothing -> error "Parse error"

main :: IO ()
main =
  putStrLn . show $ evalStr "(+ 1 1)"
