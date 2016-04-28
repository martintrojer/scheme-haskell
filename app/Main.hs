module Main where

import System.IO
import BuiltIns
import Interpreter
import Parser
import Types

repl :: Env -> IO ()
repl env = do
  putStr "> "
  hFlush stdout
  input <- getLine
  case parseExpr input of
    Just expr -> do
      let (res, newEnv) = eval env expr
      print res
      repl newEnv
    Nothing -> putStrLn "parse error"

main :: IO ()
main = do
  putStrLn "Welcome to mtscheme v0.1"
  repl baseEnv
