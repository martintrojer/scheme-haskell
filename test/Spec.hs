module Main where

import ParserTest
import EnvTest
import BuiltInsTest

main :: IO ()
main = do
  testParser
  testEnv
  testBuiltIns
