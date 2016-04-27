module Main where

import ParserTest
import BuiltInsTest

main :: IO ()
main = do
  testParser
  testBuiltIns
